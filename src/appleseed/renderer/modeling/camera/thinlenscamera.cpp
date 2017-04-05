
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header.
#include "thinlenscamera.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/dual.h"
#include "foundation/math/intersection/planesegment.h"
#include "foundation/math/matrix.h"
#include "foundation/math/sampling/imageimportancesampler.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // A thin lens camera with active autofocus.
    //
    // References:
    //
    //   http://en.wikipedia.org/wiki/Thin_lens
    //   http://en.wikipedia.org/wiki/Focal_length
    //   http://en.wikipedia.org/wiki/F-number
    //   http://en.wikipedia.org/wiki/Autofocus
    //   http://en.wikipedia.org/wiki/Diaphragm_(optics)
    //
    // Geometry of the camera:
    //
    //                                          Y+
    //
    //       Film Plane                         ^         Focal Plane
    //   Z = m_focal_length                     |    Z = -m_focal_distance
    //                                          |
    //            +-----------------------------+              +
    //            |                             |              |
    //            |                             |              |
    //            |                             -              |
    //            |                             |              |
    //            |                             |              |
    //     Z+  <--+-----------------------------+              |
    //            |                             | O            |
    //            |                             |              |
    //            |                             -              |
    //            |                             |              |
    //            |                             |              |
    //            +-----------------------------+              |
    //                                          |              +
    //                                          |
    //                                          +
    //
    //                                     Lens Plane
    //                                        Z = 0
    //

    typedef ImageImportanceSampler<Vector2d, float> ImageImportanceSamplerType;

    class ImageSampler
    {
      public:
        explicit ImageSampler(
            TextureCache&           texture_cache,
            const TextureSource*    texture_source,
            const size_t            width,
            const size_t            height)
          : m_texture_cache(texture_cache)
          , m_texture_source(texture_source)
          , m_width(width)
          , m_height(height)
          , m_range(sqrt(1.0 + static_cast<double>(m_height * m_height) / (m_width * m_width)))
        {
        }

        void sample(const size_t x, const size_t y, Vector2d& payload, float& importance) const
        {
            payload = Vector2d(
                (2.0 * x + 1.0 - m_width) / (m_width - 1.0),
                (2.0 * y + 1.0 - m_height) / (m_height - 1.0));

            if (m_height != m_width)
                payload.y *= static_cast<double>(m_height) / m_width;

            payload /= m_range;     // scale to fit in a unit disk

            const Vector2f uv(
                x / (m_width - 1.0f),
                y / (m_height - 1.0f));

            Color3f color;
            m_texture_source->evaluate(m_texture_cache, uv, color);

            importance = luminance(color);
        }

      private:
        TextureCache&               m_texture_cache;
        const TextureSource*        m_texture_source;
        const size_t                m_width;
        const size_t                m_height;
        const double                m_range;
    };

    const char* Model = "thinlens_camera";

    class ThinLensCamera
      : public Camera
    {
      public:
        ThinLensCamera(
            const char*             name,
            const ParamArray&       params)
          : Camera(name, params)
        {
            m_inputs.declare("diaphragm_map", InputFormatSpectralReflectance, "");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual bool on_render_begin(
            const Project&          project,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!Camera::on_render_begin(project, abort_switch))
                return false;

            // Extract the film dimensions from the camera parameters.
            m_film_dimensions = extract_film_dimensions();

            // Extract the focal length from the camera parameters.
            m_focal_length = extract_focal_length(m_film_dimensions[0]);

            // Extract the focal distance from the camera parameters.
            extract_focal_distance(
                m_autofocus_enabled,
                m_autofocus_target,
                m_focal_distance);

            // Extract the diaphragm configuration from the camera parameters.
            m_diaphragm_map_bound = build_diaphragm_importance_sampler(*project.get_scene());
            extract_diaphragm_blade_count();
            extract_diaphragm_tilt_angle();

            // Extract the abscissa of the near plane from the camera parameters.
            m_near_z = extract_near_z();

            // Precompute reciprocals of film dimensions.
            m_rcp_film_width = 1.0 / m_film_dimensions[0];
            m_rcp_film_height = 1.0 / m_film_dimensions[1];

            // Precompute pixel area.
            const size_t pixel_count = project.get_frame()->image().properties().m_pixel_count;
            m_pixel_area = m_film_dimensions[0] * m_film_dimensions[1] / pixel_count;

            // Precompute lens radius.
            m_lens_radius = 0.5 * m_focal_length / extract_f_stop();

            // Build the diaphragm polygon.
            if (!m_diaphragm_map_bound && m_diaphragm_blade_count > 0)
            {
                m_diaphragm_vertices.resize(m_diaphragm_blade_count);
                build_regular_polygon(
                    m_diaphragm_blade_count,
                    m_diaphragm_tilt_angle,
                    &m_diaphragm_vertices.front());
            }

            print_settings();

            return true;
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!Camera::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            // Perform autofocus, if enabled.
            if (m_autofocus_enabled)
            {
                TextureStore texture_store(*project.get_scene());
                TextureCache texture_cache(texture_store);
                Intersector intersector(project.get_trace_context(), texture_cache);
                m_focal_distance = get_autofocus_focal_distance(intersector);
            }

            // Compute ratios between focal distance and focal length.
            m_focal_ratio = m_focal_distance / m_focal_length;
            m_rcp_focal_ratio = m_focal_length / m_focal_distance;

            return true;
        }

        virtual void spawn_ray(
            SamplingContext&        sampling_context,
            const Dual2d&           ndc,
            ShadingRay&             ray) const APPLESEED_OVERRIDE
        {
            //
            // The algorithm is as follow:
            //
            //   1. Compute the camera space coordinates of the film point.
            //   2. Trace a line starting at the film point and passing
            //      through the center of the lens (the origin in camera space).
            //   3. Compute the intersection between this line and the plane of
            //      focus at Z = -m_focal_distance. Call this the focal point.
            //   4. Choose a point at random on the lens. Call this the lens point.
            //   5. The final ray originates at the lens point and passes through
            //      the focal point.
            //

            // Initialize the ray.
            initialize_ray(sampling_context, ray);

            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform =
                m_transform_sequence.evaluate(ray.m_time.m_absolute, scratch);

            // Compute lens point in world space.
            const Vector3d lens_point = transform.point_to_parent(sample_lens(sampling_context));

            // Compute ray origin and direction.
            ray.m_org = lens_point;
            ray.m_dir = compute_ray_direction(ndc.get_value(), lens_point, transform);

            // Compute ray derivatives.
            if (ndc.has_derivatives())
            {
                const Vector2d px(ndc.get_value() + ndc.get_dx());
                const Vector2d py(ndc.get_value() + ndc.get_dy());

                ray.m_rx.m_org = ray.m_org;
                ray.m_ry.m_org = ray.m_org;

                ray.m_rx.m_dir = compute_ray_direction(px, lens_point, transform);
                ray.m_ry.m_dir = compute_ray_direction(py, lens_point, transform);

                ray.m_has_differentials = true;
            }
        }

        virtual bool connect_vertex(
            SamplingContext&        sampling_context,
            const float             time,
            const Vector3d&         point,
            Vector2d&               ndc,
            Vector3d&               outgoing,
            float&                  importance) const APPLESEED_OVERRIDE
        {
            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(time, scratch);

            // Compute lens point in camera space.
            const Vector3d lens_point = sample_lens(sampling_context);

            // Transform input point to camera space.
            const Vector3d p = transform.point_to_local(point);

            // Compute the outgoing direction vector in camera space.
            outgoing = p - lens_point;

            // Compute intersection of ray with plane of focus in camera space.
            const Vector3d focus_point = lens_point - (m_focal_distance / p.z) * outgoing;

            // Compute film point in camera space.
            const Vector3d film_point = -m_rcp_focal_ratio * focus_point;

            // Convert film point to normalized device coordinates.
            ndc = camera_to_ndc(film_point);

            // The connection is impossible if the film point lies outside the film.
            if (ndc[0] < 0.0 || ndc[0] >= 1.0 ||
                ndc[1] < 0.0 || ndc[1] >= 1.0)
                return false;

            // Transform the outgoing direction vector to world space.
            outgoing = transform.vector_to_parent(outgoing);

            // Compute the emitted importance.
            const double square_dist_film_lens = square_norm(film_point);
            const double dist_film_lens = sqrt(square_dist_film_lens);
            const double cos_theta = m_focal_length / dist_film_lens;
            const double solid_angle = m_pixel_area * cos_theta / square_dist_film_lens;
            importance = 1.0f / static_cast<float>(square_norm(outgoing) * solid_angle);

            // The connection was possible.
            return true;
        }

        virtual bool project_camera_space_point(
            const Vector3d&         point,
            Vector2d&               ndc) const APPLESEED_OVERRIDE
        {
            // Cannot project the point if it is behind the near plane.
            if (point.z > m_near_z)
                return false;

            // Project the point onto the film plane.
            ndc = camera_to_ndc(point);

            // Projection was successful.
            return true;
        }

        virtual bool project_segment(
            const float             time,
            const Vector3d&         a,
            const Vector3d&         b,
            Vector2d&               a_ndc,
            Vector2d&               b_ndc) const APPLESEED_OVERRIDE
        {
            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(time, scratch);

            // Transform the segment to camera space.
            Vector3d local_a = transform.point_to_local(a);
            Vector3d local_b = transform.point_to_local(b);

            // Clip the segment against the near plane.
            if (!clip(Vector4d(0.0, 0.0, 1.0, -m_near_z), local_a, local_b))
                return false;

            // Project the segment onto the film plane.
            a_ndc = camera_to_ndc(local_a);
            b_ndc = camera_to_ndc(local_b);

            // Projection was successful.
            return true;
        }

      private:
        // Parameters.
        Vector2d            m_film_dimensions;          // film dimensions in camera space, in meters
        double              m_focal_length;             // focal length in camera space, in meters
        bool                m_autofocus_enabled;        // is autofocus enabled?
        bool                m_diaphragm_map_bound;      // is a diaphragm_map bound to the camera
        Vector2d            m_autofocus_target;         // autofocus target on film plane, in NDC
        double              m_focal_distance;           // focal distance in camera space
        size_t              m_diaphragm_blade_count;    // number of blades of the diaphragm, 0 for round aperture
        double              m_diaphragm_tilt_angle;     // tilt angle of the diaphragm in radians
        double              m_near_z;                   // Z value of the near plane in camera space, in meters

        // Precomputed values.
        double              m_rcp_film_width;           // film width reciprocal in camera space
        double              m_rcp_film_height;          // film height reciprocal in camera space
        double              m_pixel_area;               // pixel area in meters, in camera space
        double              m_lens_radius;              // radius of the lens in camera space
        double              m_focal_ratio;              // focal distance / focal length
        double              m_rcp_focal_ratio;          // focal length / focal distance

        // Vertices of the diaphragm polygon.
        vector<Vector2d>    m_diaphragm_vertices;

        // Importance sampler to sample the diaphragm map.
        auto_ptr<ImageImportanceSamplerType>
                            m_importance_sampler;
        string              m_diaphragm_map_name;

        void extract_diaphragm_blade_count()
        {
            const int blade_count = m_params.get_optional<int>("diaphragm_blades", 0);

            if (blade_count == 0 || blade_count >= 3)
                m_diaphragm_blade_count = static_cast<size_t>(blade_count);
            else
            {
                m_diaphragm_blade_count = 0;
                RENDERER_LOG_ERROR(
                    "while defining camera \"%s\": invalid value \"%d\" for parameter \"%s\", "
                    "using default value \"" FMT_SIZE_T "\".",
                    get_path().c_str(),
                    blade_count,
                    "diaphragm_blades",
                    m_diaphragm_blade_count);
            }
        }

        void extract_diaphragm_tilt_angle()
        {
            m_diaphragm_tilt_angle =
                deg_to_rad(m_params.get_optional<double>("diaphragm_tilt_angle", 0.0));
        }

        bool build_diaphragm_importance_sampler(const Scene& scene)
        {
            const TextureSource* diaphragm_map_source =
                dynamic_cast<const TextureSource*>(m_inputs.source("diaphragm_map"));

            if (diaphragm_map_source == 0)
                return false;

            const TextureInstance& texture_instance = diaphragm_map_source->get_texture_instance();
            m_diaphragm_map_name = texture_instance.get_name();

            const CanvasProperties& texture_props = texture_instance.get_texture().properties();
            const size_t width = texture_props.m_canvas_width;
            const size_t height = texture_props.m_canvas_height;

            TextureStore texture_store(scene);
            TextureCache texture_cache(texture_store);
            ImageSampler sampler(
                texture_cache,
                diaphragm_map_source,
                width,
                height);

            m_importance_sampler.reset(new ImageImportanceSamplerType(width, height));
            m_importance_sampler->rebuild(sampler);

            return true;
        }

        double get_autofocus_focal_distance(const Intersector& intersector) const
        {
            // The autofocus considers the scene at the middle of the shutter interval.
            const float time = get_shutter_middle_time();
            const Transformd transform = m_transform_sequence.evaluate(time);

            // Create a ray that goes through the center of the lens.
            ShadingRay ray;
            ray.m_org = transform.get_local_to_parent().extract_translation();
            ray.m_dir = normalize(transform.vector_to_parent(-ndc_to_camera(m_autofocus_target)));
            ray.m_tmin = 0.0;
            ray.m_tmax = numeric_limits<double>::max();
            ray.m_time =
                ShadingRay::Time::create_with_normalized_time(
                    0.5f,
                    get_shutter_open_time(),
                    get_shutter_close_time());
            ray.m_flags = VisibilityFlags::ProbeRay;
            ray.m_depth = 0;

            // Trace the ray.
            ShadingPoint shading_point;
            intersector.trace(ray, shading_point);

            if (shading_point.hit())
            {
                // Hit: compute the focal distance.
                const Vector3d v = shading_point.get_point() - ray.m_org;
                const double af_focal_distance = -transform.vector_to_local(v).z;

                RENDERER_LOG_INFO(
                    "camera \"%s\": autofocus sets focal distance to %f (using camera position at time=%.1f).",
                    get_path().c_str(),
                    af_focal_distance,
                    ray.m_time.m_absolute);

                return af_focal_distance;
            }
            else
            {
                // Miss: focus at infinity.
                RENDERER_LOG_INFO(
                    "camera \"%s\": autofocus sets focal distance to infinity (using camera position at time=%.1f).",
                    get_path().c_str(),
                    ray.m_time.m_absolute);

                return 1.0e38;
            }
        }

        void print_settings() const
        {
            RENDERER_LOG_INFO(
                "camera \"%s\" settings:\n"
                "  model                         %s\n"
                "  film width                    %f\n"
                "  film height                   %f\n"
                "  focal length                  %f\n"
                "  autofocus                     %s\n"
                "  autofocus target              %f, %f\n"
                "  diaphragm map                 %s\n"
                "  diaphragm blades              %s\n"
                "  diaphragm angle               %f\n"
                "  near z                        %f\n"
                "  shutter open                  %f\n"
                "  shutter close                 %f",
                get_path().c_str(),
                Model,
                m_film_dimensions[0],
                m_film_dimensions[1],
                m_focal_length,
                m_autofocus_enabled ? "on" : "off",
                m_autofocus_target[0],
                m_autofocus_target[1],
                m_diaphragm_map_bound ? m_diaphragm_map_name.c_str() : "none",
                pretty_uint(m_diaphragm_blade_count).c_str(),
                m_diaphragm_tilt_angle,
                m_near_z,
                m_shutter_open_time,
                m_shutter_close_time);
        }

        Vector3d ndc_to_camera(const Vector2d& point) const
        {
            return
                Vector3d(
                    (0.5 - point.x) * m_film_dimensions[0],
                    (point.y - 0.5) * m_film_dimensions[1],
                    m_focal_length);
        }

        Vector2d camera_to_ndc(const Vector3d& point) const
        {
            const double k = m_focal_length / point.z;
            return
                Vector2d(
                    0.5 - (point.x * k * m_rcp_film_width),
                    0.5 + (point.y * k * m_rcp_film_height));
        }

        Vector3d sample_lens(SamplingContext& sampling_context) const
        {
            if (m_diaphragm_map_bound)
            {
                sampling_context.split_in_place(2, 1);
                const Vector2f s = sampling_context.next2<Vector2f>();

                size_t x, y;
                Vector2d payload;
                float prob_xy;
                m_importance_sampler->sample(s, x, y, payload, prob_xy);

                const Vector2d lens_point = m_lens_radius * payload;
                return Vector3d(lens_point.x, lens_point.y, 0.0);
            }
            else if (m_diaphragm_blade_count == 0)
            {
                sampling_context.split_in_place(2, 1);
                const Vector2d s = sampling_context.next2<Vector2d>();
                const Vector2d lens_point = m_lens_radius * sample_disk_uniform(s);
                return Vector3d(lens_point.x, lens_point.y, 0.0);
            }
            else
            {
                sampling_context.split_in_place(3, 1);
                const Vector3d s = sampling_context.next2<Vector3d>();
                const Vector2d lens_point =
                    m_lens_radius *
                    sample_regular_polygon_uniform(
                        s,
                        m_diaphragm_vertices.size(),
                        &m_diaphragm_vertices.front());
                return Vector3d(lens_point.x, lens_point.y, 0.0);
            }
        }

        Vector3d compute_ray_direction(
            const Vector2d&     film_point,         // NDC
            const Vector3d&     lens_point,         // world space
            const Transformd&   transform) const
        {
            // Compute film point in camera space.
            const Vector3d film_point_cs = ndc_to_camera(film_point);

            // Compute focal point in world space.
            const Vector3d focal_point = transform.point_to_parent(-m_focal_ratio * film_point_cs);

            // Return ray direction in world space.
            return normalize(focal_point - lens_point);
        }
    };
}


//
// ThinLensCameraFactory class implementation.
//

const char* ThinLensCameraFactory::get_model() const
{
    return Model;
}

Dictionary ThinLensCameraFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Thin Lens Camera");
}

DictionaryArray ThinLensCameraFactory::get_input_metadata() const
{
    DictionaryArray metadata = CameraFactory::get_input_metadata();

    metadata.push_back(
        Dictionary()
            .insert("name", "film_dimensions")
            .insert("label", "Film Dimensions")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "film_width")
            .insert("label", "Film Width")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "film_height")
            .insert("label", "Film Height")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "aspect_ratio")
            .insert("label", "Aspect Ratio")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "focal_length")
            .insert("label", "Focal Length")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "horizontal_fov")
            .insert("label", "Horizontal FOV")
            .insert("type", "numeric")
            .insert("min_value", "1.0")
            .insert("max_value", "180.0")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "f_stop")
            .insert("label", "F-Number")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "8.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "focal_distance")
            .insert("label", "Focal Distance")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "autofocus_target")
            .insert("label", "Autofocus Target")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0.5 0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_blades")
            .insert("label", "Diaphragm Blades")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_tilt_angle")
            .insert("label", "Diaphragm Tilt Angle")
            .insert("type", "numeric")
            .insert("min_value", "-360.0")
            .insert("max_value", "360.0")
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_map")
            .insert("label", "Diaphragm Map")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "near_z")
            .insert("label", "Near Z")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "-0.001"));

    return metadata;
}

auto_release_ptr<Camera> ThinLensCameraFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Camera>(new ThinLensCamera(name, params));
}

auto_release_ptr<Camera> ThinLensCameraFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Camera>(new ThinLensCamera(name, params));
}

}   // namespace renderer
