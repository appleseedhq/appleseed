
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/camera/perspectivecamera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/sourceinputs.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/dual.h"
#include "foundation/math/matrix.h"
#include "foundation/math/sampling/imageimportancesampler.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/iostreamop.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class OnRenderBeginRecorder; }

using namespace foundation;

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
        ImageSampler(
            TextureCache&   texture_cache,
            const Source*   source,
            const size_t    width,
            const size_t    height)
          : m_texture_cache(texture_cache)
          , m_source(source)
          , m_width(width)
          , m_height(height)
          , m_range(std::sqrt(1.0 + static_cast<double>(m_height * m_height) / (m_width * m_width)))
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
            m_source->evaluate(m_texture_cache, SourceInputs(uv), color);

            importance = luminance(color);
        }

      private:
        TextureCache&       m_texture_cache;
        const Source*       m_source;
        const size_t        m_width;
        const size_t        m_height;
        const double        m_range;
    };

    const char* Model = "thinlens_camera";

    class ThinLensCamera
      : public PerspectiveCamera
    {
      public:
        ThinLensCamera(
            const char*             name,
            const ParamArray&       params)
          : PerspectiveCamera(name, params)
        {
            m_inputs.declare("diaphragm_map", InputFormat::SpectralReflectance, "");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        void print_settings() const override
        {
            RENDERER_LOG_INFO(
                "camera \"%s\" (#" FMT_UNIQUE_ID ") parameters:\n"
                "  model                         %s\n"
                "  film width                    %f\n"
                "  film height                   %f\n"
                "  focal length                  %f\n"
                "  f-number                      %f\n"
                "  autofocus                     %s\n"
                "  autofocus target              %f, %f\n"
                "  diaphragm map                 %s\n"
                "  diaphragm blades              %s\n"
                "  diaphragm tilt angle          %f\n"
                "  near z                        %f\n"
                "  shift                         %f, %f\n"
                "  shutter open begin time       %f\n"
                "  shutter open end time         %f\n"
                "  shutter close begin time      %f\n"
                "  shutter close end time        %f",
                get_path().c_str(),
                get_uid(),
                Model,
                m_film_dimensions[0],
                m_film_dimensions[1],
                m_focal_length,
                m_f_number,
                m_autofocus_enabled ? "on" : "off",
                m_autofocus_target[0],
                m_autofocus_target[1],
                m_diaphragm_map_bound ? "on" : "off",
                pretty_uint(m_diaphragm_blade_count).c_str(),
                m_diaphragm_tilt_angle,
                m_near_z,
                m_shift.x,
                m_shift.y,
                m_shutter_open_begin_time,
                m_shutter_open_end_time,
                m_shutter_close_begin_time,
                m_shutter_close_end_time);
        }

        bool on_render_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnRenderBeginRecorder&  recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!PerspectiveCamera::on_render_begin(project, parent, recorder, abort_switch))
                return false;

            // Extract autofocus status.
            m_autofocus_enabled = m_params.get_optional<bool>("autofocus_enabled", true);

            // Extract autofocus target and focal distance.
            extract_focal_distance(
                m_autofocus_enabled,
                m_autofocus_target,
                m_focal_distance);

            // Extract diaphragm configuration.
            m_diaphragm_map_bound = build_diaphragm_importance_sampler(*project.get_scene());
            extract_diaphragm_blade_count();
            extract_diaphragm_tilt_angle();

            // Extract F-number.
            m_f_number = extract_f_number();

            // Precompute lens radius.
            m_lens_radius = 0.5 * m_focal_length / m_f_number;

            // Build the diaphragm polygon.
            if (!m_diaphragm_map_bound && m_diaphragm_blade_count > 0)
            {
                m_diaphragm_vertices.resize(m_diaphragm_blade_count);
                build_regular_polygon(
                    m_diaphragm_blade_count,
                    m_diaphragm_tilt_angle,
                    &m_diaphragm_vertices.front());
            }

            return true;
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
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

        void spawn_ray(
            SamplingContext&        sampling_context,
            const Dual2d&           ndc,
            ShadingRay&             ray) const override
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
                ray.m_rx_org = ray.m_org;
                ray.m_ry_org = ray.m_org;
                ray.m_rx_dir = compute_ray_direction(px, lens_point, transform);
                ray.m_ry_dir = compute_ray_direction(py, lens_point, transform);
                ray.m_has_differentials = true;
            }
        }

        bool connect_vertex(
            SamplingContext&        sampling_context,
            const float             time,
            const Vector3d&         point,
            Vector2d&               ndc,
            Vector3d&               outgoing,
            float&                  importance) const override
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
            const double dist_film_lens = std::sqrt(square_dist_film_lens);
            const double cos_theta = m_focal_length / dist_film_lens;
            const double solid_angle = m_pixel_area * cos_theta / square_dist_film_lens;
            importance = 1.0f / static_cast<float>(square_norm(outgoing) * solid_angle);

            // The connection was possible.
            return true;
        }

      private:
        // Parameters.
        double                   m_f_number;                 // F-number
        bool                     m_autofocus_enabled;        // is autofocus enabled?
        Vector2d                 m_autofocus_target;         // autofocus target on film plane, in NDC
        double                   m_focal_distance;           // focal distance in camera space
        bool                     m_diaphragm_map_bound;      // is a diaphragm map bound to the camera
        size_t                   m_diaphragm_blade_count;    // number of blades of the diaphragm, 0 for round aperture
        double                   m_diaphragm_tilt_angle;     // tilt angle of the diaphragm in radians

        // Precomputed values.
        double                   m_lens_radius;              // radius of the lens in camera space
        double                   m_focal_ratio;              // focal distance / focal length
        double                   m_rcp_focal_ratio;          // focal length / focal distance

        // Vertices of the diaphragm polygon.
        std::vector<Vector2d>    m_diaphragm_vertices;

        // Importance sampler to sample the diaphragm map.
        std::unique_ptr<ImageImportanceSamplerType>
                                 m_importance_sampler;

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

        void extract_focal_distance(
            const bool              autofocus_enabled,
            Vector2d&               autofocus_target,
            double&                 focal_distance) const
        {
            const Vector2d DefaultAFTarget(0.5);        // in NDC
            const double DefaultFocalDistance = 1.0;    // in meters

            if (autofocus_enabled)
            {
                if (has_param("autofocus_target"))
                    autofocus_target = m_params.get_required<Vector2d>("autofocus_target", DefaultAFTarget);
                else
                {
                    RENDERER_LOG_ERROR(
                        "while defining camera \"%s\": no \"autofocus_target\" parameter found; "
                        "using default value \"%f, %f\".",
                        get_path().c_str(),
                        DefaultAFTarget[0],
                        DefaultAFTarget[1]);
                    autofocus_target = DefaultAFTarget;
                }

                focal_distance = DefaultFocalDistance;
            }
            else
            {
                if (has_param("focal_distance"))
                    focal_distance = m_params.get_required<double>("focal_distance", DefaultFocalDistance);
                else
                {
                    RENDERER_LOG_ERROR(
                        "while defining camera \"%s\": no \"focal_distance\" parameter found; "
                        "using default value \"%f\".",
                        get_path().c_str(),
                        DefaultFocalDistance);
                    focal_distance = DefaultFocalDistance;
                }

                autofocus_target = DefaultAFTarget;
            }
        }

        double extract_f_number() const
        {
            const double DefaultFNumber = 8.0;

            return get_greater_than_zero("f_stop", DefaultFNumber);
        }

        void extract_diaphragm_tilt_angle()
        {
            m_diaphragm_tilt_angle =
                deg_to_rad(m_params.get_optional<double>("diaphragm_tilt_angle", 0.0));
        }

        bool build_diaphragm_importance_sampler(const Scene& scene)
        {
            const Source* diaphragm_map_source = m_inputs.source("diaphragm_map");
            if (diaphragm_map_source == nullptr)
                return false;

            const Source::Hints diaphragm_map_hints = diaphragm_map_source->get_hints();

            TextureStore texture_store(scene);
            TextureCache texture_cache(texture_store);
            ImageSampler sampler(
                texture_cache,
                diaphragm_map_source,
                diaphragm_map_hints.m_width,
                diaphragm_map_hints.m_height);

            m_importance_sampler.reset(
                new ImageImportanceSamplerType(
                    diaphragm_map_hints.m_width,
                    diaphragm_map_hints.m_height));
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
            ray.m_tmax = std::numeric_limits<double>::max();
            ray.m_time =
                ShadingRay::Time::create_with_normalized_time(
                    0.5f,
                    get_shutter_open_begin_time(),
                    get_shutter_close_end_time());
            ray.m_flags = VisibilityFlags::ProbeRay;
            ray.m_depth = 0;

            // Trace the ray.
            ShadingPoint shading_point;
            intersector.trace(ray, shading_point);

            if (shading_point.hit_surface())
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
            const Vector2d&         film_point,         // NDC
            const Vector3d&         lens_point,         // world space
            const Transformd&       transform) const
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

void ThinLensCameraFactory::release()
{
    delete this;
}

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

    CameraFactory::add_film_metadata(metadata);
    CameraFactory::add_lens_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "f_stop")
            .insert("label", "F-number")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.5")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "256.0")
                    .insert("type", "soft"))
            .insert("use", "required")
            .insert("default", "8.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "autofocus_enabled")
            .insert("label", "Enable autofocus")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "focal_distance")
            .insert("label", "Focal Distance")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("visible_if",
                Dictionary()
                    .insert("autofocus_enabled", "false")));

    metadata.push_back(
        Dictionary()
            .insert("name", "autofocus_target")
            .insert("label", "Autofocus Target")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0.5 0.5")
            .insert("visible_if",
                Dictionary()
                    .insert("autofocus_enabled", "true")));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_blades")
            .insert("label", "Diaphragm Blades")
            .insert("type", "integer")
            .insert("min",
                Dictionary()
                    .insert("value", "3")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "256")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_tilt_angle")
            .insert("label", "Diaphragm Tilt Angle")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-360.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "360.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_map")
            .insert("label", "Diaphragm Map")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional"));

    CameraFactory::add_clipping_metadata(metadata);
    CameraFactory::add_shift_metadata(metadata);

    return metadata;
}

auto_release_ptr<Camera> ThinLensCameraFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Camera>(new ThinLensCamera(name, params));
}

}   // namespace renderer
