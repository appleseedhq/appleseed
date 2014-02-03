
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/project/project.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/frustum.h"
#include "foundation/math/matrix.h"
#include "foundation/math/sampling.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>
#include <vector>

// Forward declarations.
namespace foundation    { class AbortSwitch; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // A thin lens camera that supports active autofocus.
    //
    // References:
    //
    //   http://en.wikipedia.org/wiki/Thin_lens
    //   http://en.wikipedia.org/wiki/Focal_length
    //   http://en.wikipedia.org/wiki/F-number
    //   http://en.wikipedia.org/wiki/Autofocus
    //   http://en.wikipedia.org/wiki/Diaphragm_(optics)
    //

    const char* Model = "thinlens_camera";

    class ThinLensCamera
      : public Camera
    {
      public:
        ThinLensCamera(
            const char*         name,
            const ParamArray&   params)
          : Camera(name, params)
        {
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&      project,
            AbortSwitch*        abort_switch) OVERRIDE
        {
            if (!Camera::on_frame_begin(project, abort_switch))
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
            extract_diaphragm_blade_count();
            extract_diaphragm_tilt_angle();

            // Compute the view frustum of the camera.
            m_view_frustum = compute_view_frustum(m_film_dimensions, m_focal_length);

            // Precompute reciprocals of film dimensions.
            m_rcp_film_width = 1.0 / m_film_dimensions[0];
            m_rcp_film_height = 1.0 / m_film_dimensions[1];

            // Precompute lens radius.
            m_lens_radius = 0.5 * m_focal_length / extract_f_stop();

            // Perform autofocus, if enabled.
            if (m_autofocus_enabled)
            {
                TextureStore texture_store(*project.get_scene());
                TextureCache texture_cache(texture_store);
                Intersector intersector(project.get_trace_context(), texture_cache);
                m_focal_distance = get_autofocus_focal_distance(intersector);
            }

            // Precompute some more values.
            const double t = m_focal_distance / m_focal_length;
            m_kx = m_film_dimensions[0] * t;
            m_ky = m_film_dimensions[1] * t;

            // Build the diaphragm polygon.
            if (m_diaphragm_blade_count > 0)
            {
                m_diaphragm_vertices.resize(m_diaphragm_blade_count);
                build_regular_polygon(
                    m_diaphragm_blade_count,
                    m_diaphragm_tilt_angle,
                    &m_diaphragm_vertices.front());
            }

            return true;
        }

        virtual void generate_ray(
            SamplingContext&        sampling_context,
            const Vector2d&         point,
            ShadingRay&             ray) const OVERRIDE
        {
            // Initialize the ray.
            initialize_ray(sampling_context, ray);

            // Sample the surface of the lens.
            Vector2d lens_point;
            if (m_diaphragm_blade_count == 0)
            {
                sampling_context.split_in_place(2, 1);
                const Vector2d s = sampling_context.next_vector2<2>();
                lens_point = m_lens_radius * sample_disk_uniform(s);
            }
            else
            {
                sampling_context.split_in_place(3, 1);
                const Vector3d s = sampling_context.next_vector2<3>();
                lens_point =
                    m_lens_radius *
                    sample_regular_polygon_uniform(
                        s,
                        m_diaphragm_vertices.size(),
                        &m_diaphragm_vertices.front());
            }

            // Retrieve the camera transform.
            Transformd tmp;
            const Transformd& transform = m_transform_sequence.evaluate(ray.m_time, tmp);

            // Compute the origin of the ray.
            const Transformd::MatrixType& mat = transform.get_local_to_parent();
            ray.m_org.x =    mat[ 0] * lens_point.x +
                             mat[ 1] * lens_point.y +
                             mat[ 3];
            ray.m_org.y =    mat[ 4] * lens_point.x +
                             mat[ 5] * lens_point.y +
                             mat[ 7];
            ray.m_org.z =    mat[ 8] * lens_point.x +
                             mat[ 9] * lens_point.y +
                             mat[11];
            const double w = mat[12] * lens_point.x +
                             mat[13] * lens_point.y +
                             mat[15];
            assert(w != 0.0);
            if (w != 1.0)
                ray.m_org /= w;

            // Compute the direction of the ray.
            ray.m_dir.x = (point.x - 0.5) * m_kx - lens_point.x;
            ray.m_dir.y = (0.5 - point.y) * m_ky - lens_point.y;
            ray.m_dir.z = -m_focal_distance;
            ray.m_dir = transform.vector_to_parent(ray.m_dir);
        }

        virtual bool project_point(
            const double            time,
            const Vector3d&         point,
            Vector2d&               ndc) const OVERRIDE
        {
            // Retrieve the camera transform.
            Transformd tmp;
            const Transformd& transform = m_transform_sequence.evaluate(time, tmp);

            // Transform the point from world space to camera space.
            const Vector3d point_camera = transform.point_to_local(point);

            // Cannot project the point if it is behind the film plane.
            if (point_camera.z > -m_focal_length)
                return false;

            // Project the point onto the film plane.
            const double k = -m_focal_length / point_camera.z;
            ndc.x = 0.5 + (point_camera.x * k * m_rcp_film_width);
            ndc.y = 0.5 - (point_camera.y * k * m_rcp_film_height);

            // Projection was successful.
            return true;
        }

        virtual bool clip_segment(
            const double            time,
            Vector3d&               v0,
            Vector3d&               v1) const OVERRIDE
        {
            // Retrieve the camera transform.
            Transformd tmp;
            const Transformd& transform = m_transform_sequence.evaluate(time, tmp);

            // Transform the segment from world space to camera space.
            Vector3d v0_camera = transform.point_to_local(v0);
            Vector3d v1_camera = transform.point_to_local(v1);

            // Clip the segment against the view frustum.
            if (m_view_frustum.clip(v0_camera, v1_camera))
            {
                v0 = transform.point_to_parent(v0_camera);
                v1 = transform.point_to_parent(v1_camera);
                return true;
            }
            else return false;
        }

        virtual double get_pixel_solid_angle(
            const Frame&            frame,
            const Vector2d&         point) const OVERRIDE
        {
            const size_t pixel_count = frame.image().properties().m_pixel_count;
            const double pixel_area = m_film_dimensions[0] * m_film_dimensions[1] / pixel_count;

            const Vector3d film_point = ndc_to_camera(point);
            const double d = norm(film_point);
            const double cos_theta = m_focal_length / d;

            return pixel_area * cos_theta / (d * d);
        }

      private:
        // Parameters.
        Vector2d            m_film_dimensions;          // film dimensions in camera space, in meters
        double              m_focal_length;             // focal length in camera space, in meters
        bool                m_autofocus_enabled;        // is autofocus enabled?
        Vector2d            m_autofocus_target;         // autofocus target on film plane, in NDC
        double              m_focal_distance;           // focal distance in camera space
        size_t              m_diaphragm_blade_count;    // number of blades of the diaphragm, 0 for round aperture
        double              m_diaphragm_tilt_angle;     // tilt angle of the diaphragm in radians

        // Precomputed values.
        Pyramid3d           m_view_frustum;             // view frustum in world space
        double              m_rcp_film_width;           // film width reciprocal in camera space
        double              m_rcp_film_height;          // film height reciprocal in camera space
        double              m_lens_radius;              // radius of the lens in camera space
        double              m_kx, m_ky;

        vector<Vector2d>    m_diaphragm_vertices;

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
                    get_name(),
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

        double get_autofocus_focal_distance(const Intersector& intersector) const
        {
            // The autofocus considers the scene at the middle of the shutter interval.
            const double time = get_shutter_middle_time();
            const Transformd transform = m_transform_sequence.evaluate(time);

            // Compute the camera space coordinates of the focus point.
            const Vector3d film_point = ndc_to_camera(m_autofocus_target);

            // Create a ray in world space.
            ShadingRay ray;
            ray.m_org = transform.point_to_parent(Vector3d(0.0));
            ray.m_dir = transform.point_to_parent(film_point) - ray.m_org;
            ray.m_tmin = 0.0;
            ray.m_tmax = numeric_limits<double>::max();
            ray.m_time = time;
            ray.m_type = ShadingRay::ProbeRay;

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
                    get_name(),
                    af_focal_distance,
                    ray.m_time);

                return af_focal_distance;
            }
            else
            {
                // Miss: focus at infinity.
                RENDERER_LOG_INFO(
                    "camera \"%s\": autofocus sets focal distance to infinity (using camera position at time=%.1f).",
                    get_name(),
                    ray.m_time);

                return 1.0e38;
            }
        }

        Vector3d ndc_to_camera(const Vector2d& point) const
        {
            return
                Vector3d(
                    (point.x - 0.5) * m_film_dimensions[0],
                    (0.5 - point.y) * m_film_dimensions[1],
                    -m_focal_length);
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

const char* ThinLensCameraFactory::get_human_readable_model() const
{
    return "Thin Lens Camera";
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
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "f_stop")
            .insert("label", "F-number")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "8.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "focal_distance")
            .insert("label", "Focal Distance")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "autofocus_target")
            .insert("label", "Autofocus Target")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_blades")
            .insert("label", "Diaphragm Blades")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diaphragm_tilt_angle")
            .insert("label", "Diaphragm Tilt Angle")
            .insert("type", "numeric")
            .insert("min_value", "-360.0")
            .insert("max_value", "360.0")
            .insert("use", "required")
            .insert("default", "0.0"));

    return metadata;
}

auto_release_ptr<Camera> ThinLensCameraFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Camera>(new ThinLensCamera(name, params));
}

}   // namespace renderer
