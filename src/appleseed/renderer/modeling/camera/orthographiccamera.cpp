
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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
#include "orthographiccamera.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/dual.h"
#include "foundation/math/intersection/frustumsegment.h"
#include "foundation/math/frustum.h"
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/autoreleaseptr.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Orthographic camera.
    //

    const char* Model = "orthographic_camera";

    class OrthographicCamera
      : public Camera
    {
      public:
        OrthographicCamera(
            const char*         name,
            const ParamArray&   params)
          : Camera(name, params)
        {
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&      project,
            IAbortSwitch*       abort_switch) APPLESEED_OVERRIDE
        {
            if (!Camera::on_frame_begin(project, abort_switch))
                return false;

            // Extract the film dimensions from the camera parameters.
            m_film_dimensions = extract_film_dimensions();

            // Compute the view frustum of the camera.
            m_view_frustum = compute_view_frustum(m_film_dimensions);

            // Precompute reciprocals of film dimensions.
            m_rcp_film_width = 1.0 / m_film_dimensions[0];
            m_rcp_film_height = 1.0 / m_film_dimensions[1];

            print_settings();

            return true;
        }

        virtual void generate_ray(
            SamplingContext&    sampling_context,
            const Dual2d&       point,
            ShadingRay&         ray) const APPLESEED_OVERRIDE
        {
            // Initialize the ray.
            initialize_ray(sampling_context, ray);

            // Retrieve the camera transform.
            Transformd tmp;
            const Transformd& transform = m_transform_sequence.evaluate(ray.m_time.m_absolute, tmp);

            // Compute the origin of the ray.
            ray.m_org = transform.point_to_parent(ndc_to_camera(point.get_value()));

            // Compute the direction of the ray.
            ray.m_dir = normalize(transform.vector_to_parent(Vector3d(0.0, 0.0, -1.0)));

            if (point.has_derivatives())
            {
                ray.m_has_differentials = true;

                ray.m_rx.m_org = transform.point_to_parent(ndc_to_camera(point.get_value() + point.get_dx()));
                ray.m_ry.m_org = transform.point_to_parent(ndc_to_camera(point.get_value() + point.get_dy()));

                ray.m_rx.m_dir = ray.m_dir;
                ray.m_ry.m_dir = ray.m_dir;
            }
        }

        virtual bool project_camera_space_point(
            const Vector3d&     point,
            Vector2d&           ndc) const APPLESEED_OVERRIDE
        {
            // Cannot project the point if it is behind the film plane.
            if (point.z > 0.0)
                return false;

            // Project the point onto the film plane.
            ndc.x = 0.5 + point.x * m_rcp_film_width;
            ndc.y = 0.5 - point.y * m_rcp_film_height;

            // Projection was successful.
            return true;
        }

        virtual bool clip_segment(
            const double        time,
            Vector3d&           v0,
            Vector3d&           v1) const APPLESEED_OVERRIDE
        {
            // Retrieve the camera transform.
            Transformd tmp;
            const Transformd& transform = m_transform_sequence.evaluate(time, tmp);

            // Transform the segment from world space to camera space.
            Vector3d v0_camera = transform.point_to_local(v0);
            Vector3d v1_camera = transform.point_to_local(v1);

            // Clip the segment against the view frustum.
            if (clip(m_view_frustum, v0_camera, v1_camera))
            {
                v0 = transform.point_to_parent(v0_camera);
                v1 = transform.point_to_parent(v1_camera);
                return true;
            }
            else return false;
        }

        virtual double get_pixel_solid_angle(
            const Frame&        frame,
            const Vector2d&     point) const APPLESEED_OVERRIDE
        {
            // todo: implement.
            return 0.0;
        }

      private:
        // Parameters.
        Vector2d    m_film_dimensions;      // film dimensions in camera space, in meters

        // Precomputed values.
        Frustum     m_view_frustum;         // view frustum in world space
        double      m_rcp_film_width;       // film width reciprocal in camera space
        double      m_rcp_film_height;      // film height reciprocal in camera space

        void print_settings() const
        {
            RENDERER_LOG_INFO(
                "camera settings:\n"
                "  model            %s\n"
                "  film width       %f\n"
                "  film height      %f\n"
                "  shutter open     %f\n"
                "  shutter close    %f",
                Model,
                m_film_dimensions[0],
                m_film_dimensions[1],
                m_shutter_open_time,
                m_shutter_close_time);
        }

        static Camera::Frustum compute_view_frustum(const Vector2d& film_dimensions)
        {
            const double half_film_width = 0.5 * film_dimensions[0];
            const double half_film_height = 0.5 * film_dimensions[1];

            Frustum frustum;

            // Top plane.
            frustum.set_plane(0, Vector4d(0.0, +1.0, 0.0, -half_film_height));

            // Bottom plane.
            frustum.set_plane(1, Vector4d(0.0, -1.0, 0.0, -half_film_height));

            // Left plane.
            frustum.set_plane(2, Vector4d(-1.0, 0.0, 0.0, -half_film_width));

            // Right plane.
            frustum.set_plane(3, Vector4d(+1.0, 0.0, 0.0, -half_film_width));

            return frustum;
        }

        Vector3d ndc_to_camera(const Vector2d& point) const
        {
            return
                Vector3d(
                    (point.x - 0.5) * m_film_dimensions[0],
                    (0.5 - point.y) * m_film_dimensions[1],
                    0.0);
        }
    };
}


//
// OrthographicCameraFactory class implementation.
//

const char* OrthographicCameraFactory::get_model() const
{
    return Model;
}

Dictionary OrthographicCameraFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Orthographic Camera");
}

DictionaryArray OrthographicCameraFactory::get_input_metadata() const
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

    return metadata;
}

auto_release_ptr<Camera> OrthographicCameraFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Camera>(new OrthographicCamera(name, params));
}

}   // namespace renderer
