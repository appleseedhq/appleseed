
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/dual.h"
#include "foundation/math/intersection/planesegment.h"
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/autoreleaseptr.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }

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

        virtual bool on_render_begin(
            const Project&      project,
            IAbortSwitch*       abort_switch) APPLESEED_OVERRIDE
        {
            if (!Camera::on_render_begin(project, abort_switch))
                return false;

            // Extract the film dimensions from the camera parameters.
            m_film_dimensions = extract_film_dimensions();

            // Extract the abscissa of the near plane from the camera parameters.
            m_near_z = extract_near_z();

            // Retrieve the scene diameter that will be used to position the camera.
            m_safe_scene_diameter = project.get_scene()->get_render_data().m_safe_diameter;

            // Precompute reciprocals of film dimensions.
            m_rcp_film_width = 1.0 / m_film_dimensions[0];
            m_rcp_film_height = 1.0 / m_film_dimensions[1];

            // Precompute pixel area.
            const size_t pixel_count = project.get_frame()->image().properties().m_pixel_count;
            m_rcp_pixel_area = static_cast<float>(pixel_count / (m_film_dimensions[0] * m_film_dimensions[1]));

            print_settings();

            return true;
        }

        virtual void spawn_ray(
            SamplingContext&    sampling_context,
            const Dual2d&       ndc,
            ShadingRay&         ray) const APPLESEED_OVERRIDE
        {
            // Initialize the ray.
            initialize_ray(sampling_context, ray);

            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform =
                m_transform_sequence.evaluate(ray.m_time.m_absolute, scratch);

            // Compute ray origin and direction.
            ray.m_org = transform.point_to_parent(ndc_to_camera(ndc.get_value()));
            ray.m_dir = normalize(transform.vector_to_parent(Vector3d(0.0, 0.0, -1.0)));

            // Compute ray derivatives.
            if (ndc.has_derivatives())
            {
                const Vector2d px(ndc.get_value() + ndc.get_dx());
                const Vector2d py(ndc.get_value() + ndc.get_dy());

                ray.m_rx.m_org = transform.point_to_parent(ndc_to_camera(px));
                ray.m_ry.m_org = transform.point_to_parent(ndc_to_camera(py));

                ray.m_rx.m_dir = ray.m_dir;
                ray.m_ry.m_dir = ray.m_dir;

                ray.m_has_differentials = true;
            }
        }

        virtual bool connect_vertex(
            SamplingContext&    sampling_context,
            const float         time,
            const Vector3d&     point,
            Vector2d&           ndc,
            Vector3d&           outgoing,
            float&              importance) const APPLESEED_OVERRIDE
        {
            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(time, scratch);

            // Transform the input point to camera space.
            const Vector3d p = transform.point_to_local(point);

            // Compute the normalized device coordinates of the film point.
            ndc[0] = 0.5 + p[0];
            ndc[1] = 0.5 - p[1];

            // The connection is impossible if the projected point lies outside the film.
            if (ndc[0] < 0.0 || ndc[0] >= 1.0 ||
                ndc[1] < 0.0 || ndc[1] >= 1.0)
                return false;

            // Compute the outgoing direction vector in world space.
            outgoing = transform.vector_to_parent(Vector3d(0.0, 0.0, p.z));

            // Compute the emitted importance.
            importance = m_rcp_pixel_area;

            // The connection was possible.
            return true;
        }

        virtual bool project_camera_space_point(
            const Vector3d&     point,
            Vector2d&           ndc) const APPLESEED_OVERRIDE
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
            const float         time,
            const Vector3d&     a,
            const Vector3d&     b,
            Vector2d&           a_ndc,
            Vector2d&           b_ndc) const APPLESEED_OVERRIDE
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
        Vector2d    m_film_dimensions;      // film dimensions in camera space, in meters
        double      m_near_z;               // Z value of the near plane in camera space, in meters

        // Precomputed values.
        double      m_safe_scene_diameter;  // scene diameter plus a safety margin
        double      m_rcp_film_width;       // film width reciprocal in camera space
        double      m_rcp_film_height;      // film height reciprocal in camera space
        float       m_rcp_pixel_area;       // reciprocal of pixel area in camera space

        void print_settings() const
        {
            RENDERER_LOG_INFO(
                "camera \"%s\" settings:\n"
                "  model                         %s\n"
                "  film width                    %f\n"
                "  film height                   %f\n"
                "  near z                        %f\n"
                "  shutter open                  %f\n"
                "  shutter close                 %f",
                get_path().c_str(),
                Model,
                m_film_dimensions[0],
                m_film_dimensions[1],
                m_near_z,
                m_shutter_open_time,
                m_shutter_close_time);
        }

        Vector3d ndc_to_camera(const Vector2d& point) const
        {
            return
                Vector3d(
                    (point.x - 0.5) * m_film_dimensions[0],
                    (0.5 - point.y) * m_film_dimensions[1],
                    m_safe_scene_diameter);
        }

        Vector2d camera_to_ndc(const Vector3d& point) const
        {
            return
                Vector2d(
                    0.5 + point.x * m_rcp_film_width,
                    0.5 - point.y * m_rcp_film_height);
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

    metadata.push_back(
        Dictionary()
            .insert("name", "near_z")
            .insert("label", "Near Z")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "-0.001"));

    return metadata;
}

auto_release_ptr<Camera> OrthographicCameraFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Camera>(new OrthographicCamera(name, params));
}

auto_release_ptr<Camera> OrthographicCameraFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Camera>(new OrthographicCamera(name, params));
}

}   // namespace renderer
