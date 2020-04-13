
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
#include "sphericalcamera.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/rasterization/rasterizationcamera.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/dual.h"
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class OnRenderBeginRecorder; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Spherical camera.
    //

    const char* Model = "spherical_camera";

    class SphericalCamera
      : public Camera
    {
      public:
        SphericalCamera(
            const char*             name,
            const ParamArray&       params)
          : Camera(name, params)
        {
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
                "  shutter open begin time       %f\n"
                "  shutter open end time         %f\n"
                "  shutter close begin time      %f\n"
                "  shutter close end time        %f",
                get_path().c_str(),
                get_uid(),
                Model,
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
            if (!Camera::on_render_begin(project, parent, recorder, abort_switch))
                return false;

            // Precompute pixel dimensions.
            const CanvasProperties& props = project.get_frame()->image().properties();
            m_half_pixel_width = 0.5 * props.m_rcp_canvas_width;
            m_half_pixel_height = 0.5 * props.m_rcp_canvas_height;

            return true;
        }

        void spawn_ray(
            SamplingContext&        sampling_context,
            const Dual2d&           ndc,
            ShadingRay&             ray) const override
        {
            // Initialize the ray.
            initialize_ray(sampling_context, ray);

            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform =
                m_transform_sequence.evaluate(ray.m_time.m_absolute, scratch);

            // Compute ray origin and direction.
            ray.m_org = transform.get_local_to_parent().extract_translation();
            ray.m_dir = normalize(transform.vector_to_parent(ndc_to_camera(ndc.get_value())));

            // Compute ray derivatives.
            if (ndc.has_derivatives())
            {
                const Vector2d px(ndc.get_value() + ndc.get_dx());
                const Vector2d py(ndc.get_value() + ndc.get_dy());
                ray.m_rx_org = ray.m_org;
                ray.m_ry_org = ray.m_org;
                ray.m_rx_dir = normalize(transform.vector_to_parent(ndc_to_camera(px)));
                ray.m_ry_dir = normalize(transform.vector_to_parent(ndc_to_camera(py)));
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

            // Transform the input point to camera space.
            const Vector3d p = transform.point_to_local(point);

            // Compute the outgoing direction vector in world space.
            outgoing = transform.vector_to_parent(p);

            // Compute the normalized device coordinates of the film point.
            ndc = camera_to_ndc(p);

            // Compute the emitted importance.
            const Vector3d q0 = ndc_to_camera(Vector2d(ndc.x - m_half_pixel_width, ndc.y - m_half_pixel_height));
            const Vector3d q1 = ndc_to_camera(Vector2d(ndc.x + m_half_pixel_width, ndc.y - m_half_pixel_height));
            const Vector3d q2 = ndc_to_camera(Vector2d(ndc.x + m_half_pixel_width, ndc.y + m_half_pixel_height));
            const Vector3d q3 = ndc_to_camera(Vector2d(ndc.x - m_half_pixel_width, ndc.y + m_half_pixel_height));
            const double solid_angle = 0.5 * norm(cross(q2 - q0, q3 - q1));
            importance = 1.0f / static_cast<float>(square_norm(outgoing) * solid_angle);

            // The connection was possible.
            return true;
        }

        bool project_camera_space_point(
            const Vector3d&         point,
            Vector2d&               ndc) const override
        {
            ndc = camera_to_ndc(point);
            return true;
        }

        bool project_segment(
            const float             time,
            const Vector3d&         a,
            const Vector3d&         b,
            Vector2d&               a_ndc,
            Vector2d&               b_ndc) const override
        {
            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(time, scratch);

            // Project the segment onto the film plane.
            a_ndc = camera_to_ndc(transform.point_to_local(a));
            b_ndc = camera_to_ndc(transform.point_to_local(b));

            // Projection was successful.
            return true;
        }

        RasterizationCamera get_rasterization_camera() const override
        {
            RasterizationCamera rc;
            rc.m_aspect_ratio = 1024.0 / 576.0;
            rc.m_hfov = deg_to_rad(54.0);
            rc.m_shift_x = rc.m_shift_y = 0.0;
            return rc;
        }

      private:
        // Precomputed values.
        double  m_half_pixel_width;     // half pixel width in meters, in camera space
        double  m_half_pixel_height;    // half pixel height in meters, in camera space

        static Vector3d ndc_to_camera(const Vector2d& point)
        {
            return Vector3d::make_unit_vector(point.y * Pi<double>(), point.x * TwoPi<double>());
        }

        static Vector2d camera_to_ndc(const Vector3d& point)
        {
            // Compute the unit direction vector from the camera position to the point.
            const Vector3d dir = normalize(point);

            // Convert that direction to spherical coordinates.
            const double phi = std::atan2(dir.z, dir.x);
            const double theta = std::acos(dir.y);

            // Convert the spherical coordinates to normalized device coordinates.
            return Vector2d(
                wrap(phi * RcpTwoPi<double>()),
                saturate(theta * RcpPi<double>()));
        }
    };
}


//
// SphericalCameraFactory class implementation.
//

void SphericalCameraFactory::release()
{
    delete this;
}

const char* SphericalCameraFactory::get_model() const
{
    return Model;
}

Dictionary SphericalCameraFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Spherical Camera");
}

DictionaryArray SphericalCameraFactory::get_input_metadata() const
{
    return CameraFactory::get_input_metadata();
}

auto_release_ptr<Camera> SphericalCameraFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Camera>(new SphericalCamera(name, params));
}

}   // namespace renderer
