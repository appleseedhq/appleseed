
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
#include "sphericalcamera.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
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
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/autoreleaseptr.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }

using namespace foundation;
using namespace std;

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

            // Precompute pixel dimensions.
            const CanvasProperties& props = project.get_frame()->image().properties();
            m_half_pixel_width = 0.5 * props.m_rcp_canvas_width;
            m_half_pixel_height = 0.5 * props.m_rcp_canvas_height;

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
            ray.m_org = transform.get_local_to_parent().extract_translation();
            ray.m_dir = normalize(transform.vector_to_parent(ndc_to_camera(ndc.get_value())));

            // Compute ray derivatives.
            if (ndc.has_derivatives())
            {
                const Vector2d px(ndc.get_value() + ndc.get_dx());
                const Vector2d py(ndc.get_value() + ndc.get_dy());

                ray.m_rx.m_org = ray.m_org;
                ray.m_ry.m_org = ray.m_org;

                ray.m_rx.m_dir = normalize(transform.vector_to_parent(ndc_to_camera(px)));
                ray.m_ry.m_dir = normalize(transform.vector_to_parent(ndc_to_camera(py)));

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

        virtual bool project_camera_space_point(
            const Vector3d&     point,
            Vector2d&           ndc) const APPLESEED_OVERRIDE
        {
            ndc = camera_to_ndc(point);
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

            // Project the segment onto the film plane.
            a_ndc = camera_to_ndc(transform.point_to_local(a));
            b_ndc = camera_to_ndc(transform.point_to_local(b));

            // Projection was successful.
            return true;
        }

      private:
        // Precomputed values.
        double  m_half_pixel_width;     // half pixel width in meters, in camera space
        double  m_half_pixel_height;    // half pixel height in meters, in camera space

        void print_settings() const
        {
            RENDERER_LOG_INFO(
                "camera \"%s\" settings:\n"
                "  model                         %s\n"
                "  shutter open                  %f\n"
                "  shutter close                 %f",
                get_path().c_str(),
                Model,
                m_shutter_open_time,
                m_shutter_close_time);
        }

        static Vector3d ndc_to_camera(const Vector2d& point)
        {
            return Vector3d::make_unit_vector(point.y * Pi<double>(), point.x * TwoPi<double>());
        }

        static Vector2d camera_to_ndc(const Vector3d& point)
        {
            // Compute the unit direction vector from the camera position to the point.
            const Vector3d dir = normalize(point);

            // Convert that direction to spherical coordinates.
            const double phi = atan2(dir.z, dir.x);
            const double theta = acos(dir.y);

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

auto_release_ptr<Camera> SphericalCameraFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Camera>(new SphericalCamera(name, params));
}

}   // namespace renderer
