
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
#include "sphericalcamera.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/autoreleaseptr.h"

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Project; }

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
            const char*             name,
            const ParamArray&       params)
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
            const Project&          project,
            AbortSwitch*            abort_switch) OVERRIDE
        {
            if (!Camera::on_frame_begin(project, abort_switch))
                return false;

            // Precompute the rays origin in world space if the camera is static.
            if (m_transform_sequence.size() <= 1)
                m_ray_org = m_transform_sequence.evaluate(0.0).point_to_parent(Vector3d(0.0));

            return true;
        }

        virtual void generate_ray(
            SamplingContext&        sampling_context,
            const Vector2d&         point,
            ShadingRay&             ray) const OVERRIDE
        {
            // Initialize the ray.
            initialize_ray(sampling_context, ray);

            // Retrieve the camera transform.
            Transformd tmp;
            const Transformd& transform = m_transform_sequence.evaluate(ray.m_time, tmp);

            // Compute the origin of the ray.
            ray.m_org =
                m_transform_sequence.size() <= 1
                    ? m_ray_org
                    : transform.get_local_to_parent().extract_translation();

            // Compute the direction of the ray.
            ray.m_dir = transform.vector_to_parent(ndc_to_camera(point));
        }

        virtual bool project_point(
            const double            time,
            const Vector3d&         point,
            Vector2d&               ndc) const OVERRIDE
        {
            // Retrieve the camera transform.
            Transformd tmp;
            const Transformd& transform = m_transform_sequence.evaluate(time, tmp);

            // Compute the unit direction vector from the camera position to the point.
            const Vector3d dir = normalize(point);

            // Convert that direction to spherical coordinates.
            const double phi = atan2(dir.z, dir.x);
            const double theta = acos(dir.y);

            // Convert the spherical coordinates to normalized device coordinates.
            ndc.x = wrap(phi * RcpTwoPi);
            ndc.y = saturate(theta * RcpPi);

            // Projection was successful.
            return true;
        }

        virtual bool clip_segment(
            const double            time,
            Vector3d&               v0,
            Vector3d&               v1) const OVERRIDE
        {
            // No clipping necessary.
            return true;
        }

        virtual double get_pixel_solid_angle(
            const Frame&            frame,
            const Vector2d&         point) const OVERRIDE
        {
            const CanvasProperties& props = frame.image().properties();
            const double half_pixel_width = 0.5 * props.m_rcp_canvas_width;
            const double half_pixel_height = 0.5 * props.m_rcp_canvas_height;

            const Vector2d p0 = Vector2d(point.x - half_pixel_width, point.y - half_pixel_height);
            const Vector2d p1 = Vector2d(point.x + half_pixel_width, point.y - half_pixel_height);
            const Vector2d p2 = Vector2d(point.x + half_pixel_width, point.y + half_pixel_height);
            const Vector2d p3 = Vector2d(point.x - half_pixel_width, point.y + half_pixel_height);

            const Vector3d q0 = ndc_to_camera(p0);
            const Vector3d q1 = ndc_to_camera(p1);
            const Vector3d q2 = ndc_to_camera(p2);
            const Vector3d q3 = ndc_to_camera(p3);

            return 0.5 * norm(cross(q2 - q0, q3 - q1));
        }

      private:
        Vector3d m_ray_org;         // origin of the rays in world space

        static Vector3d ndc_to_camera(const Vector2d& point)
        {
            return Vector3d::unit_vector(point.y * Pi, point.x * TwoPi);
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

const char* SphericalCameraFactory::get_human_readable_model() const
{
    return "Spherical Camera";
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
