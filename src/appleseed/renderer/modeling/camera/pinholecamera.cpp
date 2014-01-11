
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
#include "pinholecamera.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
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
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Pinhole camera.
    //

    const char* Model = "pinhole_camera";

    class PinholeCamera
      : public Camera
    {
      public:
        PinholeCamera(
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

            // Extract the film dimensions from the camera parameters.
            m_film_dimensions = extract_film_dimensions();

            // Extract the focal length from the camera parameters.
            m_focal_length = extract_focal_length(m_film_dimensions[0]);

            // Compute the view frustum of the camera.
            m_view_frustum = compute_view_frustum(m_film_dimensions, m_focal_length);

            // Precompute reciprocals of film dimensions.
            m_rcp_film_width = 1.0 / m_film_dimensions[0];
            m_rcp_film_height = 1.0 / m_film_dimensions[1];

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
        Vector2d    m_film_dimensions;      // film dimensions in camera space, in meters
        double      m_focal_length;         // focal length in camera space, in meters

        // Precomputed values.
        Pyramid3d   m_view_frustum;         // view frustum in world space
        double      m_rcp_film_width;       // film width reciprocal in camera space
        double      m_rcp_film_height;      // film height reciprocal in camera space
        Vector3d    m_ray_org;              // origin of the rays in world space

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
// PinholeCameraFactory class implementation.
//

const char* PinholeCameraFactory::get_model() const
{
    return Model;
}

const char* PinholeCameraFactory::get_human_readable_model() const
{
    return "Pinhole Camera";
}

DictionaryArray PinholeCameraFactory::get_input_metadata() const
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

    return metadata;
}

auto_release_ptr<Camera> PinholeCameraFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Camera>(new PinholeCamera(name, params));
}

}   // namespace renderer
