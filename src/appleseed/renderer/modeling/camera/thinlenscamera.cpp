
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/camera/camera.h"

// appleseed.foundation headers.
#include "foundation/core/exceptionnotimplemented.h"
#include "foundation/math/sampling.h"
#include "foundation/utility/string.h"

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
    //

    class ThinLensCamera
      : public Camera
    {
      public:
        ThinLensCamera(
            const char*         name,
            const ParamArray&   params,
            const Transformd&   transform)
          : Camera(params)
          , m_name(name)
          , m_transform(transform)
        {
            m_film_dimensions = get_film_dimensions();
            m_focal_length = get_focal_length(m_film_dimensions[0]);
            m_f_stop = get_f_stop();

            get_focal_distance(
                m_autofocus_enabled,
                m_autofocus_target,
                m_focal_distance);

            // Compute the radius of the lens.
            m_lens_radius = 0.5 * m_focal_length / m_f_stop;

            // Precompute some values.
            m_rcp_film_width = 1.0 / m_film_dimensions[0];
            m_rcp_film_height = 1.0 / m_film_dimensions[1];
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return ThinLensCameraFactory::get_model();
        }

        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        virtual void set_transform(const Transformd& transform)
        {
            m_transform = transform;
            ++m_version_id;
        }

        virtual const Transformd& get_transform() const
        {
            return m_transform;
        }

        virtual void on_frame_begin(
            const Scene&            scene,
            const Intersector&      intersector)
        {
            // Perform autofocus, if enabled.
            if (m_autofocus_enabled)
                autofocus(intersector);

            // Precompute some values.
            const double t = m_focal_distance / m_focal_length;
            m_kx = m_film_dimensions[0] * t;
            m_ky = m_film_dimensions[1] * t;
        }

        virtual void generate_ray(
            SamplingContext&        sampling_context,
            const Vector2d&         point,
            const float             time,
            ShadingRay&             ray) const
        {
            // Create the ray.
            ray.m_tmin = 0.0;
            ray.m_tmax = numeric_limits<double>::max();
            ray.m_time = time;
            ray.m_flags = ~0;

            // Sample the surface of the lens.
            sampling_context = sampling_context.split(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector2d lens_point = m_lens_radius * sample_disk_uniform(s);

            // Set the ray origin.
            const Transformd::MatrixType& mat = m_transform.get_local_to_parent();
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

            // Compute the location of the focus point.
            const Vector3d focus_point(
                point.x * m_kx,
                point.y * m_ky,
                -m_focal_distance);

            // Set the ray direction.
            ray.m_dir = focus_point;
            ray.m_dir.x -= lens_point.x;
            ray.m_dir.y -= lens_point.y;
            ray.m_dir = m_transform.transform_vector_to_parent(ray.m_dir);
        }

        virtual Vector2d project(const Vector3d& point) const
        {
            throw ExceptionNotImplemented();
            return Vector2d(0.0);
        }

      private:
        // Order of data members impacts performance, preserve it.
        uint32              m_pad;                  // for alignment -- todo: omit in 64-bit builds?
        Transformd          m_transform;            // camera transformation
        const string        m_name;

        // Parameters.
        Vector2d            m_film_dimensions;      // film dimensions, in meters
        double              m_focal_length;         // focal length, in meters
        double              m_f_stop;               // f-stop
        bool                m_autofocus_enabled;    // is autofocus enabled?
        Vector2d            m_autofocus_target;     // autofocus target on film plane, in NDC
        double              m_focal_distance;       // focal distance, in meters
        double              m_lens_radius;          // radius of the lens, in meters, in local space

        // Precomputed values.
        double              m_rcp_film_width;       // film width reciprocal
        double              m_rcp_film_height;      // film height reciprocal
        double              m_kx, m_ky;

        // Perform autofocus.
        void autofocus(const Intersector& intersector)
        {
            // Create a ray.
            ShadingRay ray;
            ray.m_tmin = 0.0;
            ray.m_tmax = numeric_limits<double>::max();
            ray.m_time = 0.0f;
            ray.m_flags = ~0;

            // Set the ray origin.
            const Transformd::MatrixType& mat = m_transform.get_local_to_parent();
            ray.m_org.x = mat[ 3];
            ray.m_org.y = mat[ 7];
            ray.m_org.z = mat[11];
            const double w = mat[15];
            assert(w != 0.0);
            if (w != 1.0)
                ray.m_org /= w;

            // Transform the film point from NDC to camera space.
            const Vector3d target(
                m_autofocus_target[0] * m_film_dimensions[0],
                m_autofocus_target[1] * m_film_dimensions[1],
                -m_focal_length);

            // Set the ray direction.
            ray.m_dir = m_transform.transform_vector_to_parent(target);

            // Normalize the ray direction, since we're interested in the
            // actual distance to the closest intersection, in meters.
            ray.m_dir = normalize(ray.m_dir);

            // Trace the ray.
            ShadingPoint shading_point;
            intersector.trace(ray, shading_point);

            if (shading_point.hit())
            {
                // Compute the focal distance.
                const Vector3d v = shading_point.get_point() - ray.m_org;
                const Vector3d camera_direction =
                    m_transform.transform_vector_to_parent(Vector3d(0.0, 0.0, -1.0));
                m_focal_distance = dot(v, camera_direction);
                RENDERER_LOG_DEBUG(
                    "camera \"%s\": autofocus sets focal distance to %f %s",
                    m_name.c_str(),
                    m_focal_distance,
                    plural(m_focal_distance, "meter").c_str());
            }
            else
            {
                // Focus at infinity.
                m_focal_distance = 1.0e38;
                RENDERER_LOG_DEBUG(
                    "camera \"%s\": autofocus sets focal distance to infinity",
                    m_name.c_str());
            }
        }
    };
}


//
// ThinLensCameraFactory class implementation.
//

const char* ThinLensCameraFactory::get_model()
{
    return "thinlens_camera";
}

auto_release_ptr<Camera> ThinLensCameraFactory::create(
    const char*         name,
    const ParamArray&   params,
    const Transformd&   transform) const
{
    return
        auto_release_ptr<Camera>(
            new ThinLensCamera(name, params, transform));
}

}   // namespace renderer
