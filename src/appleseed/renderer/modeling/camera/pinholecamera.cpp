
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
#include "pinholecamera.h"

// appleseed.renderer headers.
#include "renderer/modeling/camera/camera.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    //
    // Pinhole camera.
    //

    class PinholeCamera
      : public Camera
    {
      public:
        // Constructor.
        PinholeCamera(
            const char*             name,
            const ParamArray&       params,
            const Transformd&       transform)
          : Camera(params)
          , m_name(name)
          , m_transform(transform)
        {
            m_film_dimensions = get_film_dimensions();
            m_focal_length = get_focal_length(m_film_dimensions[0]);

            m_rcp_film_width = 1.0 / m_film_dimensions[0];
            m_rcp_film_height = 1.0 / m_film_dimensions[1];
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Return a string identifying the model of this camera.
        virtual const char* get_model() const
        {
            return PinholeCameraFactory::get_model();
        }

        // Return the name of this camera.
        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        // Set the camera transformation.
        virtual void set_transform(const Transformd& transform)
        {
            m_transform = transform;
            ++m_version_id;
        }

        // Get the camera transformation.
        virtual const Transformd& get_transform() const
        {
            return m_transform;
        }

        // This method is called once before rendering each frame.
        virtual void on_frame_begin(
            const Scene&            scene,
            const Intersector&      intersector)
        {
            // Precompute the origin of the rays.
            const Transformd::MatrixType& mat = m_transform.get_local_to_parent();
            m_ray_org.x = mat[ 3];
            m_ray_org.y = mat[ 7];
            m_ray_org.z = mat[11];
            const double w = mat[15];
            assert(w != 0.0);
            if (w != 1.0)
                m_ray_org /= w;
        }

        // Generate a ray.
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

            // Set the ray origin.
            ray.m_org = m_ray_org;

            // Transform the film point from NDC to camera space.
            const Vector3d target(
                point.x * m_film_dimensions[0],
                point.y * m_film_dimensions[1],
                -m_focal_length);

            // Set the ray direction.
            ray.m_dir = m_transform.transform_vector_to_parent(target);
        }

        // Project a 3D point back to the film plane.
        virtual Vector2d project(const Vector3d& point) const
        {
            const double k = -m_focal_length / point.z;
            const double x = point.x * k * m_rcp_film_width;
            const double y = point.y * k * m_rcp_film_height;
            return Vector2d(x, y);
        }

      private:
        // Order of data members impacts performance, preserve it.
        uint32          m_pad;                  // for alignment -- todo: omit in 64-bit builds?
        Transformd      m_transform;
        const string    m_name;

        // Parameters.
        Vector2d        m_film_dimensions;      // film dimensions, in meters
        double          m_focal_length;         // focal length, in meters

        // Precomputed values.
        double          m_rcp_film_width;       // film width reciprocal
        double          m_rcp_film_height;      // film height reciprocal
        Vector3d        m_ray_org;              // origin of the rays
    };

}   // anonymous namespace


//
// PinholeCameraFactory class implementation.
//

// Return a string identifying this camera model.
const char* PinholeCameraFactory::get_model()
{
    return "pinhole_camera";
}

// Create a new pinhole camera.
auto_release_ptr<Camera> PinholeCameraFactory::create(
    const char*         name,
    const ParamArray&   params,
    const Transformd&   transform)
{
    return
        auto_release_ptr<Camera>(
            new PinholeCamera(name, params, transform));
}

}   // namespace renderer
