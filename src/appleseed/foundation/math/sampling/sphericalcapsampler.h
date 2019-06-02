
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Kevin Masson, The appleseedhq Organization
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

#pragma once

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cmath>

#include <iostream>

namespace foundation
{

//
// References:
//
//   [1] Stratified Sampling of Projected Spherical Caps
//       http://www.iliyan.com/publications/ProjectedSphericalCaps/ProjectedSphericalCaps_EGSR2018.pdf
//
//   [2] Sampling Projected Spherical Caps in Real Time
//       http://momentsingraphics.de/Media/I3D2019/Peters2019-SamplingSphericalCaps.pdf
//
//   [3] Sampling Light Sources
//       http://www.pbr-book.org/3ed-2018/Light_Transport_I_Surface_Reflection/Sampling_Light_Sources.html#sec:sampling-lights
//

// todo: split declaration and definition.
template <typename T>
class SphericalCapSampler
{
  public:
    // Constructor.
    SphericalCapSampler(
        const Vector<T, 3>&   surface_point,
        const Vector<T, 3>&   center,
        const T               radius)
      : m_radius(radius)
    {
        const T rcp_center_distance = T(1.0) / std::sqrt(dot(center, center));

        // Construct a coordinate frame with z pointing to the sphere.
        // todo: use Basis3 here. the vector we give to basis3 is y (normal).
        m_z = normalize(center - surface_point);
        m_x =
            (std::abs(m_z.x) > std::abs(m_z.y))
            ? (Vector<T, 3>(-m_z.z, T(0.0), m_z.x) / std::sqrt(square(m_z.x) + square(m_z.z)))
            : (Vector<T, 3>(T(0.0), m_z.z, -m_z.y) / std::sqrt(square(m_z.y) + square(m_z.z)));
        m_y = cross(m_z, m_x);
        // Make sure (m_x, m_y, m_z) forms an orthonormal basis.
        assert(is_normalized(m_x));
        assert(is_normalized(m_y));
        assert(is_normalized(m_z));
        assert(fz(dot(m_x, m_y), make_eps<T>(1.0e-4f, 1.0e-6)));
        assert(fz(dot(m_x, m_z), make_eps<T>(1.0e-4f, 1.0e-6)));
        assert(fz(dot(m_y, m_z), make_eps<T>(1.0e-4f, 1.0e-6)));

        // Make sure (m_x, m_y, m_z) is right-handed.
        assert(feq(dot(cross(m_x, m_y), m_z), T(1.0), make_eps<T>(1.0e-4f, 1.0e-5)));

        // todo: cache square_distance
        // todo: rename dc to dist
        m_dc = std::sqrt(square_distance(surface_point, center));

        // Compute sampling cone parameters.
        m_sin_theta_max_2 = square(radius) / square_distance(surface_point, center);
        m_cos_theta_max = std::sqrt(std::max(T(0.0), T(1.0) - m_sin_theta_max_2));

        if (square_distance(surface_point, center) < square(radius))
        {
            std::cout << "What the actual fuck, the point is inside.\n";
        }
    }

    T get_pdf() const
    {
        // todo: handle the case where the surface_point is inside the sphere.
        return sample_cone_uniform_pdf(m_cos_theta_max);
    }

    // Return the normal of a point on the spherical cap.
    Vector<T, 3> sample(const Vector<T, 2>& s) const
    {
        // todo: handle the case where the surface_point is inside the sphere.
        const T cos_theta = lerp(T(1.0), m_cos_theta_max, s[0]);
        const T sin_theta = safe_sqrt(T(1.0) - square(cos_theta));
        const T phi = s[1] * TwoPi<T>();

        // todo: compute ds² directly.
        const T ds = m_dc * cos_theta - safe_sqrt(square(m_radius) - square(m_dc) * square(sin_theta));
        const T cos_alpha = (square(m_dc) + square(m_radius) - square(ds)) / (T(2.0) * m_dc * m_radius);
        const T sin_alpha = safe_sqrt(T(1.0) - square(cos_alpha));

        // todo: -m_x -> m_x and -m_y -> m_y.
        const Vector<T, 3> normal =
            sin_alpha * std::cos(phi) * (m_x)
            + sin_alpha * std::sin(phi) * (m_y)
            + cos_alpha * (-m_z);

        return normal;
    }

  private:
      Vector<T, 3>      m_x;
      Vector<T, 3>      m_y;
      Vector<T, 3>      m_z;

      T                 m_radius;
      T                 m_sin_theta_max_2;
      T                 m_cos_theta_max;
      T                 m_dc;
};

}  // namespace foundation
