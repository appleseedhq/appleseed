
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
#include "foundation/math/vector.h"

// Standard headers.
#include <cmath>

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

template <typename T>
class SphericalCapSampler
{
  public:
    // Constructor.
    SphericalCapSampler(
        const Vector<T, 3>&   center,
        const T               radius)
    {
        const T rcp_center_distance = 1.0f / std::sqrt(dot(center, center));

        // Construct a coordinate frame with z pointing to the sphere.
        m_normal = rcp_center_distance * center;
        m_tangent =
            normalize(
                cross(
                    m_normal,
                    Vector<T, 3>(T(0.0), T(1.0), T(0.0))));
        m_bitangent = cross(m_normal, m_tangent);

        const T max_radius = radius * rcp_center_distance;
        m_minimal_dot =
            std::sqrt(
                saturate(
                    -square(max_radius) + T(1.0)));
        m_solid_angle = -m_minimal_dot * TwoPi<T>() + TwoPi<T>();
    }

    T get_solid_angle() const
    {
        return m_solid_angle;
    }

    Vector<T, 3> sample(const Vector<T, 2>& s) const
    {
        Vector<T, 3> local;

        local.z = lerp(m_minimal_dot, T(1.0), s.x);

        // Complete to a point on the sphere
        T radius = std::sqrt(saturate(-square(local.z) + T(1.0)));
        local.x = radius * cos(TwoPi<T>() * s.y);
        local.y = radius * sin(TwoPi<T>() * s.y);

        // Now turn that into a world space sample
        return local.x * m_tangent + local.y * m_bitangent + local.z * m_normal;
    }

  private:
      Vector<T, 3>    m_tangent;
      Vector<T, 3>    m_bitangent;
      Vector<T, 3>    m_normal;

      T               m_minimal_dot;
      T               m_solid_angle;
};

}  // namespace foundation
