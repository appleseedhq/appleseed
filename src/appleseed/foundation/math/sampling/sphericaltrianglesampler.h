
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/scalar.h"
#include "foundation/math/sphericaltriangle.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// References:
//
//   Stratified Sampling of Spherical Triangles.
//   https://www.graphics.cornell.edu/pubs/1995/Arv95c.pdf
//
//   http://mathworld.wolfram.com/SphericalTrigonometry.html
//   http://en.wikipedia.org/wiki/Spherical_trigonometry
//

template <typename T>
class SphericalTriangleSampler
{
  public:
    SphericalTriangleSampler(
        const Vector<T, 3>& v0,
        const Vector<T, 3>& v1,
        const Vector<T, 3>& v2,
        const Vector<T, 3>& center)
      : m_v0(normalize(v0 - center))
      , m_v1(normalize(v1 - center))
      , m_v2(normalize(v2 - center))
    {
        T a, b, c;
        compute_spherical_triangle_edge_lengths(m_v0, m_v1, m_v2, a, b, c);

        compute_spherical_triangle_interior_angles(a, b, c, m_alpha, m_beta, m_gamma);
        m_cos_alpha = std::cos(m_alpha);
        m_sin_alpha = std::sin(m_alpha);

        m_area = compute_spherical_triangle_area(m_alpha, m_beta, m_gamma);
    }

    T get_area() const
    {
        return m_area;
    }

    Vector<T, 3> sample(const Vector<T, 2>& s) const
    {
        // Use one random variable to select the new area.
        const T area_hat = s[0] * m_area;

        // Save the sine and cosine of the angle phi.
        const T sin_phi = std::sin(area_hat - m_alpha);
        const T cos_phi = std::cos(area_hat - m_alpha);

        // Compute the pair (u, v) that determines beta_hat.
        const T cos_c = dot(m_v0, m_v1);
        const T u = cos_phi - m_cos_alpha;
        const T v = sin_phi + m_sin_alpha * cos_c;

        // Let q be the cosine of the new edge length b_hat.
        const T q_num = (v * cos_phi - u * sin_phi) * m_cos_alpha - v;
        const T q_den = (v * sin_phi + u * cos_phi) * m_sin_alpha;
        const T q = clamp(q_num / q_den, T(-1.0), T(1.0));

        // Compute the third vertex of the sub-triangle.
        const Vector<T, 3> C_hat = q * m_v0 + std::sqrt(T(1.0) - q * q) * normalize(m_v2 - dot(m_v2, m_v0) * m_v0);
        const T dot_C_hat_B = dot(C_hat, m_v1);

        // Use the other random variable to select cos(theta).
        const T z = T(1.0) - s[1] * (T(1.0) - dot_C_hat_B);

        // Construct the corresponding point on the sphere.
        return z * m_v1 + std::sqrt(T(1.0) - z * z) * normalize(C_hat - dot_C_hat_B * m_v1);
    }

  private:
    const Vector<T, 3>  m_v0;
    const Vector<T, 3>  m_v1;
    const Vector<T, 3>  m_v2;
    T                   m_alpha;
    T                   m_beta;
    T                   m_gamma;
    T                   m_cos_alpha;
    T                   m_sin_alpha;
    T                   m_area;
};

}  // namespace foundation
