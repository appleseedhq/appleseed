
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
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// References:
//
//   An Area-Preserving Parametrization for Spherical Rectangles.
//   https://www.arnoldrenderer.com/research/egsr2013_spherical_rectangle.pdf
//

template <typename T>
class SphericalRectangleSampler
{
  public:
    SphericalRectangleSampler(
        const Vector<T, 3>& origin,
        const Vector<T, 3>& x_axis,
        const Vector<T, 3>& y_axis,
        const Vector<T, 3>& normal,
        const Vector<T, 3>& center)
      : m_x(x_axis)
      , m_y(y_axis)
      , m_z(normal)
      , m_center(center)
    {
        const Vector<T, 3> d = origin - center;

        const T width = norm(x_axis);
        const T height = norm(y_axis);

        const T x0 = dot(d, x_axis);
        const T x1 = x0 + width;

        m_y0 = dot(d, y_axis);
        m_y1 = m_y0 + height;
        m_z0 = dot(d, normal);

        // z flip
        if (m_z0 > T(0.0))
        {
            m_z0 *= T(-1.0);
            m_z  *= T(-1.0);
        }

        m_n0 = Vector<T, 3>(T(0.0), m_z0, -m_y0);
        m_n2 = Vector<T, 3>(T(0.0), -m_z0, m_y1);
        const Vector<T, 3> n1 = Vector<T, 3>(-m_z0, T(0.0), x1);
        const Vector<T, 3> n3 = Vector<T, 3>(m_z0, T(0.0), -x0);

        m_y0y0 = square(m_y0);
        m_y1y1 = square(m_y1);
        m_z0z0 = square(m_z0);

        m_n0.z /= std::sqrt(m_z0z0 + m_y0y0);
        m_n2.z /= std::sqrt(m_z0z0 + m_y1y1);
        n1.z /= std::sqrt(m_z0z0 + square(x1));
        n3.z /= std::sqrt(m_z0z0 + square(x0));

        const T g0 = std::acos(-m_n0.z * n1.z);
        const T g1 = std::acos(-n1.z * m_n2.z);
        m_g2 = std::acos(-m_n2.z * n3.z);
        m_g3 = std::acos(-n3.z * m_n0.z);

        m_area = g0 + g1 + m_g2 + m_g3 - TwoPi<T>();
    }

    T get_area() const
    {
        return m_area;
    }

    Vector<T, 3> sample(const Vector<T, 2>& s) const
    {
        const T au = s[0] * m_area - m_g2 - m_g3 + TwoPi<T>();

        const T b0 = m_n0.z;
        const T b1 = m_n2.z;
        const T fu = (std::cos(au) * b0 - b1) / std::sin(au);

        const T cu =
            clamp(
                std::copysign(T(1.0), fu) / std::sqrt(square(fu) + square(b0)),
                T(-1.0),
                T(1.0));

        const T xu = (-cu * m_z0) / safe_sqrt(T(1.0) - square(cu));
        const T d = std::sqrt(square(xu) + m_z0z0);
        const T d2 = square(d);

        const T h0 = m_y0 / std::sqrt(d2 + m_y0y0);
        const T h1 = m_y1 / std::sqrt(d2 + m_y1y1);

        const T hv = lerp(h0, h1, s[1]);
        const T hv2 = square(hv);
        const T yv = (hv * d) / safe_sqrt(T(1.0) - hv2);

        return m_center + xu * m_x + yv * m_y + m_z0 * m_z;
    }

  private:
    const Vector<T, 3>  m_x;
    const Vector<T, 3>  m_y;
    Vector<T, 3>        m_z;
    const Vector<T, 3>  m_center;

    T                   m_y0;
    T                   m_y1;
    T                   m_z0;
    T                   m_z0z0;
    T                   m_y0y0;
    T                   m_y1y1;

    T                   m_area;

    Vector<T, 3>        m_n0;
    Vector<T, 3>        m_n2;
    T                   m_g2;
    T                   m_g3;
};

}  // namespace foundation
