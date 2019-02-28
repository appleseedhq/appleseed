
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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

// Compute the arc lengths of the sides of a spherical triangle on the unit sphere.
template <typename T>
void compute_spherical_triangle_edge_lengths(
    const Vector<T, 3>& v0,
    const Vector<T, 3>& v1,
    const Vector<T, 3>& v2,
    T&                  a,
    T&                  b,
    T&                  c);

// Compute the interior angles of a spherical triangle on the unit sphere,
// given the arc lengths of its sides.
template <typename T>
void compute_spherical_triangle_interior_angles(
    const T             a,
    const T             b,
    const T             c,
    T&                  alpha,
    T&                  beta,
    T&                  gamma);

// Compute the surface area of a spherical triangle on the unit sphere,
// given its interior angles.
template <typename T>
T compute_spherical_triangle_area(
    const T             alpha,
    const T             beta,
    const T             gamma);


//
// Implementation.
//
// References:
//
//   http://mathworld.wolfram.com/SphericalTrigonometry.html
//   http://en.wikipedia.org/wiki/Spherical_trigonometry
//
//

template <typename T>
void compute_spherical_triangle_edge_lengths(
    const Vector<T, 3>& v0,
    const Vector<T, 3>& v1,
    const Vector<T, 3>& v2,
    T&                  a,
    T&                  b,
    T&                  c)
{
    assert(is_normalized(v0));
    assert(is_normalized(v1));
    assert(is_normalized(v2));

    a = std::acos(dot(v1, v2));
    b = std::acos(dot(v2, v0));
    c = std::acos(dot(v0, v1));
}

template <typename T>
void compute_spherical_triangle_interior_angles(
    const T             a,
    const T             b,
    const T             c,
    T&                  alpha,
    T&                  beta,
    T&                  gamma)
{
    const T s = (a + b + c) * T(0.5);
    const T k = std::sqrt(std::sin(s - a) * std::sin(s - b) * std::sin(s - c) / std::sin(s));

    alpha = T(2.0) * std::atan(k / (std::sin(s - a)));
    beta  = T(2.0) * std::atan(k / (std::sin(s - b)));
    gamma = T(2.0) * std::atan(k / (std::sin(s - c)));
}

template <typename T>
T compute_spherical_triangle_area(
    const T             alpha,
    const T             beta,
    const T             gamma)
{
    return alpha + beta + gamma - Pi<T>();
}

}   // namespace foundation
