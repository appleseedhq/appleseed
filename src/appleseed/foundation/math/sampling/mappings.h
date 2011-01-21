
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_SAMPLING_MAPPINGS_H
#define APPLESEED_FOUNDATION_MATH_SAMPLING_MAPPINGS_H

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
//   http://www.cs.kuleuven.ac.be/~phil/GI/TotalCompendium.pdf
//   http://jgt.akpeters.com/papers/ShirleyChiu97/
//   http://psgraphics.blogspot.com/2011/01/improved-code-for-concentric-map.html
//


//
// Sphere sampling functions.
//

// Map a uniform sample in [0,1)^2 to a direction over the unit sphere
// with a uniform probability density p(theta) = 1/(4*Pi).
template <typename T>
Vector<T, 3> sample_sphere_uniform(const Vector<T, 2>& s);


//
// Hemisphere sampling functions.
//

// Map a uniform sample in [0,1)^2 to a direction over the unit hemisphere
// with a uniform probability density p(theta) = 1/(2*Pi).
template <typename T>
Vector<T, 3> sample_hemisphere_uniform(const Vector<T, 2>& s);

// Map a uniform sample in [0,1)^2 to a direction over the unit hemisphere
// with a cosine-weighted probability density p(theta) = cos(theta)/Pi.
template <typename T>
Vector<T, 3> sample_hemisphere_cosine(const Vector<T, 2>& s);

// Map a uniform sample in [0,1)^2 to a direction over the unit hemisphere
// with a cosine lobe probability density p(theta) = (n+1)/(2*Pi)*cos(theta)^n.
template <typename T>
Vector<T, 3> sample_hemisphere_cosine(const Vector<T, 2>& s, const T n);


//
// Other sampling functions.
//

// Map a uniform sample in [0,1) to a point on the unit circle with a uniform
// probability density p(x) = 1/(2*Pi).
template <typename T>
Vector<T, 2> sample_circle_uniform(const T s);

// Map a uniform sample in [0,1)^2 to a point on the surface of the unit disk
// with a uniform probability density p(x) = 1/Pi.
template <typename T>
Vector<T, 2> sample_disk_uniform(const Vector<T, 2>& s);

// An alternate implementation of foundation::sample_disk_uniform() based on
// a polar parameterization of the unit disk. This implementation is about
// twice as fast as foundation::sample_disk_uniform() but has high distortion
// around the center of the disk. In practice it is fine for random or even
// QMC sampling.
template <typename T>
Vector<T, 2> sample_disk_uniform_alt(const Vector<T, 2>& s);

// Map a uniform sample in [0,1)^2 to a point on the surface of a triangle
// with a uniform probability density p(x) = 1/A. Return the barycentric
// coordinates of the point inside the triangle.
template <typename T>
Vector<T, 3> sample_triangle_uniform(const Vector<T, 2>& s);


//
// Implementation.
//

template <typename T>
inline Vector<T, 3> sample_sphere_uniform(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T u = T(2.0) * T(Pi) * s[0];
    const T v = T(2.0) * std::sqrt(s[1] * (T(1.0) - s[1]));

    Vector<T, 3> d;
    d.x = std::cos(u) * v;
    d.y = T(1.0) - T(2.0) * s[1];
    d.z = std::sin(u) * v;

    assert(is_normalized(d));

    return d;
}

template <typename T>
inline Vector<T, 3> sample_hemisphere_uniform(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T u = T(2.0) * T(Pi) * s[0];
    const T v = std::sqrt(T(1.0) - s[1] * s[1]);

    Vector<T, 3> d;
    d.x = std::cos(u) * v;
    d.y = s[1];
    d.z = std::sin(u) * v;

    assert(is_normalized(d));

    return d;
}

template <typename T>
inline Vector<T, 3> sample_hemisphere_cosine(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T u = T(2.0) * T(Pi) * s[0];
    const T v = std::sqrt(T(1.0) - s[1]);

    Vector<T, 3> d;
    d.x = std::cos(u) * v;
    d.y = std::sqrt(s[1]);
    d.z = std::sin(u) * v;

    assert(is_normalized(d));

    return d;
}

template <typename T>
inline Vector<T, 3> sample_hemisphere_cosine(const Vector<T, 2>& s, const T n)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));
    assert(n >= T(0.0));

    const T u = T(2.0) * T(Pi) * s[0];
    const T v = T(1.0) / (n + T(1.0));
    const T w = std::sqrt(T(1.0) - std::pow(s[1], T(2.0) * v));

    Vector<T, 3> d;
    d.x = std::cos(u) * w;
    d.y = std::pow(s[1], v);
    d.z = std::sin(u) * w;

    assert(is_normalized(d));

    return d;
}

template <typename T>
inline Vector<T, 2> sample_circle_uniform(const T s)
{
    assert(s >= T(0.0) && s < T(1.0));

    const T phi = s * T(TwoPi);

    return Vector<T, 2>(std::cos(phi), std::sin(phi));
}

template <typename T>
inline Vector<T, 2> sample_disk_uniform(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T a = T(2.0) * s[0] - T(1.0);
    const T b = T(2.0) * s[1] - T(1.0);

    T phi, r;

    if (a * a > b * b)
    {
        r = a;
        phi = T(Pi / 4.0) * (b / a);
    }
    else
    {
        r = b;
        phi = T(Pi / 2.0) - T(Pi / 4.0) * (a / b);
    }

    return Vector<T, 2>(r * std::cos(phi), r * std::sin(phi));
}

template <typename T>
inline Vector<T, 2> sample_disk_uniform_alt(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T r = std::sqrt(T(1.0) - s[1]);
    const T phi = s[0] * T(TwoPi);

    return Vector<T, 2>(r * std::cos(phi), r * std::sin(phi));
}

template <typename T>
inline Vector<T, 3> sample_triangle_uniform(const Vector<T, 2>& s)
{
    assert(s[0] >= T(0.0) && s[0] < T(1.0));
    assert(s[1] >= T(0.0) && s[1] < T(1.0));

    const T sqrt_s0 = std::sqrt(s[0]);

    Vector<T, 3> b;
    b[0] = T(1.0) - sqrt_s0;
    b[1] = (T(1.0) - s[1]) * sqrt_s0;
    b[2] = s[1] * sqrt_s0;

    assert(feq(b[0] + b[1] + b[2], T(1.0)));

    return b;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_SAMPLING_MAPPINGS_H
