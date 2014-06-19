
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Srinath Ravichandran, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_BEZIER_H
#define APPLESEED_FOUNDATION_MATH_BEZIER_H

namespace foundation
{

//
// Free functions to evaluate Bezier interpolation polynomials.
//
//   T: type of the interpolation parameter
//   V: type of the interpolated values
//

template <typename T, typename V>
V interpolate_bezier1(const V v0, const V v1, const T t);

template <typename T, typename V>
V interpolate_bezier2(const V v0, const V v1, const V v2, const T t);

template <typename T, typename V>
V interpolate_bezier3(const V v0, const V v1, const V v2, const V v3, const T t);


//
// Implementation.
//

template <typename T, typename V>
inline V interpolate_bezier1(const V v0, const V v1, const T t)
{
    return (T(1.0) - t) * v0 + t * v1;
}

template <typename T, typename V>
inline V interpolate_bezier2(const V v0, const V v1, const V v2, const T t)
{
    // Formula: (1-t)^2 * v0 + 2 * (1-t)*t * v1 + t^2 * v2.

    const T u = T(1.0) - t;
    const T a = u * u;              // (1-t)^2
    const T b = u * t;              // (1-t) * t
    const T c = t * t;              // t^2

    return a * v0 + T(2.0) * b * v1 + c * v2;
}

template <typename T, typename V>
inline V interpolate_bezier3(const V v0, const V v1, const V v2, const V v3, const T t)
{
    // Formula: (1-t)^3 * v0 + 3 * (1-t)^2 * t * v1 + 3 * (1-t) * t^2 * v2 + t^3 * v3.

    const T u = T(1.0) - t;
    const T a = u * u * u;          // (1-t)^3
    const T b = u * u * t;          // (1-t)^2 * t
    const T c = u * t * t;          // (1-t) * t^2
    const T d = t * t * t;          // t^3

    return a * v0 + T(3.0) * (b * v1 + c * v2) + d * v3;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BEZIER_H
