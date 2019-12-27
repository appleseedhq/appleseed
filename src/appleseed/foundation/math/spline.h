
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

namespace foundation
{

//
// 1-dimensional cubic Hermite spline.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/Cubic_Hermite_spline
//

// Compute knot tangents in order to generate a cardinal spline.
template <typename T>
void compute_cardinal_spline_tangents(
    const size_t    knot_count,
    const T         knot_x[],
    const T         knot_y[],
    const T         tension,
    T               knot_derivative[]);

// Evaluate a cubic Hermite spline at a given abscissa.
template <typename T>
T cubic_hermite_spline(
    const size_t    knot_count,
    const T         knot_x[],
    const T         knot_y[],
    const T         knot_derivative[],
    const T         x);

// Evaluate a cubic Hermite spline at multiple abscissa.
template <typename T>
void cubic_hermite_spline(
    const size_t    knot_count,
    const T         knot_x[],
    const T         knot_y[],
    const T         knot_derivative[],
    const size_t    point_count,
    const T         point_x[],
    T               point_y[]);


//
// 1-dimensional cubic Hermite spline implementation.
//

// Compute knot tangents in order to generate a cardinal spline.
template <typename T>
void compute_cardinal_spline_tangents(
    const size_t    knot_count,
    const T         knot_x[],
    const T         knot_y[],
    const T         tension,
    T               knot_derivative[])
{
    assert(knot_count > 0);
    assert(knot_x);
    assert(knot_y);
    assert(knot_derivative);

    if (knot_count > 1)
    {
        const T k = T(1.0) - tension;

        // Compute tangent at first input point.
        knot_derivative[0] =
            k * (knot_y[1] - knot_y[0]) / (knot_x[1] - knot_x[0]);

        // Compute tangent at every intermediate point.
        const size_t u = knot_count - 1;
        for (size_t i = 1; i < u; ++i)
        {
            knot_derivative[i] =
                T(0.5) * k * (
                    (knot_y[i] - knot_y[i - 1]) / (knot_x[i] - knot_x[i - 1]) +
                    (knot_y[i + 1] - knot_y[i]) / (knot_x[i + 1] - knot_x[i]));
        }

        // Compute tangent at last input point.
        knot_derivative[u] =
            k * (knot_y[u] - knot_y[u - 1]) / (knot_x[u] - knot_x[u - 1]);
    }
    else knot_derivative[0] = T(0.0);
}

// Evaluate a cubic Hermite spline at a given abscissa.
template <typename T>
inline T cubic_hermite_spline(
    const size_t    knot_count,
    const T         knot_x[],
    const T         knot_y[],
    const T         knot_derivative[],
    const T         x)
{
    assert(knot_count > 0);
    assert(knot_x);
    assert(knot_y);
    assert(knot_derivative);

    // Check that knots abscissa are strictly increasing.
#ifdef APPLESEED_DEBUG
    for (size_t k = 0; k < knot_count - 1; ++k)
        assert(knot_x[k] < knot_x[k + 1]);
#endif

    // Find the leftmost knot on the right of x.
    const size_t right =
        std::upper_bound(knot_x, knot_x + knot_count, x) - knot_x;

    // Handle evaluation outside of the bounds of the curve.
    if (right == 0)
        return knot_y[0];
    if (right == knot_count)
        return knot_y[knot_count - 1];

    // Find the rightmost knot on the left of x.
    const size_t left = right - 1;

    // Make sure the left and right knots are bracketing x.
    assert(x >= knot_x[left]);
    assert(x <= knot_x[right]);

    const T d = knot_x[right] - knot_x[left];

    // Compute the interpolation parameter t in [0,1].
    const T t = (x - knot_x[left]) / d;
    assert(t >= T(0.0));
    assert(t <= T(1.0));

    // Compute t^2 and t^3.
    const T t2 = t * t;
    const T t3 = t * t2;

    // Compute the Hermite basis functions.
    const T h1 = T(3.0) * t2 - T(2.0) * t3;
    const T h0 = T(1.0) - h1;
    const T h2 = t3 - T(2.0) * t2 + t;
    const T h3 = t3 - t2;

    // Compute and return the value of the curve at x.
    return
        h0 * knot_y[left] +
        h1 * knot_y[right] +
        h2 * knot_derivative[left] * d +
        h3 * knot_derivative[right] * d;
}

// Evaluate a cubic Hermite spline at multiple abscissa.
template <typename T>
void cubic_hermite_spline(
    const size_t    knot_count,
    const T         knot_x[],
    const T         knot_y[],
    const T         knot_derivative[],
    const size_t    point_count,
    const T         point_x[],
    T               point_y[])
{
    assert(knot_count > 0);
    assert(knot_x);
    assert(knot_y);
    assert(knot_derivative);
    assert(point_x);
    assert(point_y);

    for (size_t i = 0; i < point_count; ++i)
    {
        point_y[i] =
            cubic_hermite_spline(
                knot_count,
                knot_x,
                knot_y,
                knot_derivative,
                point_x[i]);
    }
}

}   // namespace foundation
