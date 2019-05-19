
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
#include "foundation/math/intersection/rayplane.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// 3D ray-parallelogram intersection functions.
//

template <typename T>
bool intersect_parallelogram(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     corner,
    const Vector<T, 3>&     x,
    const Vector<T, 3>&     y,
    const Vector<T, 3>&     n,
    T&                      t,
    T&                      u,
    T&                      v);

template <typename T>
bool intersect_parallelogram(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     corner,
    const Vector<T, 3>&     x,
    const Vector<T, 3>&     y,
    const Vector<T, 3>&     n);


//
// 3D ray-parallelogram intersection functions implementation.
//

template <typename T>
inline bool intersect_parallelogram(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     corner,
    const Vector<T, 3>&     x,
    const Vector<T, 3>&     y,
    const Vector<T, 3>&     n,
    T&                      t,
    T&                      u,
    T&                      v)
{
    if (!intersect(ray, corner, n, t))
        return false;

    if (t < ray.m_tmin || t >= ray.m_tmax)
        return false;

    const Vector<T, 3> vi = ray.point_at(t) - corner;

    u = dot(x, vi) / dot(x, x);
    if (u < T(0.0) || u > T(1.0))
        return false;

    v = dot(y, vi) / dot(y, y);
    if (v < T(0.0) || v > T(1.0))
        return false;

    return true;
}

template <typename T>
inline bool intersect_parallelogram(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     corner,
    const Vector<T, 3>&     x,
    const Vector<T, 3>&     y,
    const Vector<T, 3>&     n)
{
    T t, u, v;
    return intersect_parallelogram(ray, corner, x, y, n, t, u, v);
}

}   // namespace foundation
