
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
// 3D ray-disk intersection functions.
//

// Test the intersection between a unit-length ray segment and the surface of a disk.
// The disk is assumed to be centered at the origin and pointing to the Z direction.
template <typename T>
inline bool intersect_disk_unit_direction(
    const Ray<T, 3>&        ray,
    const T                 radius);

// Test the intersection between a unit-length ray segment and the surface of a disk.
// The disk is assumed to be centered at the origin and pointing to the Z direction.
// If the ray segment and the disk intersect, the distance to the closest
// intersection is returned in `tmin` and the parametric reprensentation of the surface
// hit is returned in `uv`. Otherwise `tmin` and `uv` are left unchanged.
template <typename T>
inline bool intersect_disk_unit_direction(
    const Ray<T, 3>&        ray,
    const T                 radius,
    T&                      tmin,
    Vector<T, 2>&           uv);


//
// 3D ray-sphere intersection functions implementation.
//

template <typename T>
inline bool intersect_disk_unit_direction(
    const Ray<T, 3>&        ray,
    const T                 radius)
{
    assert(is_normalized(ray.m_dir));

    // Reject rays parallel to the disk's plane.
    const T t = -ray.m_org[2] / ray.m_dir[2];
    if (t <= ray.m_tmin || t >= ray.m_tmax)
        return false;

    // Check if the surface hit is inside the disk.
    Vector<T, 3> hit = ray.point_at(t);
    const T dist_sqr = square(hit[0]) + square(hit[1]);
    if (dist_sqr > square(radius))
        return false;

    return true;
}

template <typename T>
inline bool intersect_disk_unit_direction(
    const Ray<T, 3>&        ray,
    const T                 radius,
    T&                      tmin,
    Vector<T, 2>&           uv)
{
    assert(is_normalized(ray.m_dir));

    // Reject rays parallel to the disk's plane.
    const T t = -ray.m_org[2] / ray.m_dir[2];
    if (t <= ray.m_tmin || t >= ray.m_tmax)
        return false;

    // Check if the surface hit is inside the disk.
    Vector<T, 3> hit = ray.point_at(t);
    const T dist_sqr = square(hit[0]) + square(hit[1]);
    if (dist_sqr > square(radius))
        return false;

    tmin = t;

    // Compute disk's parametric coordinates.
    T phi = std::atan2(hit.y, hit.x);
    if (phi < T(0))
        phi += TwoPi<T>();
    uv[0] = phi / TwoPi<T>();
    uv[1] = T(1) - std::sqrt(dist_sqr) / radius;

    return true;
}

}   // namespace foundation
