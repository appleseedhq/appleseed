
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
// 3D ray-sphere intersection functions.
//
// Each function comes in two variants:
//   - One for ray directions of arbitrary lengths
//   - One for unit-length ray directions
//

// Test the intersection between a ray segment and the surface of a sphere.
template <typename T>
bool intersect_sphere(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius);
template <typename T>
bool intersect_sphere_unit_direction(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius);

// Test the intersection between a ray segment and the surface of a sphere.
// If the ray segment and the sphere intersect, the distance to the closest
// intersection is returned in `tmin`. Otherwise `tmin` is left unchanged.
template <typename T>
bool intersect_sphere(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius,
    T&                      tmin);
template <typename T>
bool intersect_sphere_unit_direction(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius,
    T&                      tmin);

// Test the intersection between a ray segment and the surface of a sphere.
// Return the number (0, 1 or 2) of intersections between the ray segment
// and the sphere. The distances to the intersection points are stored in
// `t_out` and are ordered by ascending distance (the closest intersection
// is the first value in `t_out`).
template <typename T>
size_t intersect_sphere(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius,
    T                       t_out[2]);
template <typename T>
size_t intersect_sphere_unit_direction(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius,
    T                       t_out[2]);


//
// 3D ray-sphere intersection functions implementation.
//

template <typename T>
inline bool intersect_sphere(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius)
{
    const T a = dot(ray.m_dir, ray.m_dir);
    assert(a > T(0.0));

    const Vector<T, 3> v = center - ray.m_org;
    const T b = dot(ray.m_dir, v);

    const T d = square(b) - a * (dot(v, v) - square(radius));
    if (d >= T(0.0))
    {
        const T sqrt_d = std::sqrt(d);
        T t;

        t = (b - sqrt_d) / a;
        if (t >= ray.m_tmin && t < ray.m_tmax)
            return true;

        t = (b + sqrt_d) / a;
        if (t >= ray.m_tmin && t < ray.m_tmax)
            return true;
    }

    return false;
}

template <typename T>
inline bool intersect_sphere_unit_direction(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius)
{
    assert(is_normalized(ray.m_dir));

    const Vector<T, 3> v = center - ray.m_org;
    const T b = dot(ray.m_dir, v);

    const T d = square(b) - (dot(v, v) - square(radius));
    if (d >= T(0.0))
    {
        const T sqrt_d = std::sqrt(d);
        T t;

        t = b - sqrt_d;
        if (t >= ray.m_tmin && t < ray.m_tmax)
            return true;

        t = b + sqrt_d;
        if (t >= ray.m_tmin && t < ray.m_tmax)
            return true;
    }

    return false;
}

template <typename T>
inline bool intersect_sphere(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius,
    T&                      tmin)
{
    const T a = dot(ray.m_dir, ray.m_dir);
    assert(a > T(0.0));

    const Vector<T, 3> v = center - ray.m_org;
    const T b = dot(ray.m_dir, v);

    const T d = square(b) - a * (dot(v, v) - square(radius));
    if (d >= T(0.0))
    {
        const T sqrt_d = std::sqrt(d);
        T t;

        t = (b - sqrt_d) / a;
        if (t >= ray.m_tmin && t < ray.m_tmax)
        {
            tmin = t;
            return true;
        }

        t = (b + sqrt_d) / a;
        if (t >= ray.m_tmin && t < ray.m_tmax)
        {
            tmin = t;
            return true;
        }
    }

    return false;
}

template <typename T>
inline bool intersect_sphere_unit_direction(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius,
    T&                      tmin)
{
    assert(is_normalized(ray.m_dir));

    const Vector<T, 3> v = center - ray.m_org;
    const T b = dot(ray.m_dir, v);

    const T d = square(b) - (dot(v, v) - square(radius));
    if (d >= T(0.0))
    {
        const T sqrt_d = std::sqrt(d);
        T t;

        t = b - sqrt_d;
        if (t >= ray.m_tmin && t < ray.m_tmax)
        {
            tmin = t;
            return true;
        }

        t = b + sqrt_d;
        if (t >= ray.m_tmin && t < ray.m_tmax)
        {
            tmin = t;
            return true;
        }
    }

    return false;
}

template <typename T>
inline size_t intersect_sphere(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius,
    T                       t_out[2])
{
    size_t hit_count = 0;

    const T a = dot(ray.m_dir, ray.m_dir);
    assert(a > T(0.0));

    const Vector<T, 3> v = center - ray.m_org;
    const T b = dot(ray.m_dir, v);

    const T d = square(b) - a * (dot(v, v) - square(radius));
    if (d >= T(0.0))
    {
        const T sqrt_d = std::sqrt(d);
        T t;

        t = (b - sqrt_d) / a;
        if (t >= ray.m_tmin && t < ray.m_tmax)
            t_out[hit_count++] = t;

        t = (b + sqrt_d) / a;
        if (t >= ray.m_tmin && t < ray.m_tmax)
            t_out[hit_count++] = t;
    }

    return hit_count;
}

template <typename T>
inline size_t intersect_sphere_unit_direction(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     center,
    const T                 radius,
    T                       t_out[2])
{
    assert(is_normalized(ray.m_dir));

    size_t hit_count = 0;

    const Vector<T, 3> v = center - ray.m_org;
    const T b = dot(ray.m_dir, v);

    const T d = square(b) - (dot(v, v) - square(radius));
    if (d >= T(0.0))
    {
        const T sqrt_d = std::sqrt(d);
        T t;

        t = b - sqrt_d;
        if (t >= ray.m_tmin && t < ray.m_tmax)
            t_out[hit_count++] = t;

        t = b + sqrt_d;
        if (t >= ray.m_tmin && t < ray.m_tmax)
            t_out[hit_count++] = t;
    }

    return hit_count;
}

}   // namespace foundation
