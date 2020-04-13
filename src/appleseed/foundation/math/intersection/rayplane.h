
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
#include "foundation/math/vector.h"

namespace foundation
{

//
// 3D ray-plane intersection functions.
//

template <typename T>
bool intersect_ray_plane(
    const Vector<T, 3>&     origin,
    const Vector<T, 3>&     dir,
    const Vector<T, 3>&     point,
    const Vector<T, 3>&     normal,
    T&                      t);

template <typename T>
T intersect(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     point,
    const Vector<T, 3>&     normal);

template <typename T>
bool intersect(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     point,
    const Vector<T, 3>&     normal,
    T&                      t);


//
// 3D ray-plane intersection functions implementation.
//

template <typename T>
inline bool intersect_ray_plane(
    const Vector<T, 3>&     origin,
    const Vector<T, 3>&     dir,
    const Vector<T, 3>&     point,
    const Vector<T, 3>&     normal,
    T&                      t)
{
    const Vector<T, 3> u = point - origin;
    const T denom = dot(dir, normal);

    if (denom == T(0.0))
        return false;

    t = dot(u, normal) / denom;
    return true;
}

template <typename T>
inline T intersect(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     point,
    const Vector<T, 3>&     normal)
{
    const Vector<T, 3> u = point - ray.m_org;
    return dot(u, normal) / dot(ray.m_dir, normal);
}

template <typename T>
inline bool intersect(
    const Ray<T, 3>&        ray,
    const Vector<T, 3>&     point,
    const Vector<T, 3>&     normal,
    T&                      t)
{
    return intersect_ray_plane(ray.m_org, ray.m_dir, point, normal, t);
}

}   // namespace foundation
