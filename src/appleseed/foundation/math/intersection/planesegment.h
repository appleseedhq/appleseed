
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
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// 3D ray-segment intersection functions.
//

// Clip a line segment against the negative half space of a plane.
// The plane itself is considered to belong to the negative half space.
// Returns false if the line segment was entirely clipped away.
template <typename T>
bool clip(
    const Vector<T, 4>& plane,
    Vector<T, 3>&       a,
    Vector<T, 3>&       b);
template <typename T>
bool clip(
    const Vector<T, 3>& normal,     // unit-length
    Vector<T, 3>&       a,
    Vector<T, 3>&       b);


//
// Implementation.
//

template <typename T>
inline bool clip(
    const Vector<T, 4>& plane,
    Vector<T, 3>&       a,
    Vector<T, 3>&       b)
{
    const T dot_an = plane[0] * a[0] + plane[1] * a[1] + plane[2] * a[2] + plane[3];
    const T dot_bn = plane[0] * b[0] + plane[1] * b[1] + plane[2] * b[2] + plane[3];

    if (dot_an * dot_bn > T(0.0))
    {
        // The segment is entirely on one side of the plane.
        return dot_an <= T(0.0);
    }

    if (dot_an == dot_bn)
    {
        // The segment is parallel to the plane.
        return dot_an <= T(0.0);
    }

    const T t = dot_an / (dot_an - dot_bn);
    const Vector<T, 3> p = a + t * (b - a);

    if (dot_an > T(0.0))
        a = p;
    else b = p;

    return true;
}

template <typename T>
inline bool clip(
    const Vector<T, 3>& normal,
    Vector<T, 3>&       a,
    Vector<T, 3>&       b)
{
    assert(is_normalized(normal));

    const T dot_an = dot(a, normal);
    const T dot_bn = dot(b, normal);

    if (dot_an * dot_bn > T(0.0))
    {
        // The segment is entirely on one side of the plane.
        return dot_an <= T(0.0);
    }

    if (dot_an == dot_bn)
    {
        // The segment is parallel to the plane.
        return dot_an <= T(0.0);
    }

    const T t = dot_an / (dot_an - dot_bn);
    const Vector<T, 3> p = a + t * (b - a);

    if (dot_an > T(0.0))
        a = p;
    else b = p;

    return true;
}

}   // namespace foundation
