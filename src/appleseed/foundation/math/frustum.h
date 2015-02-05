
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_FRUSTUM_H
#define APPLESEED_FOUNDATION_MATH_FRUSTUM_H

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

//
// A frustum defined by the intersection of three or more negative half spaces.
// Half spaces are defined by the normal vector to their plane. The points
// inside the planes are considered to be part of the negative half spaces.
//

template <typename T, size_t N>
class Frustum
{
  public:
    typedef T ValueType;

    // Set a given plane of the frustum.
    void set_plane(
        const size_t        index,
        const Vector<T, 4>& plane);
    void set_plane(
        const size_t        index,
        const Vector<T, 3>& normal);    // unit-length

    // Get a given plane of the frustum.
    const Vector<T, 4>& get_plane(const size_t index) const;

    // Clip a line segment against the frustum.
    // Returns false if the line segment was entirely clipped away.
    bool clip(Vector<T, 3>& a, Vector<T, 3>& b) const;

    // Clip a line segment against a plane.
    // Returns false if the line segment was entirely clipped away.
    static bool clip(
        const Vector<T, 4>& plane,
        Vector<T, 3>&       a,
        Vector<T, 3>&       b);
    static bool clip(
        const Vector<T, 3>& normal,     // unit-length
        Vector<T, 3>&       a,
        Vector<T, 3>&       b);

  private:
    Vector<T, 4> m_planes[N];
};


//
// Frustum class implementation.
//

template <typename T, size_t N>
inline void Frustum<T, N>::set_plane(
    const size_t        index,
    const Vector<T, 4>& plane)
{
    assert(index < N);
    m_planes[index] = plane;
}

template <typename T, size_t N>
inline void Frustum<T, N>::set_plane(
    const size_t        index,
    const Vector<T, 3>& normal)
{
    assert(index < N);
    m_planes[index] = Vector4d(normal.x, normal.y, normal.z, T(0.0));
}

template <typename T, size_t N>
inline const Vector<T, 4>& Frustum<T, N>::get_plane(const size_t index) const
{
    assert(index < N);
    return m_planes[index];
}

template <typename T, size_t N>
inline bool Frustum<T, N>::clip(Vector<T, 3>& a, Vector<T, 3>& b) const
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!clip(m_planes[i], a, b))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool Frustum<T, N>::clip(
    const Vector<T, 4>& plane,
    Vector<T, 3>&       a,
    Vector<T, 3>&       b)
{
    const Vector<T, 3> n(plane[0], plane[1], plane[2]);

    const T dot_an = dot(a, n) + plane[3];
    const T dot_bn = dot(b, n) + plane[3];

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

template <typename T, size_t N>
inline bool Frustum<T, N>::clip(
    const Vector<T, 3>& normal,
    Vector<T, 3>&       a,
    Vector<T, 3>&       b)
{
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

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FRUSTUM_H
