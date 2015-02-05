
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
    typedef Vector<T, 3> VectorType;

    // Set a given plane of the frustum. n must be normalized.
    void set_plane(const size_t plane, const VectorType& n);

    // Get a given plane of the frustum.
    const VectorType& get_plane(const size_t plane) const;

    // Clip a line segment against a plane. n must be normalized.
    // Returns false if the line segment was entirely clipped away.
    static bool clip(
        const VectorType&   n,
        VectorType&         a,
        VectorType&         b);

    // Clip a line segment against the frustum.
    // Returns false if the line segment was entirely clipped away.
    bool clip(VectorType& a, VectorType& b) const;

  private:
    VectorType  m_planes[N];
};


//
// Frustum class implementation.
//

template <typename T, size_t N>
inline void Frustum<T, N>::set_plane(const size_t plane, const VectorType& n)
{
    assert(plane < N);
    assert(is_normalized(n));

    m_planes[plane] = n;
}

template <typename T, size_t N>
inline const Vector<T, 3>& Frustum<T, N>::get_plane(const size_t plane) const
{
    assert(plane < N);

    return m_planes[plane];
}

template <typename T, size_t N>
inline bool Frustum<T, N>::clip(
    const VectorType&   n,
    VectorType&         a,
    VectorType&         b)
{
    assert(is_normalized(n));

    const ValueType dot_an = dot(a, n);
    const ValueType dot_bn = dot(b, n);

    if (dot_an > ValueType(0.0) && dot_bn > ValueType(0.0))
        return false;

    if (dot_an <= ValueType(0.0) && dot_bn <= ValueType(0.0))
        return true;

    if (dot_an == dot_bn)
        return dot_an <= ValueType(0.0);

    const ValueType t = dot_an / (dot_an - dot_bn);
    const VectorType hit = a + t * (b - a);

    if (dot_an > ValueType(0.0))
        a = hit;
    else b = hit;

    return true;
}

template <typename T, size_t N>
inline bool Frustum<T, N>::clip(VectorType& a, VectorType& b) const
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!clip(m_planes[i], a, b))
            return false;
    }

    return true;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FRUSTUM_H
