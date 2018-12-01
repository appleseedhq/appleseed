
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
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

//
// A frustum defined by the intersection of three or more negative half spaces.
// Half spaces are defined by the normal vector to their plane. The planes
// themselves are considered to belong to the negative half spaces.
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
    assert(is_normalized(normal));

    m_planes[index] = Vector4d(normal.x, normal.y, normal.z, T(0.0));
}

template <typename T, size_t N>
inline const Vector<T, 4>& Frustum<T, N>::get_plane(const size_t index) const
{
    assert(index < N);

    return m_planes[index];
}

}   // namespace foundation
