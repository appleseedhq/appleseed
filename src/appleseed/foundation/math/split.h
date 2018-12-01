
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
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// An axis-aligned split plane.
//

template <typename T>
class Split
{
  public:
    // Value type.
    typedef T ValueType;

    // Split dimension and abscissa.
    size_t      m_dimension;                    // split dimension
    ValueType   m_abscissa;                     // split abscissa

    // Constructors.
    Split();                                    // leave the members uninitialized
    Split(
        const size_t        dimension,
        const ValueType     abscissa);

    // Create a middle split for a given bounding box.
    template <size_t N>
    static Split middle(const AABB<T, N>& bbox);
};

// Utility function to split a bounding box.
template <typename T, size_t N>
void split_bbox(
    const AABB<T, N>&       bbox,
    const Split<T>&         split,
    AABB<T, N>&             left,
    AABB<T, N>&             right);


//
// Split class implementation.
//

template <typename T>
inline Split<T>::Split()
{
}

template <typename T>
inline Split<T>::Split(
    const size_t            dimension,
    const ValueType         abscissa)
  : m_dimension(dimension)
  , m_abscissa(abscissa)
{
}

template <typename T>
template <size_t N>
inline Split<T> Split<T>::middle(const AABB<T, N>& bbox)
{
    Split<T> split;
    split.m_dimension = max_index(bbox.extent());
    split.m_abscissa = bbox.center(split.m_dimension);
    return split;
}

template <typename T, size_t N>
inline void split_bbox(
    const AABB<T, N>&       bbox,
    const Split<T>&         split,
    AABB<T, N>&             left,
    AABB<T, N>&             right)
{
    left = bbox;
    left.max[split.m_dimension] = split.m_abscissa;

    right = bbox;
    right.min[split.m_dimension] = split.m_abscissa;
}

}   // namespace foundation
