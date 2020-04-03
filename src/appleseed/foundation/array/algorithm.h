
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/array/applyvisitor.h"
#include "foundation/array/array.h"
#include "foundation/array/arrayref.h"
#include "foundation/array/arrayview.h"
#include "foundation/array/exception.h"
#include "foundation/math/aabb.h"

namespace foundation
{
namespace detail
{

template <typename RandomAccessIter>
class CopyIndexedVisitor
{
  public:
    CopyIndexedVisitor(
        const Array&              src,
        const RandomAccessIter    first_index,
        const RandomAccessIter    last_index)
      : m_src(src)
      , m_first_index(first_index)
      , m_last_index(last_index)
    {
    }

    template <typename T>
    void operator()(ArrayRef<T>& dst)
    {
        const ArrayView<T> src(m_src);

        for (RandomAccessIter it = m_first_index; it != m_last_index; ++it)
            dst.push_back(src[*it]);
    }

  private:
    const Array&              m_src;
    const RandomAccessIter    m_first_index;
    const RandomAccessIter    m_last_index;
};

}       // namespace detail

//
// Return a new array, where each element is taken
// from src as specified by a range of indices:
// new_array[i] = src[indices[i]];
//

template <typename RandomAccessIter>
Array copy_indexed(
    const Array&        src,
    RandomAccessIter    begin_index,
    RandomAccessIter    end_index)
{
    Array dst(src.type());

    if (begin_index == end_index)
        return dst;

    dst.reserve(end_index - begin_index);

    detail::CopyIndexedVisitor<RandomAccessIter> v(src, begin_index, end_index);
    apply_visitor(dst, v);

    return dst;
}


//
// If the array contains integers, convert the array in place
// to the smallest type that can represent the array elements,
// otherwise leave the array unchanged.
//

void convert_to_smallest_type(Array& array);


//
// Compute the bounding box of an array of points.
//

AABB3f compute_bounding_box(const Array& vertices);
AABB3f compute_bounding_box(const Array& vertices, const AABB3f& initial_bbox);

}       // namespace foundation
