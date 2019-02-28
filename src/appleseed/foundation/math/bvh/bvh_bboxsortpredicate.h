
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

// Standard headers.
#include <cstddef>

namespace foundation {
namespace bvh {

//
// A predicate to sort items according to the centroid of their bounding box in a given dimension.
//

template <typename AABBVector>
class BboxSortPredicate
{
  public:
    BboxSortPredicate(
        const AABBVector&   bboxes,
        const size_t        dim);

    bool operator()(const size_t lhs, const size_t rhs) const;

  private:
    typedef typename AABBVector::value_type AABBType;
    typedef typename AABBType::ValueType ValueType;

    const AABBVector&       m_bboxes;
    const size_t            m_dim;
};


//
// Same as BboxSortPredicate but provides a stable sort.
//

template <typename AABBVector>
class StableBboxSortPredicate
{
  public:
    StableBboxSortPredicate(
        const AABBVector&   bboxes,
        const size_t        dim);

    bool operator()(const size_t lhs, const size_t rhs) const;

  private:
    typedef typename AABBVector::value_type AABBType;
    typedef typename AABBType::ValueType ValueType;

    const AABBVector&       m_bboxes;
    const size_t            m_dim;
};


//
// BboxSortPredicate class implementation.
//

template <typename AABBVector>
inline BboxSortPredicate<AABBVector>::BboxSortPredicate(
    const AABBVector&       bboxes,
    const size_t            dim)
  : m_bboxes(bboxes)
  , m_dim(dim)
{
}

template <typename AABBVector>
inline bool BboxSortPredicate<AABBVector>::operator()(
    const size_t            lhs,
    const size_t            rhs) const
{
    const AABBType& lhs_bbox = m_bboxes[lhs];
    const AABBType& rhs_bbox = m_bboxes[rhs];

    const ValueType lhs_center = lhs_bbox.min[m_dim] + lhs_bbox.max[m_dim];
    const ValueType rhs_center = rhs_bbox.min[m_dim] + rhs_bbox.max[m_dim];

    return lhs_center < rhs_center;
}


//
// StableBboxSortPredicate class implementation.
//

template <typename AABBVector>
inline StableBboxSortPredicate<AABBVector>::StableBboxSortPredicate(
    const AABBVector&       bboxes,
    const size_t            dim)
  : m_bboxes(bboxes)
  , m_dim(dim)
{
}

template <typename AABBVector>
inline bool StableBboxSortPredicate<AABBVector>::operator()(
    const size_t            lhs,
    const size_t            rhs) const
{
    const AABBType& lhs_bbox = m_bboxes[lhs];
    const AABBType& rhs_bbox = m_bboxes[rhs];

    {
        const ValueType lhs_center = lhs_bbox.min[m_dim] + lhs_bbox.max[m_dim];
        const ValueType rhs_center = rhs_bbox.min[m_dim] + rhs_bbox.max[m_dim];

        if (lhs_center < rhs_center)
            return true;

        if (lhs_center > rhs_center)
            return false;
    }

    for (size_t i = 0; i < AABBVector::value_type::Dimension; ++i)
    {
        const ValueType lhs_center = lhs_bbox.min[i] + lhs_bbox.max[i];
        const ValueType rhs_center = rhs_bbox.min[i] + rhs_bbox.max[i];

        if (lhs_center < rhs_center)
            return true;

        if (lhs_center > rhs_center)
            return false;
    }

    return false;
}

}   // namespace bvh
}   // namespace foundation
