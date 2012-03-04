
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_BBOXSORTPREDICATE_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_BBOXSORTPREDICATE_H

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
    return m_bboxes[lhs].center(m_dim) < m_bboxes[rhs].center(m_dim);
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
    for (size_t i = 0; i < AABBVector::value_type::Dimension; ++i)
    {
        const size_t d = (m_dim + i) % AABBVector::value_type::Dimension;

        if (m_bboxes[lhs].center(d) < m_bboxes[rhs].center(d))
            return true;

        if (m_bboxes[lhs].center(d) > m_bboxes[rhs].center(d))
            return false;
    }

    return false;
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_BBOXSORTPREDICATE_H
