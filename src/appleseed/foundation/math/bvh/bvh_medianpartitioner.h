
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_MEDIANPARTITIONER_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_MEDIANPARTITIONER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/bvh/bvh_bboxsortpredicate.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <vector>

namespace foundation {
namespace bvh {

//
// A BVH partitioner that use median splitting.
//

template <typename Tree>
class MedianPartitioner
  : public NonCopyable
{
  public:
    // Types.
    typedef typename Tree::ValueType ValueType;
    typedef typename Tree::AABBType AABBType;

    // Constructor.
    explicit MedianPartitioner(
        const size_t                    max_leaf_size);

    // Initialize the partitioner for a given number of items.
    void initialize(const size_t size);

    // Compute the bounding box of a given set of items.
    AABBType compute_bbox(
        const std::vector<AABBType>&    bboxes,
        const size_t                    begin,
        const size_t                    end) const;

    // Partition a set of items into two distinct sets.
    size_t partition(
        const std::vector<AABBType>&    bboxes,
        const size_t                    begin,
        const size_t                    end,
        const AABBType&                 bbox);

    // Return the items ordering.
    const std::vector<size_t>& get_item_ordering() const;

  private:
    const size_t            m_max_leaf_size;
    std::vector<size_t>     m_indices;
};


//
// MedianPartitioner class implementation.
//

template <typename Tree>
inline MedianPartitioner<Tree>::MedianPartitioner(
    const size_t                        max_leaf_size)
  : m_max_leaf_size(max_leaf_size)
{
}

template <typename Tree>
void MedianPartitioner<Tree>::initialize(const size_t size)
{
    m_indices.resize(size);

    for (size_t i = 0; i < size; ++i)
        m_indices[i] = i;
}

template <typename Tree>
inline typename Tree::AABBType MedianPartitioner<Tree>::compute_bbox(
    const std::vector<AABBType>&        bboxes,
    const size_t                        begin,
    const size_t                        end) const
{
    AABBType bbox;
    bbox.invalidate();

    for (size_t i = begin; i < end; ++i)
        bbox.insert(bboxes[m_indices[i]]);

    return bbox;
}

template <typename Tree>
inline size_t MedianPartitioner<Tree>::partition(
    const std::vector<AABBType>&        bboxes,
    const size_t                        begin,
    const size_t                        end,
    const AABBType&                     bbox)
{
    const size_t count = end - begin;
    assert(count > 1);

    // Don't split leaves containing less than a predefined number of items.
    if (count <= m_max_leaf_size)
        return end;

    // Sort the items according to their bounding boxes.
    BboxSortPredicate<AABBType> predicate(bboxes, max_index(bbox.extent()));
    std::sort(&m_indices[begin], &m_indices[begin] + count, predicate);

    // Split the items in two sets of roughly equal size.
    return (begin + end) / 2;
}

template <typename Tree>
inline const std::vector<size_t>& MedianPartitioner<Tree>::get_item_ordering() const
{
    return m_indices;
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_MEDIANPARTITIONER_H
