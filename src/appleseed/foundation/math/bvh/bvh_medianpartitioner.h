
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
        const size_t            max_leaf_size);

    // Partition a set of items into two distinct sets.
    size_t partition(
        std::vector<size_t>&    indices,
        std::vector<AABBType>&  bboxes,
        const size_t            begin,
        const size_t            end,
        const AABBType&         bbox) const;

  private:
    const size_t m_max_leaf_size;
};


//
// MedianPartitioner class implementation.
//

template <typename Tree>
inline MedianPartitioner<Tree>::MedianPartitioner(
    const size_t                max_leaf_size)
  : m_max_leaf_size(max_leaf_size)
{
}

template <typename Tree>
inline size_t MedianPartitioner<Tree>::partition(
    std::vector<size_t>&        indices,
    std::vector<AABBType>&      bboxes,
    const size_t                begin,
    const size_t                end,
    const AABBType&             bbox) const
{
    const size_t count = end - begin;
    assert(count > 1);

    // Don't split leaves containing less than a predefined number of items.
    if (count <= m_max_leaf_size)
        return end;

    // Sort the items according to their bounding boxes.
    BboxSortPredicate<AABBType> predicate(bboxes, max_index(bbox.extent()));
    std::sort(&indices[begin], &indices[begin] + count, predicate);

    // Split the items in two sets of roughly equal size.
    return (begin + end) / 2;
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_MEDIANPARTITIONER_H
