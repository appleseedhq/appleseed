
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_SAHPARTITIONER_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_SAHPARTITIONER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/permutation.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <vector>

namespace foundation {
namespace bvh {

template <typename Tree>
class SAHPartitioner
  : public NonCopyable
{
  public:
    // Types.
    typedef typename Tree::ValueType ValueType;
    typedef typename Tree::AABBType AABBType;
    typedef typename Tree::NodeType NodeType;
    typedef typename Tree::ItemType ItemType;
    typedef Tree TreeType;

    // Partition a set of items into two distinct sets.
    // Return end if the set is not to be partitioned.
    size_t partition(
        std::vector<ItemType>&  items,
        std::vector<AABBType>&  bboxes,
        const size_t            begin,
        const size_t            end,
        const AABBType&         bbox)
    {
        const size_t count = end - begin;
        assert(count > 1);

        // Ensure that sufficient memory is allocated for the working arrays.
        ensure_minimum_size(m_indices, count);
        ensure_minimum_size(m_left_bboxes, count);
        ensure_minimum_size(m_temp_items, count);
        ensure_minimum_size(m_temp_bboxes, count);

        // Create the set of indices.
        for (size_t i = 0; i < count; ++i)
            m_indices[i] = i;

        ValueType best_split_cost = std::numeric_limits<ValueType>::max();
        size_t best_split_dim = 0;
        size_t best_split_pivot = 0;
        AABBType group_bbox;

        for (size_t dim = 0; dim < Tree::Dimension; ++dim)
        {
            // Sort the items according to their bounding boxes.
            BboxSortPredicate predicate(bboxes, begin, dim);
            std::sort(&m_indices[0], &m_indices[0] + count, predicate);

            // Left-to-right sweep to accumulate bounding boxes.
            group_bbox.invalidate();
            for (size_t i = 0; i < count; ++i)
            {
                group_bbox.insert(bboxes[begin + m_indices[i]]);
                m_left_bboxes[i] = group_bbox;
            }

            // Right-to-left sweep to accumulate bounding boxes and evaluate SAH.
            group_bbox.invalidate();
            for (size_t i = count - 1; i > 0; --i)
            {
                // Get left and right bounding boxes.
                const AABBType& left_bbox = m_left_bboxes[i - 1];
                group_bbox.insert(bboxes[begin + m_indices[i]]);

                // Compute the cost of this partition.
                const ValueType left_cost = left_bbox.half_surface_area() * i;
                const ValueType right_cost = group_bbox.half_surface_area() * (count - i);
                const ValueType split_cost = left_cost + right_cost;

                // Keep track of the partition with the lowest cost.
                if (best_split_cost > split_cost)
                {
                    best_split_cost = split_cost;
                    best_split_dim = dim;
                    best_split_pivot = i;
                }
            }
        }

        // Just split in half if the cost of the best partition is too high.
        const ValueType leaf_cost = bbox.half_surface_area() * count;
        if (best_split_cost >= leaf_cost)
            return (begin + end) / 2;

        // Sort the indices according to the item bounding boxes.
        BboxSortPredicate predicate(bboxes, begin, best_split_dim);
        std::sort(&m_indices[0], &m_indices[0] + count, predicate);

        // Reorder the items.
        small_item_reorder(&items[begin], &m_temp_items[0], &m_indices[0], count);
        small_item_reorder(&bboxes[begin], &m_temp_bboxes[0], &m_indices[0], count);

        assert(begin + best_split_pivot < end);
        return begin + best_split_pivot;
    }

  private:
    class BboxSortPredicate
    {
      public:
        BboxSortPredicate(
            const std::vector<AABBType>&    bboxes,
            const size_t                    begin,
            const size_t                    dim)
          : m_bboxes(bboxes)
          , m_begin(begin)
          , m_dim(dim)
        {
        }

        bool operator()(const size_t lhs, const size_t rhs) const
        {
            return
                  m_bboxes[m_begin + lhs].min[m_dim] + m_bboxes[m_begin + lhs].max[m_dim]
                < m_bboxes[m_begin + rhs].min[m_dim] + m_bboxes[m_begin + rhs].max[m_dim];
        }

      private:
        const std::vector<AABBType>&        m_bboxes;
        const size_t                        m_begin;
        const size_t                        m_dim;
    };

    std::vector<size_t>     m_indices;
    std::vector<AABBType>   m_left_bboxes;
    std::vector<ItemType>   m_temp_items;
    std::vector<AABBType>   m_temp_bboxes;
};

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_SAHPARTITIONER_H
