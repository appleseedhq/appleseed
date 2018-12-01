
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
#include "foundation/math/bvh/bvh_partitionerbase.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>
#include <vector>

namespace foundation {
namespace bvh {

//
// A BVH partitioner based on the Surface Area Heuristic (SAH).
//

template <typename AABBVector>
class SAHPartitioner
  : public PartitionerBase<AABBVector>
{
  public:
    typedef AABBVector AABBVectorType;
    typedef typename AABBVectorType::value_type AABBType;
    typedef typename AABBType::ValueType ValueType;

    // Constructor.
    SAHPartitioner(
        const AABBVectorType&   bboxes,
        const size_t            max_leaf_size = 1,
        const ValueType         interior_node_traversal_cost = ValueType(1.0),
        const ValueType         item_intersection_cost = ValueType(1.0));

    // Partition a set of items into two distinct sets.
    size_t partition(
        const size_t            begin,
        const size_t            end,
        const AABBType&         bbox);

  private:
    static const size_t Dimension = AABBType::Dimension;

    const size_t                m_max_leaf_size;
    const ValueType             m_interior_node_traversal_cost;
    const ValueType             m_item_intersection_cost;
    std::vector<ValueType>      m_left_areas;
};


//
// SAHPartitioner class implementation.
//

template <typename AABBVector>
inline SAHPartitioner<AABBVector>::SAHPartitioner(
    const AABBVectorType&       bboxes,
    const size_t                max_leaf_size,
    const ValueType             interior_node_traversal_cost,
    const ValueType             item_intersection_cost)
  : PartitionerBase<AABBVectorType>(bboxes)
  , m_max_leaf_size(max_leaf_size)
  , m_interior_node_traversal_cost(interior_node_traversal_cost)
  , m_item_intersection_cost(item_intersection_cost)
  , m_left_areas(bboxes.size() > 1 ? bboxes.size() - 1 : 0)
{
}

template <typename AABBVector>
size_t SAHPartitioner<AABBVector>::partition(
    const size_t                begin,
    const size_t                end,
    const AABBType&             bbox)
{
    // Don't split leaves containing only degenerate triangles.
    if (bbox.rank() < Dimension - 1)
        return end;

    const size_t count = end - begin;
    assert(count > 1);

    // Don't split leaves containing less than a predefined number of items.
    if (count <= m_max_leaf_size)
        return end;

    ValueType best_split_cost = std::numeric_limits<ValueType>::max();
    size_t best_split_dim = 0;
    size_t best_split_pivot = 0;

    for (size_t d = 0; d < Dimension; ++d)
    {
        const AABBVectorType& bboxes = PartitionerBase<AABBVector>::m_bboxes;
        const std::vector<size_t>& indices = PartitionerBase<AABBVector>::m_indices[d];

        AABBType bbox_accumulator;

        // Left-to-right sweep to accumulate bounding boxes and compute their surface area.
        bbox_accumulator.invalidate();
        for (size_t i = 0; i < count - 1; ++i)
        {
            bbox_accumulator.insert(bboxes[indices[begin + i]]);
            m_left_areas[i] = half_surface_area(bbox_accumulator);
        }

        // Right-to-left sweep to accumulate bounding boxes, compute their surface area find the best partition.
        bbox_accumulator.invalidate();
        for (size_t i = count - 1; i > 0; --i)
        {
            // Compute right bounding box.
            bbox_accumulator.insert(bboxes[indices[begin + i]]);

            // Compute the cost of this partition.
            const ValueType left_cost = m_left_areas[i - 1] * i;
            const ValueType right_cost = half_surface_area(bbox_accumulator) * (count - i);
            const ValueType split_cost = left_cost + right_cost;

            // Keep track of the partition with the lowest cost.
            if (best_split_cost > split_cost)
            {
                best_split_cost = split_cost;
                best_split_dim = d;
                best_split_pivot = i;
            }
        }
    }

    // Don't split if it's cheaper to make a leaf.
    const ValueType split_cost =
        m_interior_node_traversal_cost +
        best_split_cost / half_surface_area(bbox) * m_item_intersection_cost;
    const ValueType leaf_cost = count * m_item_intersection_cost;
    if (leaf_cost <= split_cost)
        return end;

    const size_t pivot = begin + best_split_pivot;
    assert(pivot < end);

    PartitionerBase<AABBVector>::sort_indices(best_split_dim, begin, end, pivot);

    return pivot;
}

}   // namespace bvh
}   // namespace foundation
