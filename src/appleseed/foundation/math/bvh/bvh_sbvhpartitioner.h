
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/bvh/bvh_bboxsortpredicate.h"
#include "foundation/math/scalar.h"
#include "foundation/math/split.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

namespace foundation {
namespace bvh {

//
// A partitioner implementing the SBVH algorithm (use with foundation::bvh::SpatialBuilder).
//
// Reference:
//
//   http://www.nvidia.com/docs/IO/77714/sbvh.pdf
//
// The ItemHandler class must conform to the following prototype:
//
//      class ItemHandler
//        : public foundation::NonCopyable
//      {
//        public:
//          ValueType get_bbox_grow_eps() const;
//
//          AABBType clip(
//              const size_t        item_index,
//              const size_t        dimension,
//              const ValueType     slab_min,
//              const ValueType     slab_max) const;
//
//          bool intersect(
//              const size_t        item_index,
//              const AABBType&     bbox) const;
//      };
//

// When defined, additional costly correctness checks are enabled (only in Debug).
#undef FOUNDATION_SBVH_DEEPCHECK

template <typename ItemHandler, typename AABBVector>
class SBVHPartitioner
  : public foundation::NonCopyable
{
  public:
    typedef AABBVector AABBVectorType;
    typedef typename AABBVectorType::value_type AABBType;
    typedef typename AABBType::ValueType ValueType;
    static const size_t Dimension = AABBType::Dimension;

    struct LeafType
    {
        std::vector<size_t> m_indices[Dimension];

        size_t size() const
        {
            return m_indices[0].size();
        }
    };

    // Constructor.
    SBVHPartitioner(
        ItemHandler&                item_handler,
        const AABBVectorType&       bboxes,
        const size_t                max_leaf_size = 1,
        const size_t                bin_count = 64,
        const ValueType             interior_node_traversal_cost = ValueType(1.0),
        const ValueType             item_intersection_cost = ValueType(1.0));

    // Create the root leaf of the tree. Ownership of the leaf is passed to the caller.
    LeafType* create_root_leaf() const;

    // Compute the bounding box of a given leaf.
    AABBType compute_leaf_bbox(const LeafType& leaf) const;

    // Split a leaf. Return true if the split should be split or false if it should be kept unsplit.
    bool split(
        LeafType&                   leaf,
        const AABBType&             leaf_bbox,
        LeafType&                   left_leaf,
        AABBType&                   left_leaf_bbox,
        LeafType&                   right_leaf,
        AABBType&                   right_leaf_bbox);

    // Store a leaf. Return the index of the first stored item.
    size_t store(const LeafType& leaf);

    // Return the items ordering.
    const std::vector<size_t>& get_item_ordering() const;

    // Split counters.
    size_t get_spatial_split_count() const;
    size_t get_object_split_count() const;

  private:
    typedef Split<ValueType> SplitType;

    struct Bin
    {
        AABBType    m_bin_bbox;         // bbox of this bin
        AABBType    m_left_bbox;        // bbox of all the bins to the left of this one
        size_t      m_entry_counter;    // number of items that begin in this bin
        size_t      m_exit_counter;     // number of items that end in this bin
    };

    ItemHandler&                    m_item_handler;
    const AABBVectorType&           m_bboxes;
    const size_t                    m_max_leaf_size;
    const size_t                    m_bin_count;
    const ValueType                 m_rcp_bin_count;
    const ValueType                 m_interior_node_traversal_cost;
    const ValueType                 m_item_intersection_cost;

    ValueType                       m_root_bbox_rcp_sa;
    std::vector<AABBType>           m_left_bboxes;
    std::vector<Bin>                m_bins;
    std::vector<std::uint8_t>       m_tags;
    std::vector<size_t>             m_final_indices;

    size_t                          m_spatial_split_count;
    size_t                          m_object_split_count;

    void compute_root_bbox_surface_area();

    ValueType compute_final_split_cost(
        const AABBType&             bbox,
        const double                cost) const;

    // Find the best object split for a given set of items.
    void find_object_split(
        LeafType&                   leaf,
        const AABBType&             leaf_bbox,
        AABBType&                   left_leaf_bbox,
        AABBType&                   right_leaf_bbox,
        size_t&                     best_split_dim,
        size_t&                     best_split_pivot,
        ValueType&                  best_split_cost);

    // Find the best spatial split for a given set of items.
    void find_spatial_split(
        const LeafType&             leaf,
        const AABBType&             leaf_bbox,
        AABBType&                   left_leaf_bbox,
        AABBType&                   right_leaf_bbox,
        SplitType&                  best_split,
        ValueType&                  best_split_cost);

    // Sort a set of items into two subsets according to a given object split.
    void object_sort(
        LeafType&                   leaf,
        const size_t                split_dim,
        const size_t                split_pivot,
        const AABBType&             left_leaf_bbox,
        const AABBType&             right_leaf_bbox,
        LeafType&                   left_leaf,
        LeafType&                   right_leaf);

    // Sort a set of items into two subsets according to a given spatial split.
    void spatial_sort(
        const LeafType&             leaf,
        const SplitType&            split,
        const AABBType&             left_leaf_bbox,
        const AABBType&             right_leaf_bbox,
        LeafType&                   left_leaf,
        LeafType&                   right_leaf) const;
};


//
// SBVHPartitioner class implementation.
//

template <typename ItemHandler, typename AABBVector>
SBVHPartitioner<ItemHandler, AABBVector>::SBVHPartitioner(
    ItemHandler&                    item_handler,
    const AABBVectorType&           bboxes,
    const size_t                    max_leaf_size,
    const size_t                    bin_count,
    const ValueType                 interior_node_traversal_cost,
    const ValueType                 item_intersection_cost)
  : m_item_handler(item_handler)
  , m_bboxes(bboxes)
  , m_max_leaf_size(max_leaf_size)
  , m_bin_count(bin_count)
  , m_rcp_bin_count(ValueType(1.0) / bin_count)
  , m_interior_node_traversal_cost(interior_node_traversal_cost)
  , m_item_intersection_cost(item_intersection_cost)
  , m_left_bboxes(bboxes.size() > 1 ? bboxes.size() - 1 : 0)
  , m_bins(bin_count)
  , m_tags(bboxes.size())
  , m_spatial_split_count(0)
  , m_object_split_count(0)
{
    compute_root_bbox_surface_area();
}

template <typename ItemHandler, typename AABBVector>
typename SBVHPartitioner<ItemHandler, AABBVector>::LeafType* SBVHPartitioner<ItemHandler, AABBVector>::create_root_leaf() const
{
    LeafType* leaf = new LeafType();

    const size_t size = m_bboxes.size();

    for (size_t d = 0; d < Dimension; ++d)
    {
        std::vector<size_t>& indices = leaf->m_indices[d];

        // Identity ordering.
        indices.resize(size);
        for (size_t i = 0; i < size; ++i)
            indices[i] = i;

        // Sort the items according to their bounding boxes.
        StableBboxSortPredicate<AABBVectorType> predicate(m_bboxes, d);
        std::sort(indices.begin(), indices.end(), predicate);
    }

    return leaf;
}

template <typename ItemHandler, typename AABBVector>
typename AABBVector::value_type SBVHPartitioner<ItemHandler, AABBVector>::compute_leaf_bbox(const LeafType& leaf) const
{
    AABBType bbox;
    bbox.invalidate();

    const std::vector<size_t>& indices = leaf.m_indices[0];
    const size_t size = indices.size();

    for (size_t i = 0; i < size; ++i)
        bbox.insert(m_bboxes[indices[i]]);

    return bbox;
}

template <typename ItemHandler, typename AABBVector>
bool SBVHPartitioner<ItemHandler, AABBVector>::split(
    LeafType&                       leaf,
    const AABBType&                 leaf_bbox,
    LeafType&                       left_leaf,
    AABBType&                       left_leaf_bbox,
    LeafType&                       right_leaf,
    AABBType&                       right_leaf_bbox)
{
    assert(!leaf_bbox.is_valid() || leaf_bbox.rank() >= Dimension - 1);

#ifdef FOUNDATION_SBVH_DEEPCHECK
    // Make sure every item intersects the leaf it belongs to.
    for (size_t i = 0; i < leaf.m_indices[0].size(); ++i)
    {
        const size_t item_index = leaf.m_indices[0][i];
        const AABBType& item_bbox = m_bboxes[item_index];
        assert(AABBType::overlap(item_bbox, leaf_bbox));
    }
#endif

    // Don't split leaves with less than two items.
    if (leaf.m_indices[0].size() < 2)
        return false;

    // Find the best object split.
    AABBType object_split_left_bbox;
    AABBType object_split_right_bbox;
    size_t object_split_dim;
    size_t object_split_pivot;
    ValueType object_split_cost = std::numeric_limits<ValueType>::max();
    find_object_split(
        leaf,
        leaf_bbox,
        object_split_left_bbox,
        object_split_right_bbox,
        object_split_dim,
        object_split_pivot,
        object_split_cost);

    // Don't try to find a spatial split if the object split is good enough.
    bool do_find_spatial_split = true;
    if (object_split_cost < std::numeric_limits<ValueType>::max())
    {
        const AABBType overlap_bbox =
            AABBType::intersect(object_split_left_bbox, object_split_right_bbox);
        if (!overlap_bbox.is_valid())
            do_find_spatial_split = false;
        else
        {
            const ValueType overlap_sa = surface_area(overlap_bbox);
            const ValueType overlap_factor = overlap_sa * m_root_bbox_rcp_sa;
            const ValueType Alpha = ValueType(1.0e-4);
            if (overlap_factor <= Alpha)
                do_find_spatial_split = false;
        }
    }

    // Find the best spatial split.
    AABBType spatial_split_left_bbox;
    AABBType spatial_split_right_bbox;
    SplitType spatial_split;
    ValueType spatial_split_cost = std::numeric_limits<ValueType>::max();
    if (do_find_spatial_split)
    {
        find_spatial_split(
            leaf,
            leaf_bbox,
            spatial_split_left_bbox,
            spatial_split_right_bbox,
            spatial_split,
            spatial_split_cost);
    }

    // Compute the cost of keeping the leaf unsplit.
    const ValueType leaf_cost = leaf.size() * m_item_intersection_cost;

    // Select the cheapest option.
    if (leaf_cost <= object_split_cost && leaf_cost <= spatial_split_cost)
    {
        // Don't split, make a leaf.
        return false;
    }
    else if (object_split_cost <= spatial_split_cost)
    {
        // Perform the object split.
        left_leaf_bbox = object_split_left_bbox;
        right_leaf_bbox = object_split_right_bbox;
        object_sort(
            leaf,
            object_split_dim,
            object_split_pivot,
            left_leaf_bbox,
            right_leaf_bbox,
            left_leaf,
            right_leaf);
        ++m_object_split_count;
        return true;
    }
    else
    {
        // Perform the spatial split.
        left_leaf_bbox = spatial_split_left_bbox;
        right_leaf_bbox = spatial_split_right_bbox;
        spatial_sort(
            leaf,
            spatial_split,
            left_leaf_bbox,
            right_leaf_bbox,
            left_leaf,
            right_leaf);
        ++m_spatial_split_count;
        return true;
    }
}

template <typename ItemHandler, typename AABBVector>
void SBVHPartitioner<ItemHandler, AABBVector>::compute_root_bbox_surface_area()
{
    AABBType root_bbox;
    root_bbox.invalidate();

    const size_t size = m_bboxes.size();

    for (size_t i = 0; i < size; ++i)
        root_bbox.insert(m_bboxes[i]);

    m_root_bbox_rcp_sa =
        size > 0
            ? ValueType(1.0) / surface_area(root_bbox)
            : ValueType(0.0);
}

template <typename ItemHandler, typename AABBVector>
inline typename AABBVector::value_type::ValueType SBVHPartitioner<ItemHandler, AABBVector>::compute_final_split_cost(
    const AABBType&                 bbox,
    const double                    cost) const
{
    assert(bbox.is_valid());

    if (!(cost < std::numeric_limits<ValueType>::max()))
        return cost;

    const ValueType bbox_half_surface_area = half_surface_area(bbox);

    assert(bbox_half_surface_area > ValueType(0.0));

    return
        m_interior_node_traversal_cost +
        m_item_intersection_cost * (cost / bbox_half_surface_area);
}

template <typename ItemHandler, typename AABBVector>
void SBVHPartitioner<ItemHandler, AABBVector>::find_object_split(
    LeafType&                       leaf,
    const AABBType&                 leaf_bbox,
    AABBType&                       left_leaf_bbox,
    AABBType&                       right_leaf_bbox,
    size_t&                         best_split_dim,
    size_t&                         best_split_pivot,
    ValueType&                      best_split_cost)
{
    for (size_t d = 0; d < Dimension; ++d)
    {
        const std::vector<size_t>& indices = leaf.m_indices[d];
        const size_t item_count = indices.size();

        AABBType bbox_accumulator;

        // Left-to-right sweep to accumulate bounding boxes and compute their surface area.
        bbox_accumulator.invalidate();
        for (size_t i = 0; i < item_count - 1; ++i)
        {
            const size_t item_index = indices[i];
            const AABBType& item_bbox = m_bboxes[item_index];
            const AABBType clipped_item_bbox = AABBType::intersect(item_bbox, leaf_bbox);
            assert(clipped_item_bbox.is_valid());
            bbox_accumulator.insert(clipped_item_bbox);
            m_left_bboxes[i] = bbox_accumulator;
        }

        // Right-to-left sweep to accumulate bounding boxes, compute their surface area find the best partition.
        bbox_accumulator.invalidate();
        for (size_t i = item_count - 1; i > 0; --i)
        {
            // Compute right bounding box.
            const size_t item_index = indices[i];
            const AABBType& item_bbox = m_bboxes[item_index];
            const AABBType clipped_item_bbox = AABBType::intersect(item_bbox, leaf_bbox);
            assert(clipped_item_bbox.is_valid());
            bbox_accumulator.insert(clipped_item_bbox);

            // Compute the cost of this partition.
            const ValueType left_cost = half_surface_area(m_left_bboxes[i - 1]) * i;
            const ValueType right_cost = half_surface_area(bbox_accumulator) * (item_count - i);
            const ValueType split_cost = left_cost + right_cost;

            // Keep track of the partition with the lowest cost.
            if (best_split_cost > split_cost)
            {
                best_split_cost = split_cost;
                best_split_dim = d;
                best_split_pivot = i;
                left_leaf_bbox = m_left_bboxes[i - 1];
                right_leaf_bbox = bbox_accumulator;
            }
        }
    }

    best_split_cost = compute_final_split_cost(leaf_bbox, best_split_cost);
}

template <typename ItemHandler, typename AABBVector>
void SBVHPartitioner<ItemHandler, AABBVector>::find_spatial_split(
    const LeafType&                 leaf,
    const AABBType&                 leaf_bbox,
    AABBType&                       left_leaf_bbox,
    AABBType&                       right_leaf_bbox,
    SplitType&                      best_split,
    ValueType&                      best_split_cost)
{
    for (size_t d = 0; d < Dimension; ++d)
    {
        const std::vector<size_t>& indices = leaf.m_indices[d];
        const size_t item_count = indices.size();

        // Compute the extent of the leaf in the splitting dimension.
        const ValueType bbox_min = leaf_bbox.min[d];
        const ValueType bbox_max = leaf_bbox.max[d];
        const ValueType bbox_extent = bbox_max - bbox_min;
        const ValueType rcp_bin_size = m_bin_count / bbox_extent;

        // This node is flat in this dimension.
        if (bbox_extent == ValueType(0.0))
            continue;

        // Clear the bins.
        for (size_t i = 0; i < m_bin_count; ++i)
        {
            Bin& bin = m_bins[i];
            bin.m_bin_bbox.invalidate();
            bin.m_entry_counter = 0;
            bin.m_exit_counter = 0;
        }

        // Push the items through the bins.
        for (size_t i = 0; i < item_count; ++i)
        {
            // Compute the extent of this item in the splitting dimension.
            const size_t item_index = indices[i];
            const AABBType& item_bbox = m_bboxes[item_index];
            const ValueType item_bbox_min = item_bbox.min[d];
            const ValueType item_bbox_max = item_bbox.max[d];
            assert(item_bbox_min <= bbox_max && item_bbox_max >= bbox_min);

            // Find the range of bins covered by this item.
            const size_t begin_bin =
                item_bbox_min > bbox_min
                    ? std::min<size_t>(truncate<size_t>((item_bbox_min - bbox_min) * rcp_bin_size), m_bin_count - 1)
                    : 0;
            const size_t end_bin =
                std::min<size_t>(truncate<size_t>((item_bbox_max - bbox_min) * rcp_bin_size), m_bin_count - 1);
            assert(begin_bin < m_bin_count);
            assert(end_bin < m_bin_count);
            assert(begin_bin <= end_bin);

            // Update the bins that this item overlaps.
            for (size_t b = begin_bin; b <= end_bin; ++b)
            {
                // Compute the bounds of this bin.
                const ValueType bin_min = lerp(bbox_min, bbox_max, (b + 0) * m_rcp_bin_count);
                const ValueType bin_max = lerp(bbox_min, bbox_max, (b + 1) * m_rcp_bin_count);

                // Clip the item against the bin boundaries.
                const AABBType item_clipped_bbox =
                    m_item_handler.clip(
                        item_index,
                        d,
                        bin_min,
                        bin_max);
                assert(item_clipped_bbox.is_valid());

                // Grow the bounding box associated with this bin.
                m_bins[b].m_bin_bbox.insert(item_clipped_bbox);
            }

            // Update the enter/leave counters.
            ++m_bins[begin_bin].m_entry_counter;
            ++m_bins[end_bin].m_exit_counter;
        }

        AABBType bbox_accumulator;

        // Left-to-right sweep to compute the left bounding boxes.
        bbox_accumulator = m_bins[0].m_bin_bbox;
        for (size_t i = 1; i < m_bin_count; ++i)
        {
            Bin& bin = m_bins[i];
            bin.m_left_bbox = bbox_accumulator;
            bbox_accumulator.insert(bin.m_bin_bbox);
        }

        // Right-to-left sweep to compute the right bounding boxes and find the best split.
        size_t left_item_count = item_count;
        size_t right_item_count = 0;
        bbox_accumulator.invalidate();
        for (size_t i = m_bin_count - 1; i > 0; --i)
        {
            const Bin& bin = m_bins[i];

            // Compute the right bounding box.
            bbox_accumulator.insert(bin.m_bin_bbox);

            // We need to have items on both the left and right sides.
            if (!bin.m_left_bbox.is_valid() || !bbox_accumulator.is_valid())
                continue;

            // Update the item counters.
            assert(left_item_count >= bin.m_entry_counter);
            left_item_count -= bin.m_entry_counter;
            right_item_count += bin.m_exit_counter;

            // Compute the cost of this split.
            const ValueType left_cost = half_surface_area(bin.m_left_bbox) * left_item_count;
            const ValueType right_cost = half_surface_area(bbox_accumulator) * right_item_count;
            const ValueType split_cost = left_cost + right_cost;

            // Keep track of the partition with the lowest cost.
            if (best_split_cost > split_cost)
            {
                best_split_cost = split_cost;
                best_split.m_dimension = d;
                best_split.m_abscissa = bbox_accumulator.min[d];
                left_leaf_bbox = bin.m_left_bbox;
                right_leaf_bbox = bbox_accumulator;
            }
        }
    }

    best_split_cost = compute_final_split_cost(leaf_bbox, best_split_cost);

    if (best_split_cost < std::numeric_limits<ValueType>::max())
    {
        // In the case of a spatial split, the bounding boxes of the child nodes must be disjoint.
        assert(AABBType::intersect(left_leaf_bbox, right_leaf_bbox).rank() < Dimension);
    }
}

template <typename ItemHandler, typename AABBVector>
void SBVHPartitioner<ItemHandler, AABBVector>::object_sort(
    LeafType&                       leaf,
    const size_t                    split_dim,
    const size_t                    split_pivot,
    const AABBType&                 left_leaf_bbox,
    const AABBType&                 right_leaf_bbox,
    LeafType&                       left_leaf,
    LeafType&                       right_leaf)
{
    const std::vector<size_t>& split_indices = leaf.m_indices[split_dim];
    const size_t size = split_indices.size();

    enum { Left = 0, Right = 1 };

    for (size_t i = 0; i < split_pivot; ++i)
        m_tags[split_indices[i]] = Left;

    for (size_t i = split_pivot; i < size; ++i)
        m_tags[split_indices[i]] = Right;

    for (size_t d = 0; d < Dimension; ++d)
    {
        left_leaf.m_indices[d].resize(split_pivot);
        right_leaf.m_indices[d].resize(size - split_pivot);

        if (d == split_dim)
        {
            for (size_t i = 0; i < split_pivot; ++i)
            {
                const size_t item_index = leaf.m_indices[d][i];

#ifdef FOUNDATION_SBVH_DEEPCHECK
                assert(AABBType::overlap(m_bboxes[item_index], left_leaf_bbox));
#endif

                left_leaf.m_indices[d][i] = item_index;
            }

            for (size_t i = split_pivot; i < size; ++i)
            {
                const size_t item_index = leaf.m_indices[d][i];

#ifdef FOUNDATION_SBVH_DEEPCHECK
                assert(AABBType::overlap(m_bboxes[item_index], right_leaf_bbox));
#endif

                right_leaf.m_indices[d][i - split_pivot] = item_index;
            }
        }
        else
        {
            size_t left = 0;
            size_t right = 0;

            for (size_t i = 0; i < size; ++i)
            {
                const size_t item_index = leaf.m_indices[d][i];

                if (m_tags[item_index] == Left)
                {
                    assert(left < split_pivot);
                    left_leaf.m_indices[d][left++] = item_index;
                }
                else
                {
                    assert(right < size - split_pivot);
                    right_leaf.m_indices[d][right++] = item_index;
                }
            }

            assert(left == split_pivot);
            assert(right == size - split_pivot);
        }
    }
}

template <typename ItemHandler, typename AABBVector>
void SBVHPartitioner<ItemHandler, AABBVector>::spatial_sort(
    const LeafType&                 leaf,
    const SplitType&                split,
    const AABBType&                 left_leaf_bbox,
    const AABBType&                 right_leaf_bbox,
    LeafType&                       left_leaf,
    LeafType&                       right_leaf) const
{
    // Prevent numerical instabilities by slightly enlarging the left and right bounding boxes.
    const ValueType eps = m_item_handler.get_bbox_grow_eps();
    AABBType enlarged_left_bbox(left_leaf_bbox);
    AABBType enlarged_right_bbox(right_leaf_bbox);
    enlarged_left_bbox.robust_grow(eps);
    enlarged_right_bbox.robust_grow(eps);

    const size_t item_count = leaf.size();

    for (size_t i = 0; i < item_count; ++i)
    {
        const size_t item_index = leaf.m_indices[split.m_dimension][i];
        const AABBType& item_bbox = m_bboxes[item_index];

        if (item_bbox.max[split.m_dimension] <= split.m_abscissa)
        {
#ifdef FOUNDATION_SBVH_DEEPCHECK
            assert(AABBType::overlap(item_bbox, left_leaf_bbox));
#endif
            left_leaf.m_indices[split.m_dimension].push_back(item_index);
        }
        else if (item_bbox.min[split.m_dimension] >= split.m_abscissa)
        {
#ifdef FOUNDATION_SBVH_DEEPCHECK
            assert(AABBType::overlap(item_bbox, right_leaf_bbox));
#endif
            right_leaf.m_indices[split.m_dimension].push_back(item_index);
        }
        else
        {
            const bool in_left = m_item_handler.intersect(item_index, enlarged_left_bbox);
            const bool in_right = !in_left || m_item_handler.intersect(item_index, enlarged_right_bbox);

            assert(in_left || in_right);

            if (in_left)
                left_leaf.m_indices[split.m_dimension].push_back(item_index);

            if (in_right)
                right_leaf.m_indices[split.m_dimension].push_back(item_index);
        }
    }

    // Basic check to make sure we didn't loose any items.
    assert(
        left_leaf.m_indices[split.m_dimension].size() +
        right_leaf.m_indices[split.m_dimension].size() >=
        leaf.m_indices[split.m_dimension].size());

    // Unlike object splits, spatial splits break items orderings, so we need to sort
    // again the items in the left and right leaf nodes, along all dimensions.
    for (size_t d = 0; d < Dimension; ++d)
    {
        if (d != split.m_dimension)
        {
            left_leaf.m_indices[d] = left_leaf.m_indices[split.m_dimension];
            right_leaf.m_indices[d] = right_leaf.m_indices[split.m_dimension];

            // Sort the items according to their bounding boxes.
            StableBboxSortPredicate<AABBVectorType> predicate(m_bboxes, d);
            std::sort(left_leaf.m_indices[d].begin(), left_leaf.m_indices[d].end(), predicate);
            std::sort(right_leaf.m_indices[d].begin(), right_leaf.m_indices[d].end(), predicate);
        }
    }
}

template <typename ItemHandler, typename AABBVector>
inline size_t SBVHPartitioner<ItemHandler, AABBVector>::store(const LeafType& leaf)
{
    const size_t first = m_final_indices.size();

    const std::vector<size_t>& indices = leaf.m_indices[0];
    const size_t size = indices.size();

    for (size_t i = 0; i < size; ++i)
        m_final_indices.push_back(indices[i]);

    return first;
}

template <typename ItemHandler, typename AABBVector>
inline const std::vector<size_t>& SBVHPartitioner<ItemHandler, AABBVector>::get_item_ordering() const
{
    return m_final_indices;
}

template <typename ItemHandler, typename AABBVector>
inline size_t SBVHPartitioner<ItemHandler, AABBVector>::get_spatial_split_count() const
{
    return m_spatial_split_count;
}

template <typename ItemHandler, typename AABBVector>
inline size_t SBVHPartitioner<ItemHandler, AABBVector>::get_object_split_count() const
{
    return m_object_split_count;
}

}   // namespace bvh
}   // namespace foundation
