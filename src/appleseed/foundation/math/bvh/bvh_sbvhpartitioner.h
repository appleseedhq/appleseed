
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_SBVHPARTITIONER_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_SBVHPARTITIONER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/bvh/bvh_bboxsortpredicate.h"
#include "foundation/math/scalar.h"
#include "foundation/math/split.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
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
#define FOUNDATION_SBVH_DEEPCHECK

template <typename ItemHandler, typename AABBVector>
class SBVHPartitioner
  : public foundation::NonCopyable
{
  public:
    typedef AABBVector AABBVectorType;
    typedef typename AABBVectorType::value_type AABBType;
    typedef typename AABBType::ValueType ValueType;

    // Constructor.
    SBVHPartitioner(
        ItemHandler&                item_handler,
        const AABBVectorType&       bboxes,
        const size_t                max_leaf_size = 1,
        const size_t                bin_count = 64,
        const ValueType             interior_node_traversal_cost = ValueType(1.0),
        const ValueType             triangle_intersection_cost = ValueType(1.0));

    // Compute the bounding box of a given set of items.
    AABBType compute_bbox(const std::vector<size_t>& indices) const;

    // Partition a set of items into two distinct sets.
    // Return true if the partition must occur, false if the set should be kept unsplit.
    bool partition(
        std::vector<size_t>&        indices,
        const AABBType&             bbox,
        std::vector<size_t>&        left_indices,
        AABBType&                   left_bbox,
        std::vector<size_t>&        right_indices,
        AABBType&                   right_bbox);

    // Store references and return the index of the first reference.
    size_t store_indices(const std::vector<size_t>& indices);

    // Return the items ordering.
    const std::vector<size_t>& get_item_ordering() const;

  private:
    typedef Split<ValueType> SplitType;
    static const size_t Dimension = AABBType::Dimension;

    struct Bin
    {
        AABBType    m_bin_bbox;         // bbox of this bin
        AABBType    m_left_bbox;        // bbox of all the bins to the left of this one
        size_t      m_entry_counter;    // number of triangles that begin in this bin
        size_t      m_exit_counter;     // number of triangles that end in this bin
    };

    ItemHandler&                    m_item_handler;
    const AABBVectorType&           m_bboxes;
    const size_t                    m_max_leaf_size;
    const size_t                    m_bin_count;
    const ValueType                 m_rcp_bin_count;
    const ValueType                 m_interior_node_traversal_cost;
    const ValueType                 m_triangle_intersection_cost;

    ValueType                       m_root_bbox_rcp_sa;
    std::vector<AABBType>           m_left_bboxes;
    std::vector<Bin>                m_bins;
    std::vector<size_t>             m_indices;

    void compute_root_bbox_surface_area();

    ValueType compute_final_split_cost(
        const AABBType&             bbox,
        const double                cost) const;

    // Find the best object split for a given set of items.
    void find_object_split(
        std::vector<size_t>&        indices,
        const AABBType&             bbox,
        AABBType&                   left_bbox,
        AABBType&                   right_bbox,
        size_t&                     best_split_dim,
        size_t&                     best_split_pivot,
        ValueType&                  best_split_cost);

    // Find the best spatial split for a given set of items.
    void find_spatial_split(
        const std::vector<size_t>&  indices,
        const AABBType&             bbox,
        AABBType&                   left_bbox,
        AABBType&                   right_bbox,
        SplitType&                  best_split,
        ValueType&                  best_split_cost);

    // Sort a set of items into two subsets according to a given object split.
    void object_sort(
        std::vector<size_t>&        indices,
        const size_t                split_dim,
        const size_t                split_pivot,
        const AABBType&             left_bbox,
        const AABBType&             right_bbox,
        std::vector<size_t>&        left_indices,
        std::vector<size_t>&        right_indices) const;

    // Sort a set of items into two subsets according to a given spatial split.
    void spatial_sort(
        const std::vector<size_t>&  indices,
        const SplitType&            split,
        const AABBType&             left_bbox,
        const AABBType&             right_bbox,
        std::vector<size_t>&        left_indices,
        std::vector<size_t>&        right_indices) const;
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
    const ValueType                 triangle_intersection_cost)
  : m_item_handler(item_handler)
  , m_bboxes(bboxes)
  , m_max_leaf_size(max_leaf_size)
  , m_bin_count(bin_count)
  , m_rcp_bin_count(ValueType(1.0) / bin_count)
  , m_interior_node_traversal_cost(interior_node_traversal_cost)
  , m_triangle_intersection_cost(triangle_intersection_cost)
  , m_left_bboxes(bboxes.size() > 1 ? bboxes.size() - 1 : 0)
  , m_bins(bin_count)
{
    compute_root_bbox_surface_area();
}

template <typename ItemHandler, typename AABBVector>
typename AABBVector::value_type SBVHPartitioner<ItemHandler, AABBVector>::compute_bbox(
    const std::vector<size_t>&      indices) const
{
    AABBType bbox;
    bbox.invalidate();

    const size_t size = indices.size();

    for (size_t i = 0; i < size; ++i)
        bbox.insert(m_bboxes[indices[i]]);

    return bbox;
}

template <typename ItemHandler, typename AABBVector>
bool SBVHPartitioner<ItemHandler, AABBVector>::partition(
    std::vector<size_t>&            indices,
    const AABBType&                 bbox,
    std::vector<size_t>&            left_indices,
    AABBType&                       left_bbox,
    std::vector<size_t>&            right_indices,
    AABBType&                       right_bbox)
{
    assert(bbox.rank() >= Dimension - 1);

#ifdef FOUNDATION_SBVH_DEEPCHECK
    // Make sure every item intersects the leaf it belongs to.
    for (size_t i = 0; i < indices.size(); ++i)
    {
        const size_t item_index = indices[i];
        const AABBType& item_bbox = m_bboxes[item_index];
        assert(AABBType::overlap(item_bbox, bbox));
    }
#endif

    // Find the best object split.
    AABBType object_split_left_bbox;
    AABBType object_split_right_bbox;
    size_t object_split_dim;
    size_t object_split_pivot;
    ValueType object_split_cost = std::numeric_limits<ValueType>::max();
    find_object_split(
        indices,
        bbox,
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
            const ValueType Alpha = ValueType(1.0e-5);
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
            indices,
            bbox,
            spatial_split_left_bbox,
            spatial_split_right_bbox,
            spatial_split,
            spatial_split_cost);
    }

    // Compute the cost of keeping the leaf unsplit.
    const ValueType leaf_cost = indices.size() * m_triangle_intersection_cost;

    // Select the cheapest option.
    if (leaf_cost <= object_split_cost && leaf_cost <= spatial_split_cost)
    {
        // Don't split, make a leaf.
        return false;
    }
    else if (object_split_cost <= spatial_split_cost)
    {
        // Perform the object split.
        left_bbox = object_split_left_bbox;
        right_bbox = object_split_right_bbox;
        object_sort(
            indices,
            object_split_dim,
            object_split_pivot,
            left_bbox,
            right_bbox,
            left_indices,
            right_indices);
        return true;
    }
    else
    {
        // Perform the spatial split.
        left_bbox = spatial_split_left_bbox;
        right_bbox = spatial_split_right_bbox;
        spatial_sort(
            indices,
            spatial_split,
            left_bbox,
            right_bbox,
            left_indices,
            right_indices);
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

    m_root_bbox_rcp_sa = ValueType(1.0) / surface_area(root_bbox);
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
        m_triangle_intersection_cost * (cost / bbox_half_surface_area);
}

template <typename ItemHandler, typename AABBVector>
void SBVHPartitioner<ItemHandler, AABBVector>::find_object_split(
    std::vector<size_t>&            indices,
    const AABBType&                 bbox,
    AABBType&                       left_bbox,
    AABBType&                       right_bbox,
    size_t&                         best_split_dim,
    size_t&                         best_split_pivot,
    ValueType&                      best_split_cost)
{
    const size_t item_count = indices.size();

    for (size_t d = 0; d < Dimension; ++d)
    {
        // Sort items according to the centroid of their bounding box.
        StableBboxSortPredicate<AABBVectorType> predicate(m_bboxes, d);
        std::sort(indices.begin(), indices.end(), predicate);

        AABBType bbox_accumulator;

        // Left-to-right sweep to accumulate bounding boxes and compute their surface area.
        bbox_accumulator.invalidate();
        for (size_t i = 0; i < item_count - 1; ++i)
        {
            const size_t item_index = indices[i];
            const AABBType& item_bbox = m_bboxes[item_index];
            const AABBType clipped_item_bbox = AABBType::intersect(item_bbox, bbox);
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
            const AABBType clipped_item_bbox = AABBType::intersect(item_bbox, bbox);
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
                left_bbox = m_left_bboxes[i - 1];
                right_bbox = bbox_accumulator;
            }
        }
    }

    best_split_cost = compute_final_split_cost(bbox, best_split_cost);
}

template <typename ItemHandler, typename AABBVector>
void SBVHPartitioner<ItemHandler, AABBVector>::find_spatial_split(
    const std::vector<size_t>&      indices,
    const AABBType&                 bbox,
    AABBType&                       left_bbox,
    AABBType&                       right_bbox,
    SplitType&                      best_split,
    ValueType&                      best_split_cost)
{
    const size_t item_count = indices.size();

    for (size_t d = 0; d < Dimension; ++d)
    {
        // Compute the extent of the leaf in the splitting dimension.
        const ValueType bbox_min = bbox.min[d];
        const ValueType bbox_max = bbox.max[d];
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
                left_bbox = bin.m_left_bbox;
                right_bbox = bbox_accumulator;
            }
        }
    }

    best_split_cost = compute_final_split_cost(bbox, best_split_cost);

    if (best_split_cost < std::numeric_limits<ValueType>::max())
    {
        // In the case of a spatial split, the bounding boxes of the child nodes must be disjoint.
        assert(AABBType::intersect(left_bbox, right_bbox).rank() < Dimension);
    }
}

template <typename ItemHandler, typename AABBVector>
void SBVHPartitioner<ItemHandler, AABBVector>::object_sort(
    std::vector<size_t>&            indices,
    const size_t                    split_dim,
    const size_t                    split_pivot,
    const AABBType&                 left_bbox,
    const AABBType&                 right_bbox,
    std::vector<size_t>&            left_indices,
    std::vector<size_t>&            right_indices) const
{
    // Sort items according to the centroid of their bounding box.
    StableBboxSortPredicate<AABBVectorType> predicate(m_bboxes, split_dim);
    std::sort(indices.begin(), indices.end(), predicate);

    const size_t item_count = indices.size();

    left_indices.resize(split_pivot);
    right_indices.resize(item_count - split_pivot);

    for (size_t i = 0; i < split_pivot; ++i)
    {
        const size_t item_index = indices[i];
        const AABBType& item_bbox = m_bboxes[item_index];
#ifdef FOUNDATION_SBVH_DEEPCHECK
        assert(AABBType::overlap(item_bbox, left_bbox));
#endif
        left_indices[i] = item_index;
    }

    for (size_t i = split_pivot; i < item_count; ++i)
    {
        const size_t item_index = indices[i];
        const AABBType& item_bbox = m_bboxes[item_index];
#ifdef FOUNDATION_SBVH_DEEPCHECK
        assert(AABBType::overlap(item_bbox, right_bbox));
#endif
        right_indices[i - split_pivot] = item_index;
    }
}

template <typename ItemHandler, typename AABBVector>
void SBVHPartitioner<ItemHandler, AABBVector>::spatial_sort(
    const std::vector<size_t>&      indices,
    const SplitType&                split,
    const AABBType&                 left_bbox,
    const AABBType&                 right_bbox,
    std::vector<size_t>&            left_indices,
    std::vector<size_t>&            right_indices) const
{
    // Prevent numerical instabilities by slightly enlarging the left and right bounding boxes.
    const ValueType eps = m_item_handler.get_bbox_grow_eps();
    AABBType enlarged_left_bbox(left_bbox);
    AABBType enlarged_right_bbox(right_bbox);
    enlarged_left_bbox.robust_grow(eps);
    enlarged_right_bbox.robust_grow(eps);

    const size_t item_count = indices.size();

    for (size_t i = 0; i < item_count; ++i)
    {
        const size_t item_index = indices[i];
        const AABBType& item_bbox = m_bboxes[item_index];

        if (item_bbox.max[split.m_dimension] <= split.m_abscissa)
        {
#ifdef FOUNDATION_SBVH_DEEPCHECK
            assert(AABBType::overlap(item_bbox, left_bbox));
#endif
            left_indices.push_back(item_index);
        }
        else if (item_bbox.min[split.m_dimension] >= split.m_abscissa)
        {
#ifdef FOUNDATION_SBVH_DEEPCHECK
            assert(AABBType::overlap(item_bbox, right_bbox));
#endif
            right_indices.push_back(item_index);
        }
        else
        {
            const bool in_left = m_item_handler.intersect(item_index, enlarged_left_bbox);
            const bool in_right = !in_left || m_item_handler.intersect(item_index, enlarged_right_bbox);

            assert(in_left || in_right);

            if (in_left)
                left_indices.push_back(item_index);

            if (in_right)
                right_indices.push_back(item_index);
        }
    }
}

template <typename ItemHandler, typename AABBVector>
inline size_t SBVHPartitioner<ItemHandler, AABBVector>::store_indices(
    const std::vector<size_t>&      indices)
{
    const size_t first = m_indices.size();
    m_indices.insert(m_indices.end(), indices.begin(), indices.end());
    return first;
}

template <typename ItemHandler, typename AABBVector>
inline const std::vector<size_t>& SBVHPartitioner<ItemHandler, AABBVector>::get_item_ordering() const
{
    return m_indices;
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_SBVHPARTITIONER_H
