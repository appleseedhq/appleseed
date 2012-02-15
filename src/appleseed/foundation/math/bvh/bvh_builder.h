
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_BUILDER_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_BUILDER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/permutation.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

namespace foundation {
namespace bvh {

//
// BVH builder.
//
// The Partitioner class must conform to the following prototype:
//
//      class Partitioner
//        : public foundation::NonCopyable
//      {
//          // Initialize the partitioner for a given number of items.
//          void initialize(
//              const std::vector<AABBType>&    bboxes,
//              const size_t                    size);
//
//          // Compute the bounding box of a given set of items.
//          AABBType compute_bbox(
//              const std::vector<AABBType>&    bboxes,
//              const size_t                    begin,
//              const size_t                    end) const;
//
//          // Partition a set of items into two distinct sets.
//          // 'bbox' is the bounding box of the items in [begin, end).
//          // Return the index of the first item in the right
//          // partition, or 'end' if the set is not to be partitioned.
//          size_t partition(
//              const std::vector<AABB>&        bboxes,
//              const size_t                    begin,
//              const size_t                    end,
//              const AABB&                     bbox);
//
//          // Return the items ordering.
//          const std::vector<size_t>& get_item_ordering() const;
//      };
//

template <typename Tree, typename Partitioner>
class Builder
  : public NonCopyable
{
  public:
    // Types.
    typedef typename Tree::ValueType ValueType;
    typedef typename Tree::AABBType AABBType;
    typedef typename Tree::NodeType NodeType;
    typedef typename Tree::ItemType ItemType;
    typedef Tree TreeType;

    // Constructor.
    Builder();

    // Build a tree for a given set of items.
    template <typename Timer>
    void build(
        TreeType&       tree,
        Partitioner&    partitioner);

    // Return the construction time.
    double get_build_time() const;

  private:
    double  m_build_time;

    // Recursively subdivide the tree.
    void subdivide_recurse(
        TreeType&       tree,
        const size_t    node_index,
        const size_t    begin,
        const size_t    end,
        const AABBType& bbox,
        Partitioner&    partitioner);
};


//
// Builder class implementation.
//

template <typename Tree, typename Partitioner>
Builder<Tree, Partitioner>::Builder()
  : m_build_time(0.0)
{
}

template <typename Tree, typename Partitioner>
template <typename Timer>
void Builder<Tree, Partitioner>::build(
    TreeType&           tree,
    Partitioner&        partitioner)
{
    assert(tree.m_items.size() == tree.m_bboxes.size());

    // Start stopwatch.
    Stopwatch<Timer> stopwatch;
    stopwatch.start();

    const size_t size = tree.m_items.size();

    // Initialize the partitioner.
    partitioner.initialize(tree.m_bboxes, size);

    // Clear the tree.
    tree.m_nodes.clear();

    // Create the root node of the tree.
    tree.m_nodes.push_back(NodeType());

    // todo: preallocate memory.

    // Recursively subdivide the tree.
    subdivide_recurse(
        tree,
        0,                  // node index
        0,                  // begin
        size,               // end
        tree.get_bbox(),
        partitioner);

    if (size > 0)
    {
        const std::vector<size_t>& indices = partitioner.get_item_ordering();

        // Reorder the items.
        std::vector<ItemType> temp_items(size);
        small_item_reorder(&tree.m_items[0], &temp_items[0], &indices[0], size);
        clear_release_memory(temp_items);

        // Reorder their bounding boxes.
        std::vector<AABBType> temp_bboxes(size);
        small_item_reorder(&tree.m_bboxes[0], &temp_bboxes[0], &indices[0], size);
        clear_release_memory(temp_bboxes);
    }

    // Measure and save construction time.
    stopwatch.measure();
    m_build_time = stopwatch.get_seconds();
}

template <typename Tree, typename Partitioner>
inline double Builder<Tree, Partitioner>::get_build_time() const
{
    return m_build_time;
}

template <typename Tree, typename Partitioner>
void Builder<Tree, Partitioner>::subdivide_recurse(
    TreeType&           tree,
    const size_t        node_index,
    const size_t        begin,
    const size_t        end,
    const AABBType&     bbox,
    Partitioner&        partitioner)
{
    assert(node_index < tree.m_nodes.size());

    // Try to partition the set of items.
    size_t pivot = end;
    if (end - begin > 1)
    {
        pivot =
            partitioner.partition(
                tree.m_bboxes,
                begin,
                end,
                bbox);
        assert(pivot > begin);
        assert(pivot <= end);
    }

    if (pivot == end)
    {
        // Turn the parent node into a leaf node for this set of items.
        NodeType& node = tree.m_nodes[node_index];
        node.set_type(NodeType::Leaf);
        node.set_item_index(begin);
        node.set_item_count(end - begin);
    }
    else
    {
        // Compute the bounding box of the child nodes.
        const AABBType left_bbox = partitioner.compute_bbox(tree.m_bboxes, begin, pivot);
        const AABBType right_bbox = partitioner.compute_bbox(tree.m_bboxes, pivot, end);

        // Compute the indices of the child nodes.
        const size_t left_node_index = tree.m_nodes.size();
        const size_t right_node_index = left_node_index + 1;

        // Turn the parent node into an interior node.
        NodeType& node = tree.m_nodes[node_index];
        node.set_type(NodeType::Interior);
        node.set_left_bbox(left_bbox);
        node.set_right_bbox(right_bbox);
        node.set_child_node_index(left_node_index);

        // Create the child nodes.
        tree.m_nodes.push_back(NodeType());
        tree.m_nodes.push_back(NodeType());

        // Recurse into the left subtree.
        subdivide_recurse(
            tree,
            left_node_index,
            begin,
            pivot,
            left_bbox,
            partitioner);

        // Recurse into the right subtree.
        subdivide_recurse(
            tree,
            right_node_index,
            pivot,
            end,
            right_bbox,
            partitioner);
    }
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_BUILDER_H
