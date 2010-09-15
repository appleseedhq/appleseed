
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/timer.h"
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
//          // Partition a set of items into two distinct sets.
//          // bbox is the bounding box of the items in [begin, end).
//          // Return the index of the first item in the right
//          // partition, or end if the set is not to be partitioned.
//          size_t partition(
//              std::vector<Item>&      items,
//              std::vector<AABB>&      bboxes,
//              const size_t            begin,
//              const size_t            end,
//              const AABB&             bbox);
//      };
//

template <
    typename Tree,
    typename Partitioner,
    typename Timer = DefaultWallclockTimer
>
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

    // Build a BVH for a given set of items.
    void build(
        TreeType&       tree,
        Partitioner&    partitioner);

    // Return the construction time.
    double get_build_time() const;

  private:
    double m_build_time;

    // Compute the bounding box of a set of bounding boxes.
    static AABBType compute_bbox(
        const std::vector<AABBType>&    bboxes,
        const size_t                    begin,
        const size_t                    end)
    {
        AABBType bbox;
        bbox.invalidate();
        for (size_t i = begin; i < end; ++i)
            bbox.insert(bboxes[i]);
        return bbox;
    }

    // Recursively subdivide the tree.
    void subdivide_recurse(
        TreeType&       tree,
        const size_t    node_index,
        const size_t    begin,
        const size_t    end,
        Partitioner&    partitioner);
};


//
// Builder class implementation.
//

// Constructor.
template <
    typename Tree,
    typename Partitioner,
    typename Timer
>
Builder<Tree, Partitioner, Timer>::Builder()
  : m_build_time(0.0)
{
}

// Build a BVH for a given set of items.
template <
    typename Tree,
    typename Partitioner,
    typename Timer
>
void Builder<Tree, Partitioner, Timer>::build(
    TreeType&           tree,
    Partitioner&        partitioner)
{
    assert(tree.m_items.size() == tree.m_bboxes.size());

    // Start stopwatch.
    Stopwatch<Timer> stopwatch;
    stopwatch.start();

    // Clear the BVH.
    tree.m_nodes.clear();

    // Create the root node of the tree.
    tree.m_nodes.push_back(NodeType());

    // todo: preallocate memory.

    // Recursively subdivide the tree.
    subdivide_recurse(
        tree,
        0,                      // node index
        0,                      // begin
        tree.m_items.size(),    // end
        partitioner);

    // Measure and save construction time.
    stopwatch.measure();
    m_build_time = stopwatch.get_seconds();
}

// Return the construction time.
template <
    typename Tree,
    typename Partitioner,
    typename Timer
>
double Builder<Tree, Partitioner, Timer>::get_build_time() const
{
    return m_build_time;
}

// Recursively subdivide the tree.
template <
    typename Tree,
    typename Partitioner,
    typename Timer
>
void Builder<Tree, Partitioner, Timer>::subdivide_recurse(
    TreeType&           tree,
    const size_t        node_index,
    const size_t        begin,
    const size_t        end,
    Partitioner&        partitioner)
{
    assert(node_index < tree.m_nodes.size());

    // Compute the bounding box of the items.
    const AABBType bbox = compute_bbox(tree.m_bboxes, begin, end);

    // Try to partition the set of items.
    size_t pivot = end;
    if (end - begin > 1)
    {
        pivot =
            partitioner.partition(
                tree.m_items,
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
        node.set_bbox(bbox);
        node.set_item_index(begin);
        node.set_item_count(end - begin);
    }
    else
    {
        // Compute the indices of the child nodes.
        const size_t left_node_index = tree.m_nodes.size();
        const size_t right_node_index = left_node_index + 1;

        // Turn the parent node into an interior node.
        NodeType& node = tree.m_nodes[node_index];
        node.set_type(NodeType::Interior);
        node.set_bbox(bbox);
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
            partitioner);

        // Recurse into the right subtree.
        subdivide_recurse(
            tree,
            right_node_index,
            pivot,
            end,
            partitioner);
    }
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_BUILDER_H
