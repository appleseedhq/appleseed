
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
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cassert>
#include <cstddef>

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
//        public:
//          // Compute the bounding box of a given set of items.
//          AABBType compute_bbox(
//              const size_t        begin,
//              const size_t        end) const;
//
//          // Partition a set of items into two distinct sets.
//          // 'bbox' is the bounding box of the items in [begin, end).
//          // Return the index of the first item in the right
//          // partition, or 'end' if the set is not to be partitioned.
//          size_t partition(
//              const size_t        begin,
//              const size_t        end,
//              const AABBType&     bbox);
//      };
//

template <typename Tree, typename Partitioner>
class Builder
  : public NonCopyable
{
  public:
    // Constructor.
    Builder();

    // Build a tree.
    template <typename Timer>
    void build(
        Tree&           tree,
        Partitioner&    partitioner,
        const size_t    size,
        const size_t    items_per_leaf_hint);

    // Return the construction time.
    double get_build_time() const;

  private:
    typedef typename Tree::NodeType NodeType;
    typedef typename NodeType::AABBType AABBType;

    double m_build_time;

    // Recursively subdivide the tree.
    void subdivide_recurse(
        Tree&           tree,
        Partitioner&    partitioner,
        const size_t    node_index,
        const size_t    begin,
        const size_t    end,
        const AABBType& bbox);
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
    Tree&               tree,
    Partitioner&        partitioner,
    const size_t        size,
    const size_t        items_per_leaf_hint)
{
    // Start stopwatch.
    Stopwatch<Timer> stopwatch;
    stopwatch.start();

    // Clear the tree.
    tree.m_nodes.clear();

    // Reserve memory for the nodes.
    const size_t leaf_count_guess = size / items_per_leaf_hint;
    const size_t node_count_guess = leaf_count_guess > 0 ? 2 * leaf_count_guess - 1 : 0;
    tree.m_nodes.reserve(node_count_guess);

    // Create the root node of the tree.
    tree.m_nodes.push_back(NodeType());

    // Compute the bounding box of the tree.
    const AABBType root_bbox(partitioner.compute_bbox(0, size));

    // todo: preallocate node memory?

    // Recursively subdivide the tree.
    subdivide_recurse(
        tree,
        partitioner,
        0,              // node index
        0,              // begin
        size,           // end
        root_bbox);

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
    Tree&               tree,
    Partitioner&        partitioner,
    const size_t        node_index,
    const size_t        begin,
    const size_t        end,
    const AABBType&     bbox)
{
    assert(node_index < tree.m_nodes.size());

    // Try to partition the set of items.
    size_t pivot = end;
    if (end - begin > 1)
    {
        pivot = partitioner.partition(begin, end, typename Partitioner::AABBType(bbox));
        assert(pivot > begin);
        assert(pivot <= end);
    }

    if (pivot == end)
    {
        // Turn the current node into a leaf node.
        NodeType& node = tree.m_nodes[node_index];
        node.make_leaf();
        node.set_item_index(begin);
        node.set_item_count(end - begin);
    }
    else
    {
        // Compute the bounding box of the child nodes.
        const AABBType left_bbox(partitioner.compute_bbox(begin, pivot));
        const AABBType right_bbox(partitioner.compute_bbox(pivot, end));

        // Compute the indices of the child nodes.
        const size_t left_node_index = tree.m_nodes.size();
        const size_t right_node_index = left_node_index + 1;

        // Turn the current node into an interior node.
        NodeType& node = tree.m_nodes[node_index];
        node.make_interior();
        node.set_left_bbox(left_bbox);
        node.set_right_bbox(right_bbox);
        node.set_child_node_index(left_node_index);

        // Create the child nodes.
        tree.m_nodes.push_back(NodeType());
        tree.m_nodes.push_back(NodeType());

        // Recurse into the left subtree.
        subdivide_recurse(
            tree,
            partitioner,
            left_node_index,
            begin,
            pivot,
            left_bbox);

        // Recurse into the right subtree.
        subdivide_recurse(
            tree,
            partitioner,
            right_node_index,
            pivot,
            end,
            right_bbox);
    }
}

}   // namespace bvh
}   // namespace foundation
