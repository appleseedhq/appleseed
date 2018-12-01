
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
#include <vector>

namespace foundation {
namespace bvh {

//
// BVH builder supporting spatial splits (with possible reference duplication).
//
// The Partitioner class must conform to the following prototype:
//
//      class Partitioner
//        : public foundation::NonCopyable
//      {
//        public:
//          struct LeafType
//          {
//              // Return the number of items in the leaf.
//              size_t size();
//          };
//
//          // Split a leaf. Return true if the split should be split or false if it should be kept unsplit.
//          bool split(
//              const LeafType&     leaf,
//              const AABBType&     leaf_bbox,
//              LeafType&           left_leaf,
//              AABBType&           left_left_bbox,
//              LeafType&           right_leaf,
//              AABBType&           right_leaf_bbox);
//
//          // Store a leaf. Return the index of the first stored item.
//          size_t store(const LeafType& leaf);
//      };
//

template <typename Tree, typename Partitioner>
class SpatialBuilder
  : public NonCopyable
{
  public:
    typedef typename Tree::NodeType NodeType;
    typedef typename NodeType::AABBType AABBType;
    typedef typename Partitioner::LeafType LeafType;

    // Constructor.
    SpatialBuilder();

    // Build a tree.
    template <typename Timer>
    void build(
        Tree&               tree,
        Partitioner&        partitioner,
        LeafType*           root_leaf,
        const AABBType&     root_leaf_bbox);

    // Return the construction time.
    double get_build_time() const;

  private:
    typedef std::vector<const LeafType*> LeafVector;

    double m_build_time;

    // Recursively subdivide the tree.
    void subdivide_recurse(
        Tree&               tree,
        Partitioner&        partitioner,
        LeafVector&         leaves,
        LeafType*           leaf,
        const AABBType&     leaf_bbox,
        const size_t        leaf_node_index,
        const size_t        depth);
};


//
// SpatialBuilder class implementation.
//

template <typename Tree, typename Partitioner>
SpatialBuilder<Tree, Partitioner>::SpatialBuilder()
  : m_build_time(0.0)
{
}

template <typename Tree, typename Partitioner>
template <typename Timer>
void SpatialBuilder<Tree, Partitioner>::build(
    Tree&                   tree,
    Partitioner&            partitioner,
    LeafType*               root_leaf,
    const AABBType&         root_leaf_bbox)
{
    // Start stopwatch.
    Stopwatch<Timer> stopwatch;
    stopwatch.start();

    // Clear the tree.
    tree.m_nodes.clear();

    // Create the root node of the tree.
    tree.m_nodes.push_back(NodeType());

    // todo: preallocate node memory?

    // Recursively subdivide the tree.
    LeafVector leaves;
    subdivide_recurse(
        tree,
        partitioner,
        leaves,
        root_leaf,
        root_leaf_bbox,
        0,
        0);

    // Store the leaves.
    const size_t node_count = tree.m_nodes.size();
    for (size_t i = 0; i < node_count; ++i)
    {
        NodeType& node = tree.m_nodes[i];
        if (node.is_leaf())
        {
            const LeafType* leaf = leaves[node.get_item_index()];
            node.set_item_index(partitioner.store(*leaf));
            delete leaf;
        }
    }

    // Measure and save construction time.
    stopwatch.measure();
    m_build_time = stopwatch.get_seconds();
}

template <typename Tree, typename Partitioner>
inline double SpatialBuilder<Tree, Partitioner>::get_build_time() const
{
    return m_build_time;
}

template <typename Tree, typename Partitioner>
void SpatialBuilder<Tree, Partitioner>::subdivide_recurse(
    Tree&                   tree,
    Partitioner&            partitioner,
    LeafVector&             leaves,
    LeafType*               leaf,
    const AABBType&         leaf_bbox,
    const size_t            leaf_node_index,
    const size_t            depth)
{
    assert(leaf_node_index < tree.m_nodes.size());

    // Try to split the leaf.
    LeafType* left_leaf = new LeafType();
    LeafType* right_leaf = new LeafType();
    AABBType left_leaf_bbox, right_leaf_bbox;
    const bool split =
        partitioner.split(
            *leaf,
            leaf_bbox,
            *left_leaf,
            left_leaf_bbox,
            *right_leaf,
            right_leaf_bbox);

    if (split)
    {
        // Get rid of the current leaf.
        delete leaf;

        // Compute the indices of the child nodes.
        const size_t left_node_index = tree.m_nodes.size();
        const size_t right_node_index = left_node_index + 1;

        // Turn the current node into an interior node.
        NodeType& node = tree.m_nodes[leaf_node_index];
        node.make_interior();
        node.set_left_bbox(left_leaf_bbox);
        node.set_right_bbox(right_leaf_bbox);
        node.set_child_node_index(left_node_index);

        // Create the child nodes.
        tree.m_nodes.push_back(NodeType());
        tree.m_nodes.push_back(NodeType());

        // Recurse into the left subtree.
        subdivide_recurse(
            tree,
            partitioner,
            leaves,
            left_leaf,
            left_leaf_bbox,
            left_node_index,
            depth + 1);

        // Recurse into the right subtree.
        subdivide_recurse(
            tree,
            partitioner,
            leaves,
            right_leaf,
            right_leaf_bbox,
            right_node_index,
            depth + 1);
    }
    else
    {
        // Get rid of the child nodes.
        delete left_leaf;
        delete right_leaf;

        // Turn the current node into a leaf node.
        NodeType& node = tree.m_nodes[leaf_node_index];
        node.make_leaf();
        node.set_item_index(leaves.size());
        node.set_item_count(leaf->size());
        leaves.push_back(leaf);
    }
}

}   // namespace bvh
}   // namespace foundation
