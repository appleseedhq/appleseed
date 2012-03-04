
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_SPATIALBUILDER_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_SPATIALBUILDER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/permutation.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

namespace foundation {
namespace bvh {

//
// BVH builder supporting spatial splits (with reference duplication).
//
// The Partitioner class must conform to the following prototype:
//
//      class Partitioner
//        : public foundation::NonCopyable
//      {
//        public:
//          // Compute the bounding box of a given set of items.
//          AABBType compute_bbox(const std::vector<size_t>& indices) const;
//
//          // Partition a set of items into two distinct sets.
//          // Return true if the partition must occur, false if the set should be kept unsplit.
//          bool partition(
//              const std::vector<size_t>&  indices,
//              const AABBType&             bbox,
//              std::vector<size_t>&        left_indices,
//              AABBType&                   left_bbox,
//              std::vector<size_t>&        right_indices,
//              AABBType&                   right_bbox);
//
//          // Store references and return the index of the first reference.
//          size_t store_indices(const std::vector<size_t>& indices);
//      };
//

template <typename Tree, typename Partitioner>
class SpatialBuilder
  : public NonCopyable
{
  public:
    // Constructor.
    SpatialBuilder();

    // Build a tree.
    template <typename Timer>
    void build(
        Tree&               tree,
        const size_t        size,
        Partitioner&        partitioner);

    // Return the construction time.
    double get_build_time() const;

  private:
    typedef typename Tree::NodeType NodeType;
    typedef typename NodeType::AABBType AABBType;

    typedef std::vector<size_t> IndexVector;
    typedef std::vector<const IndexVector*> LeafVector;

    double m_build_time;

    // Recursively subdivide the tree.
    void subdivide_recurse(
        Tree&               tree,
        LeafVector&         leaves,
        const size_t        depth,
        const size_t        node_index,
        IndexVector*        indices,
        const AABBType&     bbox,
        Partitioner&        partitioner);
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
    const size_t            size,
    Partitioner&            partitioner)
{
    // Start stopwatch.
    Stopwatch<Timer> stopwatch;
    stopwatch.start();

    // Clear the tree.
    tree.m_nodes.clear();

    // Create the root node of the tree.
    tree.m_nodes.push_back(NodeType());

    // Create the root leaf.
    IndexVector* root_indices = new IndexVector(size);
    if (size > 0)
        identity_permutation(size, &root_indices->front());
    const AABBType root_bbox = partitioner.compute_bbox(*root_indices);

    // todo: preallocate node memory?

    // Recursively subdivide the tree.
    LeafVector leaves;
    subdivide_recurse(
        tree,
        leaves,
        0,
        0,
        root_indices,
        root_bbox,
        partitioner);

    // Store leaf indices.
    const size_t node_count = tree.m_nodes.size();
    for (size_t i = 0; i < node_count; ++i)
    {
        NodeType& node = tree.m_nodes[i];
        if (node.is_leaf())
        {
            const IndexVector* indices = leaves[node.get_item_index()];
            node.set_item_index(partitioner.store_indices(*indices));
            delete indices;
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
    LeafVector&             leaves,
    const size_t            depth,
    const size_t            node_index,
    IndexVector*            indices,
    const AABBType&         bbox,
    Partitioner&            partitioner)
{
    assert(node_index < tree.m_nodes.size());

    IndexVector* left_indices = new IndexVector();
    IndexVector* right_indices = new IndexVector();
    AABBType left_bbox, right_bbox;
    bool split = false;

    // Try to partition the set of items.
    if (indices->size() > 1)
    {
        split =
            partitioner.partition(
                *indices,
                bbox,
                *left_indices,
                left_bbox,
                *right_indices,
                right_bbox);
    }

    if (split)
    {
        // Basic check to make sure we didn't loose any items.
        assert(left_indices->size() + right_indices->size() >= indices->size());

        // Get rid of the current node.
        delete indices;

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
            leaves,
            depth + 1,
            left_node_index,
            left_indices,
            left_bbox,
            partitioner);

        // Recurse into the right subtree.
        subdivide_recurse(
            tree,
            leaves,
            depth + 1,
            right_node_index,
            right_indices,
            right_bbox,
            partitioner);
    }
    else
    {
        // Get rid of the child nodes.
        delete left_indices;
        delete right_indices;

        // Turn the current node into a leaf node.
        NodeType& node = tree.m_nodes[node_index];
        node.make_leaf();
        node.set_item_index(leaves.size());
        node.set_item_count(indices->size());
        leaves.push_back(indices);
    }
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_SPATIALBUILDER_H
