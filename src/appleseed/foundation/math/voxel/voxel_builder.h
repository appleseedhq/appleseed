
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
#include "foundation/math/split.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation {
namespace voxel {

//
// Voxel tree builder.
//

template <
    typename Tree,
    typename Timer = DefaultWallclockTimer
>
class Builder
  : public NonCopyable
{
  public:
    // Types.
    typedef typename Tree::ValueType ValueType;
    typedef typename Tree::VectorType VectorType;
    typedef typename Tree::AABBType AABBType;
    typedef typename Tree::NodeType NodeType;
    typedef Tree TreeType;

    // Constructor.
    Builder(
        TreeType&       tree,
        const AABBType& bbox,
        const ValueType max_extent);

    //
    // Push an item into the tree, refining it where necessary.
    //
    // The item is implicitly represented by an ItemIntersector
    // functor, which must conform to the following prototype:
    //
    //     class ItemIntersector
    //     {
    //       public:
    //         // Return whether the item intersect a given bounding box.
    //         bool intersect(const AABBType& bbox) const;
    //     };
    //
    // Note: by convention, a splitting plane is always considered
    // to belong to the right node only.
    //

    template <typename ItemIntersector>
    void push(const ItemIntersector& item_intersector);

    // Complete the construction of the tree.
    void complete();

    // Return the construction time.
    double get_build_time() const;

  private:
    typedef Split<ValueType> SplitType;

    TreeType&           m_tree;
    ValueType           m_max_extent;
    Stopwatch<Timer>    m_stopwatch;
    double              m_build_time;

    // Recursively push an item into the tree.
    template <typename ItemIntersector>
    void push_recurse(
        const ItemIntersector&  item_intersector,
        const size_t            node_index,
        const AABBType&         node_bbox);

    // Recursively trim the tree.
    bool trim_recurse(
        const size_t            node_index);

    // Compute the maximum leaf node diagonal length.
    void compute_max_diagonal_recurse(
        const size_t            node_index,
        const AABBType&         node_bbox);
};


//
// Builder class implementation.
//

// Constructor.
template <typename Tree, typename Timer>
Builder<Tree, Timer>::Builder(
    TreeType&       tree,
    const AABBType& bbox,
    const ValueType max_extent)
  : m_tree(tree)
  , m_max_extent(max_extent)
  , m_build_time(0.0)
{
    assert(max_extent > ValueType(0.0));

    // Start stopwatch.
    m_stopwatch.start();

    // Clear the voxel tree.
    m_tree.clear();
    m_tree.m_bbox = bbox;

    // Create the root node.
    NodeType root;
    root.make_leaf();
    root.set_solid_bit(false);
    m_tree.m_nodes.push_back(root);
}

// Complete the construction of the tree.
template <typename Tree, typename Timer>
void Builder<Tree, Timer>::complete()
{
    // Trim the tree.
    trim_recurse(0);

    // Compute the maximum leaf node diagonal length.
    assert(m_tree.m_max_diag == ValueType(0.0));
    compute_max_diagonal_recurse(
        0,                      // root node
        m_tree.m_bbox);         // bounding box of the root node
    m_tree.m_max_diag = std::sqrt(m_tree.m_max_diag);

    // Measure and save construction time.
    m_stopwatch.measure();
    m_build_time = m_stopwatch.get_seconds();
}

// Push an item into the tree, refining it where necessary.
template <typename Tree, typename Timer>
template <typename ItemIntersector>
void Builder<Tree, Timer>::push(const ItemIntersector& item_intersector)
{
    // Recursively push the item into the tree.
    push_recurse(
        item_intersector,
        0,                      // root node
        m_tree.m_bbox);         // bounding box of the root node
}

// Return the construction time.
template <typename Tree, typename Timer>
double Builder<Tree, Timer>::get_build_time() const
{
    return m_build_time;
}

// Recursively push an item into the tree.
template <typename Tree, typename Timer>
template <typename ItemIntersector>
void Builder<Tree, Timer>::push_recurse(
    const ItemIntersector&      item_intersector,
    const size_t                node_index,
    const AABBType&             node_bbox)
{
    assert(node_index < m_tree.m_nodes.size());

    // Compute the splitting dimension and abscissa.
    const SplitType split = SplitType::middle(node_bbox);

    // Compute the extent of the node along the splitting dimension.
    const ValueType node_extent =
          node_bbox.max[split.m_dimension]
        - node_bbox.min[split.m_dimension];

    // Descend into the tree if the node is too large.
    if (node_extent > m_max_extent)
    {
        size_t left_node_index;
        size_t right_node_index;

        // Locate the left and right child nodes, or create them if they don't exist yet.
        if (m_tree.m_nodes[node_index].is_leaf())
        {
            // Compute the indices of the child nodes.
            left_node_index = m_tree.m_nodes.size();
            right_node_index = left_node_index + 1;

            // Create the left node.
            NodeType left_node;
            left_node.make_leaf();
            left_node.set_solid_bit(false);
            m_tree.m_nodes.push_back(left_node);

            // Create the right node.
            NodeType right_node;
            right_node.make_leaf();
            right_node.set_solid_bit(false);
            m_tree.m_nodes.push_back(right_node);

            // Convert the parent node to an interior node.
            m_tree.m_nodes[node_index].make_interior();
            m_tree.m_nodes[node_index].set_child_node_index(left_node_index);
            m_tree.m_nodes[node_index].set_split_dim(split.m_dimension);
            m_tree.m_nodes[node_index].set_split_abs(split.m_abscissa);
        }
        else
        {
            // Compute the indices of the child nodes.
            left_node_index = m_tree.m_nodes[node_index].get_child_node_index();
            right_node_index = left_node_index + 1;
        }

        // Compute the bounding boxes of the child nodes.
        AABBType left_node_bbox, right_node_bbox;
        split_bbox(node_bbox, split, left_node_bbox, right_node_bbox);

        // Recursively push the item into the left subtree.
        if (item_intersector.intersect(left_node_bbox))
        {
            push_recurse(
                item_intersector,
                left_node_index,
                left_node_bbox);
        }

        // Recursively push the item into the right subtree.
        if (item_intersector.intersect(right_node_bbox))
        {
            push_recurse(
                item_intersector,
                right_node_index,
                right_node_bbox);
        }
    }
    else
    {
        // The refinement criterion is met: terminate the branch with a solid leaf.
        assert(m_tree.m_nodes[node_index].is_leaf());
        m_tree.m_nodes[node_index].set_solid_bit(true);
    }
}

// Recursively trim the tree.
template <typename Tree, typename Timer>
bool Builder<Tree, Timer>::trim_recurse(
    const size_t                node_index)
{
    assert(node_index < m_tree.m_nodes.size());

    // Fetch the node.
    NodeType& node = m_tree.m_nodes[node_index];

    // If both the left and right children of the current interior node
    // contain only solid leaves, then transform the interior node into
    // a solid leaf node.
    if (node.is_interior())
    {
        const size_t left_node_index = node.get_child_node_index();
        const size_t right_node_index = left_node_index + 1;
        if (trim_recurse(left_node_index) &&
            trim_recurse(right_node_index))
        {
            node.make_leaf();
            node.set_solid_bit(true);
        }
    }

    return node.is_solid();
}

// Compute the maximum leaf node diagonal length.
template <typename Tree, typename Timer>
void Builder<Tree, Timer>::compute_max_diagonal_recurse(
    const size_t                node_index,
    const AABBType&             node_bbox)
{
    assert(node_index < m_tree.m_nodes.size());

    // Fetch the node.
    const NodeType& node = m_tree.m_nodes[node_index];

    if (node.is_leaf())
    {
        if (node.is_solid())
        {
            // Compute the (square of the) length of the diagonal of this leaf node.
            const VectorType e = node_bbox.extent();
            const ValueType diag = e[0] * e[0] + e[1] * e[1] + e[2] * e[2];

            // Keep track of the maximum (squared) diagonal length.
            if (m_tree.m_max_diag < diag)
                m_tree.m_max_diag = diag;
        }
    }
    else
    {
        // Compute the bounding boxes of the child nodes.
        const Split<ValueType> split(node.get_split_dim(), node.get_split_abs());
        AABBType left_node_bbox, right_node_bbox;
        split_bbox(node_bbox, split, left_node_bbox, right_node_bbox);

        // Recurse into the child nodes.
        const size_t child_index = node.get_child_node_index();
        compute_max_diagonal_recurse(child_index, left_node_bbox);
        compute_max_diagonal_recurse(child_index + 1, right_node_bbox);
    }
}

}   // namespace voxel
}   // namespace foundation
