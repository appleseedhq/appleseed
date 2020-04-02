
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
#include "foundation/math/population.h"
#include "foundation/string/string.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation {
namespace bvh {

//
// BVH tree statistics.
//

template <typename Tree>
class TreeStatistics
  : public Statistics
{
  public:
    typedef typename Tree::NodeType NodeType;
    typedef typename NodeType::AABBType AABBType;

    // Constructor, collects statistics for a given tree.
    TreeStatistics(
        const Tree&         tree,
        const AABBType&     tree_bbox);

  private:
    typedef typename AABBType::ValueType ValueType;

    ValueType               m_leaf_volume;          // total volume of the leaves
    size_t                  m_leaf_count;           // number of leaf nodes
    Population<size_t>      m_leaf_depth;           // leaf depth statistics
    Population<size_t>      m_leaf_size;            // leaf size statistics
    Population<double>      m_sibling_overlap;      // amount of overlap between sibling nodes

    // Helper method to recursively traverse the tree and collect statistics.
    void collect_stats_recurse(
        const Tree&         tree,
        const NodeType&     node,
        const AABBType&     bbox,
        const size_t        depth);
};


//
// BVH traversal statistics.
//

class TraversalStatistics
  : public NonCopyable
{
  public:
    size_t                  m_traversal_count;      // number of times the tree was traversed
    Population<size_t>      m_visited_nodes;        // number of visited nodes
    Population<size_t>      m_visited_leaves;       // number of visited leaves
    Population<size_t>      m_intersected_bboxes;   // number of bounding boxes intersected
    Population<size_t>      m_discarded_nodes;      // number of discarded nodes (not hit by the ray)
    Population<size_t>      m_intersected_items;    // number of items tested for intersection

    // Constructor.
    TraversalStatistics();

    // Retrieve performance statistics.
    Statistics get_statistics() const;
};


//
// TreeStatistics class implementation.
//

template <typename Tree>
TreeStatistics<Tree>::TreeStatistics(
    const Tree&             tree,
    const AABBType&         tree_bbox)
  : m_leaf_volume(ValueType(0.0))
  , m_leaf_count(0)
{
    assert(!tree.m_nodes.empty());

    collect_stats_recurse(tree, tree.m_nodes.front(), tree_bbox, 1);

    const ValueType tree_volume =
        tree_bbox.is_valid() ? tree_bbox.volume() : ValueType(0.0);

    if (m_leaf_volume > tree_volume)
        m_leaf_volume = tree_volume;

    insert_size("size", tree.get_memory_size());
    insert(
        "nodes",
        "total " + pretty_uint(tree.m_nodes.size()) +
        "  interior " + pretty_uint(tree.m_nodes.size() - m_leaf_count) +
        "  leaves " + pretty_uint(m_leaf_count));
    insert_percent("leaf volume", m_leaf_volume, tree_volume);
    insert("leaf depth", m_leaf_depth);
    insert("leaf size", m_leaf_size);
    insert("sibling overlap", m_sibling_overlap, "%");
}

template <typename Tree>
void TreeStatistics<Tree>::collect_stats_recurse(
    const Tree&             tree,
    const NodeType&         node,
    const AABBType&         bbox,
    const size_t            depth)
{
    if (node.is_leaf())
    {
        // Gather leaf statistics.
        m_leaf_depth.insert(depth);
        m_leaf_size.insert(node.get_item_count());
        ++m_leaf_count;
        if (bbox.is_valid())
            m_leaf_volume += bbox.volume();
    }
    else
    {
        // Fetch left and right children.
        const size_t child_index = node.get_child_node_index();
        const AABBType left_bbox = node.get_left_bbox();
        const AABBType right_bbox = node.get_right_bbox();
        const NodeType& left_node = tree.m_nodes[child_index];
        const NodeType& right_node = tree.m_nodes[child_index + 1];

        // Keep track of the amount of overlap between children.
        m_sibling_overlap.insert(AABBType::overlap_ratio(left_bbox, right_bbox) * ValueType(100.0));

        // Recurse into the child nodes.
        collect_stats_recurse(tree, left_node, left_bbox, depth + 1);
        collect_stats_recurse(tree, right_node, right_bbox, depth + 1);
    }
}

}   // namespace bvh
}   // namespace foundation
