
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_BSP_BSP_STATISTICS_H
#define APPLESEED_FOUNDATION_MATH_BSP_BSP_STATISTICS_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/population.h"
#include "foundation/math/split.h"
#include "foundation/utility/log.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation {
namespace bsp {

//
// BSP tree statistics.
//

template <typename Tree, typename Builder>
class TreeStatistics
  : public NonCopyable
{
  public:
    // Types.
    typedef typename Tree::ValueType ValueType;
    typedef typename Tree::AABBType AABBType;
    typedef typename Tree::NodeType NodeType;

    // Constructor, collects statistics for a given tree.
    TreeStatistics(
        const Tree&         tree,
        const Builder&      builder);

    // Print tree statistics.
    void print(Logger& logger);

  private:
    const double            m_build_time;           // construction time in seconds
    const size_t            m_memory_size;          // size of the tree in memory
    const size_t            m_node_count;           // total number of nodes (leaf and interior nodes)
    const size_t            m_leaf_count;           // number of leaf nodes
    const ValueType         m_volume;               // volume of the tree
    ValueType               m_empty_volume;         // total empty volume
    size_t                  m_empty_leaf_count;     // number of empty leaf nodes
    size_t                  m_tree_size;            // number of objects
    Population<size_t>      m_leaf_depth;           // leaf depth statistics
    Population<size_t>      m_leaf_size;            // leaf size statistics

    // Helper method to recursively traverse the tree and collect statistics.
    void collect_stats_recurse(
        const Tree&         tree,
        const NodeType&     node,
        const AABBType&     bbox,
        const size_t        depth);
};


//
// BSP tree traversal statistics.
//

class TraversalStatistics
  : public NonCopyable
{
  public:
    // Public members.
    size_t                  m_traversal_count;      // number of times the tree was traversed
    Population<size_t>      m_visited_interior;     // number of interior nodes visited
    Population<size_t>      m_visited_leaves;       // number of leaf nodes visited
    Population<size_t>      m_intersected_items;    // number of items tested for intersection

    // Constructor.
    TraversalStatistics();

    // Print traversal statistics.
    void print(Logger& logger);
};


//
// TreeStatistics class implementation.
//

template <typename Tree, typename Builder>
TreeStatistics<Tree, Builder>::TreeStatistics(
    const Tree&         tree,
    const Builder&      builder)
  : m_build_time(builder.get_build_time())
  , m_memory_size(tree.get_memory_size())
  , m_node_count(tree.m_nodes.size())
  , m_leaf_count(tree.m_leaves.size())
  , m_volume(tree.m_bbox.is_valid() ? tree.m_bbox.volume() : ValueType(0.0))
  , m_empty_volume(ValueType(0.0))
  , m_empty_leaf_count(0)
  , m_tree_size(0)
{
    assert(!tree.m_nodes.empty());

    collect_stats_recurse(tree, tree.m_nodes.front(), tree.m_bbox, 1);

    if (m_empty_volume > m_volume)
        m_empty_volume = m_volume;
}

template <typename Tree, typename Builder>
void TreeStatistics<Tree, Builder>::print(Logger& logger)
{
    LOG_DEBUG(
        logger,
        "  build time                    %s\n"
        "  size                          %s  %s %s\n"
        "  nodes                         total %s  interior %s  leaves %s\n"
        "  empty leaves                  leaves %s  volume %s\n"
        "  leaf depth                    avg %.1f  min %s  max %s  dev %.1f\n"
        "  leaf size                     avg %.1f  min %s  max %s  dev %.1f",
        pretty_time(m_build_time).c_str(),
        pretty_size(m_memory_size).c_str(),
        pretty_uint(m_tree_size).c_str(),
        plural(m_tree_size, "item").c_str(),
        pretty_uint(m_node_count).c_str(),
        pretty_uint(m_node_count - m_leaf_count).c_str(),
        pretty_uint(m_leaf_count).c_str(),
        pretty_percent(m_empty_leaf_count, m_leaf_count, 1).c_str(),
        pretty_percent(m_empty_volume, m_volume, 1).c_str(),
        m_leaf_depth.get_mean(),
        pretty_uint(m_leaf_depth.get_min()).c_str(),
        pretty_uint(m_leaf_depth.get_max()).c_str(),
        m_leaf_depth.get_dev(),
        m_leaf_size.get_mean(),
        pretty_uint(m_leaf_size.get_min()).c_str(),
        pretty_uint(m_leaf_size.get_max()).c_str(),
        m_leaf_size.get_dev());
}

template <typename Tree, typename Builder>
void TreeStatistics<Tree, Builder>::collect_stats_recurse(
    const Tree&             tree,
    const NodeType&         node,
    const AABBType&         bbox,
    const size_t            depth)
{
    if (node.is_leaf())
    {
        // Gather leaf statistics.
        const size_t leaf_size = node.get_leaf_size();
        assert(leaf_size == tree.m_leaves[node.get_leaf_index()]->get_size());
        m_leaf_depth.insert(depth);
        m_leaf_size.insert(leaf_size);
        m_tree_size += leaf_size;

        // Gather empty leaves statistics.
        if (leaf_size == 0)
        {
            ++m_empty_leaf_count;
            if (bbox.is_valid())
                m_empty_volume += bbox.volume();
        }
    }
    else
    {
        // Compute the bounding boxes of the child nodes.
        const Split<ValueType> split(node.get_split_dim(), node.get_split_abs());
        AABBType left_bbox, right_bbox;
        split_bbox(bbox, split, left_bbox, right_bbox);

        // Recurse into the child nodes.
        const size_t child_index = node.get_child_node_index();
        collect_stats_recurse(tree, tree.m_nodes[child_index], left_bbox, depth + 1);
        collect_stats_recurse(tree, tree.m_nodes[child_index + 1], right_bbox, depth + 1);
    }
}

}       // namespace bsp
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BSP_BSP_STATISTICS_H
