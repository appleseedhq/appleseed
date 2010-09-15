
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_STATISTICS_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_STATISTICS_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/population.h"
#include "foundation/utility/log.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation {
namespace bvh {

//
// BVH tree statistics.
//

template <typename Tree, typename Builder>
class TreeStatistics
  : public NonCopyable
{
  public:
    // Node type.
    typedef typename Tree::ValueType ValueType;
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
    const ValueType         m_volume;               // volume of the tree
    ValueType               m_leaf_volume;          // total volume of the leaves
    size_t                  m_leaf_count;           // number of leaf nodes
    Population<size_t>      m_leaf_depth;           // leaf depth statistics
    Population<size_t>      m_leaf_size;            // leaf size statistics

    // Helper method to recursively traverse the tree and collect statistics.
    void collect_stats_recurse(
        const Tree&         tree,
        const NodeType&     node,
        const size_t        depth);
};


//
// BVH traversal statistics.
//

class TraversalStatistics
  : public NonCopyable
{
  public:
    // Public members.
    size_t                  m_traversal_count;      // number of times the tree was traversed
    Population<size_t>      m_fetched_nodes;        // number of nodes fetched from memory
    Population<size_t>      m_visited_leaves;       // number of leaves actually visited
    Population<size_t>      m_intersected_items;    // number of items tested for intersection

    // Constructor.
    TraversalStatistics();

    // Print traversal statistics.
    void print(Logger& logger);
};


//
// TreeStatistics class implementation.
//

// Constructor, collects statistics for a given tree.
template <typename Tree, typename Builder>
TreeStatistics<Tree, Builder>::TreeStatistics(
    const Tree&         tree,
    const Builder&      builder)
  : m_build_time(builder.get_build_time())
  , m_memory_size(tree.get_memory_size())
  , m_node_count(tree.m_nodes.size())
  , m_volume(tree.m_bbox.is_valid() ? tree.m_bbox.volume() : ValueType(0.0))
  , m_leaf_volume(ValueType(0.0))
  , m_leaf_count(0)
{
    assert(!tree.m_nodes.empty());

    // Recursively traverse the tree and collect statistics.
    if (tree.m_bbox.is_valid())
        collect_stats_recurse(tree, tree.m_nodes.front(), 0);
}

// Print tree statistics.
template <typename Tree, typename Builder>
void TreeStatistics<Tree, Builder>::print(Logger& logger)
{
    FOUNDATION_LOG_DEBUG(
        logger,
        "  build time       %s\n"
        "  size             %s\n"
        "  nodes            total %s  interior %s  leaves %s\n"
        "  leaf volume      %s\n"
        "  leaf depth       avg %.1f  min %s  max %s  dev %.1f\n"
        "  leaf size        avg %.1f  min %s  max %s  dev %.1f",
        pretty_time(m_build_time).c_str(),
        pretty_size(m_memory_size).c_str(),
        pretty_uint(m_node_count).c_str(),
        pretty_uint(m_node_count - m_leaf_count).c_str(),
        pretty_uint(m_leaf_count).c_str(),
        pretty_percent(m_leaf_volume, m_volume, 1).c_str(),
        m_leaf_depth.get_avg(),
        pretty_uint(m_leaf_depth.get_min()).c_str(),
        pretty_uint(m_leaf_depth.get_max()).c_str(),
        m_leaf_depth.get_dev(),
        m_leaf_size.get_avg(),
        pretty_uint(m_leaf_size.get_min()).c_str(),
        pretty_uint(m_leaf_size.get_max()).c_str(),
        m_leaf_size.get_dev());
}

// Helper method to recursively traverse the tree and collect statistics.
template <typename Tree, typename Builder>
void TreeStatistics<Tree, Builder>::collect_stats_recurse(
    const Tree&             tree,
    const NodeType&         node,
    const size_t            depth)
{
    if (node.get_type() == NodeType::Leaf)
    {
        // Gather leaf statistics.
        m_leaf_depth.insert(depth);
        m_leaf_size.insert(node.get_item_count());
        ++m_leaf_count;
        m_leaf_volume += node.get_bbox().volume();
    }
    else
    {
        // Recurse into the child nodes.
        const size_t child_index = node.get_child_node_index();
        collect_stats_recurse(tree, tree.m_nodes[child_index], depth + 1);
        collect_stats_recurse(tree, tree.m_nodes[child_index + 1], depth + 1);
    }
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_STATISTICS_H
