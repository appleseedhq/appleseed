
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

#ifndef APPLESEED_FOUNDATION_MATH_KNN_KNN_STATISTICS_H
#define APPLESEED_FOUNDATION_MATH_KNN_KNN_STATISTICS_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/population.h"
#include "foundation/utility/log.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation {
namespace knn {

//
// Tree statistics.
//

template <typename Tree, typename Builder>
class TreeStatistics
  : public NonCopyable
{
  public:
    typedef typename Tree::ValueType ValueType;
    typedef typename Tree::NodeType NodeType;

    // Constructor, collects statistics for a given tree.
    TreeStatistics(
        const Tree&         tree,
        const Builder&      builder);

    void print(Logger& logger);

  private:
    const double            m_build_time;           // construction time in seconds
    const size_t            m_point_count;          // number of points in the tree
    const size_t            m_memory_size;          // size of the tree in memory
    const size_t            m_node_count;           // total number of nodes (leaf and interior nodes)
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
// Query statistics.
//

class QueryStatistics
  : public NonCopyable
{
  public:
    size_t                  m_query_count;          // number of executed queries
    Population<size_t>      m_fetched_nodes;        // number of nodes fetched from memory
    Population<size_t>      m_visited_leaves;       // number of leaves actually visited
    Population<size_t>      m_tested_points;        // number of points tested for inclusion

    QueryStatistics();

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
  , m_point_count(tree.m_points.size())
  , m_memory_size(tree.get_memory_size())
  , m_node_count(tree.m_nodes.size())
  , m_leaf_count(0)
{
    assert(!tree.empty());

    collect_stats_recurse(tree, tree.m_nodes.front(), 0);
}

template <typename Tree, typename Builder>
void TreeStatistics<Tree, Builder>::print(Logger& logger)
{
    LOG_DEBUG(
        logger,
        "  build time       %s\n"
        "  points           %s\n"
        "  size             %s\n"
        "  nodes            total %s  interior %s  leaves %s\n"
        "  leaf depth       avg %.1f  min %s  max %s  dev %.1f\n"
        "  leaf size        avg %.1f  min %s  max %s  dev %.1f",
        pretty_time(m_build_time).c_str(),
        pretty_uint(m_point_count).c_str(),
        pretty_size(m_memory_size).c_str(),
        pretty_uint(m_node_count).c_str(),
        pretty_uint(m_node_count - m_leaf_count).c_str(),
        pretty_uint(m_leaf_count).c_str(),
        m_leaf_depth.get_avg(),
        pretty_uint(m_leaf_depth.get_min()).c_str(),
        pretty_uint(m_leaf_depth.get_max()).c_str(),
        m_leaf_depth.get_dev(),
        m_leaf_size.get_avg(),
        pretty_uint(m_leaf_size.get_min()).c_str(),
        pretty_uint(m_leaf_size.get_max()).c_str(),
        m_leaf_size.get_dev());
}

template <typename Tree, typename Builder>
void TreeStatistics<Tree, Builder>::collect_stats_recurse(
    const Tree&             tree,
    const NodeType&         node,
    const size_t            depth)
{
    if (node.is_leaf())
    {
        m_leaf_depth.insert(depth);
        m_leaf_size.insert(node.get_point_count());
        ++m_leaf_count;
    }
    else
    {
        const size_t child_index = node.get_child_node_index();
        collect_stats_recurse(tree, tree.m_nodes[child_index], depth + 1);
        collect_stats_recurse(tree, tree.m_nodes[child_index + 1], depth + 1);
    }
}

}       // namespace knn
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_KNN_KNN_STATISTICS_H
