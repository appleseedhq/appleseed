
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
namespace knn {

//
// Tree statistics.
//

template <typename Tree>
class TreeStatistics
  : public Statistics
{
  public:
    typedef typename Tree::ValueType ValueType;
    typedef typename Tree::NodeType NodeType;

    // Constructor, collects statistics for a given tree.
    explicit TreeStatistics(
        const Tree&         tree);

  private:
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

    // Constructor.
    QueryStatistics();

    // Retrieve query statistics.
    Statistics get_statistics() const;
};


//
// TreeStatistics class implementation.
//

template <typename Tree>
TreeStatistics<Tree>::TreeStatistics(
    const Tree&             tree)
  : m_leaf_count(0)
{
    assert(!tree.empty());

    collect_stats_recurse(tree, tree.m_nodes.front(), 1);

    insert("points", tree.m_points.size());
    insert_size("size", tree.get_memory_size());
    insert(
        "nodes",
        "total " + pretty_uint(tree.m_nodes.size()) +
        "  interior " + pretty_uint(tree.m_nodes.size() - m_leaf_count) +
        "  leaves " + pretty_uint(m_leaf_count));
    insert("leaf depth", m_leaf_depth);
    insert("leaf size", m_leaf_size);
}

template <typename Tree>
void TreeStatistics<Tree>::collect_stats_recurse(
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

}   // namespace knn
}   // namespace foundation
