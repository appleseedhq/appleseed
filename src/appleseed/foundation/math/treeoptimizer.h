
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_TREEOPTIMIZER_H
#define APPLESEED_FOUNDATION_MATH_TREEOPTIMIZER_H

// appleseed.foundation headers.
#include "foundation/utility/memory.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <stack>
#include <vector>

namespace foundation
{

//
// Rearrange an array of nodes to follow the van Emde Boas layout.
//

template <typename Node>
class TreeOptimizer
{
  public:
    void optimize_node_layout(
        std::vector<Node>&          nodes,
        const size_t                subtree_depth);

  private:
    std::stack<size_t>              m_roots;
    std::vector<Node>               m_optimized_nodes;

    void optimize_subtree(
        const std::vector<Node>&    nodes,
        const size_t                node_index,
        const size_t                subtree_depth);
};


//
// TreeOptimizer class implementation.
//

template <typename Node>
void TreeOptimizer<Node>::optimize_node_layout(
    std::vector<Node>&              nodes,
    const size_t                    subtree_depth)
{
    assert(m_roots.empty());
    assert(m_optimized_nodes.empty());
    assert(!nodes.empty());
    assert(subtree_depth > 0);

    m_optimized_nodes.reserve(nodes.size());

    m_roots.push(0);
    m_optimized_nodes.push_back(nodes[0]);

    while (!m_roots.empty())
    {
        const size_t root_index = m_roots.top();
        m_roots.pop();

        optimize_subtree(nodes, root_index, subtree_depth);
    }

    assert(m_roots.empty());
    assert(m_optimized_nodes.size() == nodes.size());

    m_optimized_nodes.swap(nodes);

    clear_release_memory(m_optimized_nodes);
}

template <typename Node>
void TreeOptimizer<Node>::optimize_subtree(
    const std::vector<Node>&        nodes,
    const size_t                    node_index,
    const size_t                    subtree_depth)
{
    Node& node = m_optimized_nodes[node_index];

    if (node.get_type() == Node::Leaf)
        return;

    const size_t old_child_index = node.get_child_node_index();
    const size_t new_child_index = m_optimized_nodes.size();

    node.set_child_node_index(new_child_index);

    m_optimized_nodes.push_back(nodes[old_child_index + 0]);
    m_optimized_nodes.push_back(nodes[old_child_index + 1]);

    if (subtree_depth > 1)
    {
        optimize_subtree(nodes, new_child_index + 0, subtree_depth - 1);
        optimize_subtree(nodes, new_child_index + 1, subtree_depth - 1);
    }
    else
    {
        m_roots.push(new_child_index + 0);
        m_roots.push(new_child_index + 1);
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_TREEOPTIMIZER_H
