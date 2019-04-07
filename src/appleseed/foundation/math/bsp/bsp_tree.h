
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
#include "foundation/math/aabb.h"
#include "foundation/math/bsp/bsp_node.h"

// Standard headers.
#include <cstddef>
#include <vector>

namespace foundation {
namespace bsp {

//
// Binary Space Partition (BSP) tree.
//
// The Leaf class must conform to the following prototype:
//
//      class Leaf
//        : public foundation::NonCopyable
//      {
//        public:
//          // Remove all items from the leaf.
//          void clear();
//
//          // Return the number of items in the leaf.
//          size_t get_size() const;
//
//          // Return the bounding box of the leaf.
//          foundation::AABB<ValueType, 3> get_bbox() const;
//
//          // Return the size (in bytes) of this object in memory.
//          size_t get_memory_size() const;
//      };
//

template <typename T, size_t N, typename Leaf>
class Tree
{
  public:
    // Value type and dimension.
    typedef T ValueType;
    static const size_t Dimension = N;

    // AABB, node, leaf and tree types.
    typedef AABB<T, N> AABBType;
    typedef Node<T> NodeType;
    typedef Leaf LeafType;
    typedef Tree<T, N, Leaf> TreeType;

    // Constructor.
    Tree();

    // Destructor.
    ~Tree();

    // Clear the tree.
    void clear();

    // Return the bounding box of the tree.
    const AABBType& get_bbox() const;

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

  protected:
    template <
        typename Tree,
        typename LeafFactory,
        typename LeafSplitter,
        typename Timer
    >
    friend class Builder;

    template <
        typename T_,
        typename Tree,
        typename Visitor,
        typename Ray,
        size_t S
    >
    friend class Intersector;

    template <typename Tree, typename Builder>
    friend class TreeStatistics;

    typedef std::vector<NodeType> NodeVector;
    typedef std::vector<LeafType*> LeafVector;

    AABBType    m_bbox;                             // bounding box of the tree
    NodeVector  m_nodes;                            // nodes of the tree
    LeafVector  m_leaves;                           // leaves of the tree
};


//
// Tree class implementation.
//

template <typename T, size_t N, typename Leaf>
Tree<T, N, Leaf>::Tree()
{
    m_bbox.invalidate();
}

template <typename T, size_t N, typename Leaf>
Tree<T, N, Leaf>::~Tree()
{
    clear();
}

template <typename T, size_t N, typename Leaf>
void Tree<T, N, Leaf>::clear()
{
    // Invalidate tree bounding box.
    m_bbox.invalidate();

    // Delete nodes.
    m_nodes.clear();

    // Delete leaves.
    for (size_t i = 0; i < m_leaves.size(); ++i)
        delete m_leaves[i];
    m_leaves.clear();
}

template <typename T, size_t N, typename Leaf>
inline const AABB<T, N>& Tree<T, N, Leaf>::get_bbox() const
{
    return m_bbox;
}

template <typename T, size_t N, typename Leaf>
size_t Tree<T, N, Leaf>::get_memory_size() const
{
    size_t mem_size = sizeof(*this);

    mem_size += m_nodes.capacity() * sizeof(NodeType);

    const size_t num_leaves = m_leaves.size();
    for (size_t i = 0; i < num_leaves; ++i)
        mem_size += m_leaves[i]->get_memory_size();

    mem_size += (m_leaves.capacity() - num_leaves) * sizeof(LeafType*);

    return mem_size;
}

}   // namespace bsp
}   // namespace foundation
