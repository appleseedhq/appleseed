
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

// Standard headers.
#include <cstddef>
#include <vector>

namespace foundation {
namespace bvh {

//
// Bounding Volume Hierarchy (BVH).
//

template <typename NodeVector>
class Tree
{
  public:
    typedef NodeVector NodeVectorType;
    typedef Tree<NodeVectorType> TreeType;
    typedef typename NodeVectorType::value_type NodeType;
    typedef typename NodeVectorType::allocator_type AllocatorType;

    // Constructor.
    explicit Tree(const AllocatorType& allocator = AllocatorType());

    // Clear the tree.
    void clear();

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

  protected:
    template <typename Tree, typename Partitioner>
    friend class Builder;

    template <typename Tree, typename Partitioner>
    friend class SpatialBuilder;

    template <typename Tree>
    friend class TreeStatistics;

    template <typename Tree, typename Visitor, typename Ray, size_t StackSize, size_t N>
    friend class Intersector;

    typedef typename NodeType::AABBType AABBType;
    typedef std::vector<AABBType> AABBVector;

    NodeVector  m_nodes;
    AABBVector  m_node_bboxes;
};


//
// Tree class implementation.
//

template <typename NodeVector>
Tree<NodeVector>::Tree(const AllocatorType& allocator)
  : m_nodes(allocator)
{
    clear();
}

template <typename NodeVector>
void Tree<NodeVector>::clear()
{
    m_nodes.clear();
}

template <typename NodeVector>
size_t Tree<NodeVector>::get_memory_size() const
{
    return
          sizeof(*this)
        + m_nodes.capacity() * sizeof(NodeType);
}

}   // namespace bvh
}   // namespace foundation
