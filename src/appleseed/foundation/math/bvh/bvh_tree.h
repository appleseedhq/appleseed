
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_TREE_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_TREE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cstddef>
#include <memory>
#include <vector>

namespace foundation {
namespace bvh {

//
// Bounding Volume Hierarchy (BVH).
//

template <typename Node, typename Allocator = std::allocator<void> >
class Tree
  : public NonCopyable
{
  public:
    typedef Node NodeType;
    typedef Allocator AllocatorType;
    typedef Tree<NodeType, AllocatorType> TreeType;
    typedef std::vector<NodeType, AllocatorType> NodeVector;

    // Constructor.
    explicit Tree(const AllocatorType& allocator = AllocatorType());

    // Clear the tree.
    void clear();

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

  protected:
    template <typename Tree, typename Partitioner>
    friend class Builder;

    template <typename Tree, typename Builder>
    friend class TreeStatistics;

    template <typename Tree, typename Visitor, size_t StackSize>
    friend class Intersector;

    NodeVector m_nodes;
};


//
// Tree class implementation.
//

template <typename Node, typename Allocator>
Tree<Node, Allocator>::Tree(const AllocatorType& allocator)
  : m_nodes(allocator)
{
    clear();
}

template <typename Node, typename Allocator>
void Tree<Node, Allocator>::clear()
{
    m_nodes.clear();
}

template <typename Node, typename Allocator>
size_t Tree<Node, Allocator>::get_memory_size() const
{
    return
          sizeof(*this)
        + m_nodes.capacity() * sizeof(NodeType);
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_TREE_H
