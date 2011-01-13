
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_NODE_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_NODE_H

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation {
namespace bvh {

//
// Node (leaf node or interior node) of a BVH.
//

template <typename T, size_t N>
class Node
{
  public:
    // Value type and dimension.
    typedef T ValueType;
    static const size_t Dimension = N;

    // AABB type.
    typedef AABB<T, N> AABBType;

    // Node types.
    typedef uint32 Type;
    static const Type Leaf     = 0x00000000UL;
    static const Type Interior = 0x80000000UL;

    // Set/get the node type.
    void set_type(const Type type);
    Type get_type() const;
    bool is_interior() const;
    bool is_leaf() const;

    // Set/get the node bounding box.
    void set_bbox(const AABBType& bbox);
    const AABBType& get_bbox() const;

    // Set/get the child node index (interior nodes only).
    void set_child_node_index(const size_t index);
    size_t get_child_node_index() const;

    // Set/get the item index (leaf nodes only).
    void set_item_index(const size_t index);
    size_t get_item_index() const;

    // Set/get the item count (leaf nodes only).
    void set_item_count(const size_t count);
    size_t get_item_count() const;

  private:

    //
    // The info field of the node is organized as follow:
    //
    //   interior node:
    //
    //     bits 0-30    child node index
    //     bit  31      node type (1 for interior node)
    //
    //   leaf node:
    //
    //     bits 0-30    leaf index
    //     bit  31      node type (0 for leaf node)
    //
    // The maximum size of a single BSP tree is 2^29 = 536,870,912 nodes.
    //

    AABBType    m_bbox;
    uint32      m_info;
    uint32      m_count;
};


//
// Node class implementation.
//

// Set/get the node type.
template <typename T, size_t N>
inline void Node<T, N>::set_type(const Type type)
{
    assert(type == Leaf || type == Interior);
    m_info &= 0x7FFFFFFFUL;
    m_info |= type;
}
template <typename T, size_t N>
inline typename Node<T, N>::Type Node<T, N>::get_type() const
{
    return static_cast<Type>(m_info & 0x80000000UL);
}
template <typename T, size_t N>
inline bool Node<T, N>::is_interior() const
{
    return (m_info & 0x80000000UL) != 0;
}
template <typename T, size_t N>
inline bool Node<T, N>::is_leaf() const
{
    return (m_info & 0x80000000UL) == 0;
}

// Set/get the node bounding box.
template <typename T, size_t N>
inline void Node<T, N>::set_bbox(const AABBType& bbox)
{
    m_bbox = bbox;
}
template <typename T, size_t N>
inline const AABB<T, N>& Node<T, N>::get_bbox() const
{
    return m_bbox;
}

// Set/get the child node index (interior nodes only).
template <typename T, size_t N>
inline void Node<T, N>::set_child_node_index(const size_t index)
{
    assert(index < (1UL << 31));
    m_info &= 0x80000000UL;
    m_info |= static_cast<uint32>(index);
}
template <typename T, size_t N>
inline size_t Node<T, N>::get_child_node_index() const
{
    return static_cast<size_t>(m_info & 0x7FFFFFFFUL);
}

// Set/get the item index (leaf nodes only).
template <typename T, size_t N>
inline void Node<T, N>::set_item_index(const size_t index)
{
    assert(index < (1UL << 31));
    m_info &= 0x80000000UL;
    m_info |= static_cast<uint32>(index);
}
template <typename T, size_t N>
inline size_t Node<T, N>::get_item_index() const
{
    return static_cast<size_t>(m_info & 0x7FFFFFFFUL);
}

// Set/get the item count (leaf nodes only).
template <typename T, size_t N>
inline void Node<T, N>::set_item_count(const size_t count)
{
    assert(count <= 0xFFFFFFFFUL);
    m_count = static_cast<uint32>(count);
}
template <typename T, size_t N>
inline size_t Node<T, N>::get_item_count() const
{
    return static_cast<size_t>(m_count);
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_NODE_H
