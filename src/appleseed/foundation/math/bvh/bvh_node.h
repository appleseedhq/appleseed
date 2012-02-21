
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_NODE_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_NODE_H

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/platform/compiler.h"
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
class ALIGN(64) Node
{
  public:
    // Value type and dimension.
    typedef T ValueType;
    static const size_t Dimension = N;

    // AABB type.
    typedef AABB<T, N> AABBType;

    // Set/get the node type.
    void make_interior();
    void make_leaf();
    bool is_interior() const;
    bool is_leaf() const;

    // Set/get the bounding boxes of the child nodes (interior nodes only).
    void set_left_bbox(const AABBType& bbox);
    void set_right_bbox(const AABBType& bbox);
    AABBType get_left_bbox() const;
    AABBType get_right_bbox() const;

    // Get user data (leaf nodes only).
    static const size_t MaxUserDataSize;
    template <typename U> void set_user_data(const U& data);
    template <typename U> const U& get_user_data() const;
    template <typename U> U& get_user_data();

    // Set/get the index of the first child node (interior nodes only).
    void set_child_node_index(const size_t index);
    size_t get_child_node_index() const;

    // Set/get the index of the first item (leaf nodes only).
    void set_item_index(const size_t index);
    size_t get_item_index() const;

    // Set/get the item count (leaf nodes only).
    void set_item_count(const size_t count);
    size_t get_item_count() const;

  private:
    template <
        typename T_,
        typename Tree,
        typename Visitor,
        size_t StackSize
    >
    friend class Intersector;

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
    // The maximum size of a single BVH is 2^31 = 2,147,483,648 nodes.
    //

    uint32                  m_info;
    uint32                  m_count;
    SSE_ALIGN ValueType     m_bbox_data[4 * N];
};


//
// Node class implementation.
//

template <typename T, size_t N>
inline void Node<T, N>::make_interior()
{
    m_count = ~0;
}

template <typename T, size_t N>
inline void Node<T, N>::make_leaf()
{
    if (m_count == ~0)
        m_count = 0;
}

template <typename T, size_t N>
inline bool Node<T, N>::is_interior() const
{
    return m_count == ~0;
}

template <typename T, size_t N>
inline bool Node<T, N>::is_leaf() const
{
    return m_count != ~0;
}

template <typename T, size_t N>
inline void Node<T, N>::set_left_bbox(const AABBType& bbox)
{
    for (size_t i = 0; i < N; ++i)
    {
        m_bbox_data[i * 4 + 0] = bbox.min[i];
        m_bbox_data[i * 4 + 2] = bbox.max[i];
    }
}

template <typename T, size_t N>
inline void Node<T, N>::set_right_bbox(const AABBType& bbox)
{
    for (size_t i = 0; i < N; ++i)
    {
        m_bbox_data[i * 4 + 1] = bbox.min[i];
        m_bbox_data[i * 4 + 3] = bbox.max[i];
    }
}

template <typename T, size_t N>
inline AABB<T, N> Node<T, N>::get_left_bbox() const
{
    AABBType bbox;

    for (size_t i = 0; i < N; ++i)
    {
        bbox.min[i] = m_bbox_data[i * 4 + 0];
        bbox.max[i] = m_bbox_data[i * 4 + 2];
    }

    return bbox;
}

template <typename T, size_t N>
inline AABB<T, N> Node<T, N>::get_right_bbox() const
{
    AABBType bbox;

    for (size_t i = 0; i < N; ++i)
    {
        bbox.min[i] = m_bbox_data[i * 4 + 1];
        bbox.max[i] = m_bbox_data[i * 4 + 3];
    }

    return bbox;
}

template <typename T, size_t N>
const size_t Node<T, N>::MaxUserDataSize = sizeof(Node<T, N>) - 8;

template <typename T, size_t N>
template <typename U>
inline void Node<T, N>::set_user_data(const U& data)
{
    get_user_data<U>() = data;
}

template <typename T, size_t N>
template <typename U>
inline const U& Node<T, N>::get_user_data() const
{
    assert(sizeof(U) <= MaxUserDataSize);               // todo: use static_assert<>
    return *reinterpret_cast<const U*>(m_bbox_data);
}

template <typename T, size_t N>
template <typename U>
inline U& Node<T, N>::get_user_data()
{
    assert(sizeof(U) <= MaxUserDataSize);               // todo: use static_assert<>
    return *reinterpret_cast<U*>(m_bbox_data);
}

template <typename T, size_t N>
inline void Node<T, N>::set_child_node_index(const size_t index)
{
    assert(index <= 0xFFFFFFFFUL);
    m_info = static_cast<uint32>(index);
}

template <typename T, size_t N>
inline size_t Node<T, N>::get_child_node_index() const
{
    return static_cast<size_t>(m_info);
}

template <typename T, size_t N>
inline void Node<T, N>::set_item_index(const size_t index)
{
    assert(index <= 0xFFFFFFFFUL);
    m_info = static_cast<uint32>(index);
}

template <typename T, size_t N>
inline size_t Node<T, N>::get_item_index() const
{
    return static_cast<size_t>(m_info);
}

template <typename T, size_t N>
inline void Node<T, N>::set_item_count(const size_t count)
{
    assert(count < 0xFFFFFFFFUL);
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
