
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

template <typename AABB>
class APPLESEED_ALIGN(64) Node
{
  public:
    typedef AABB AABBType;

    // Set/get the node type.
    void make_interior();
    void make_leaf();
    bool is_interior() const;
    bool is_leaf() const;

    // Set/get the bounding boxes of the child nodes (interior nodes only, static case).
    void set_left_bbox(const AABBType& bbox);
    void set_right_bbox(const AABBType& bbox);
    AABBType get_left_bbox() const;
    AABBType get_right_bbox() const;

    // Set/get the bounding boxes of the child nodes (interior nodes only, motion case).
    void set_left_bbox_index(const size_t index);
    void set_left_bbox_count(const size_t count);
    void set_right_bbox_index(const size_t index);
    void set_right_bbox_count(const size_t count);
    size_t get_left_bbox_index() const;
    size_t get_left_bbox_count() const;
    size_t get_right_bbox_index() const;
    size_t get_right_bbox_count() const;

    // Access user data (leaf nodes only).
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
    template <typename Tree, typename Visitor, typename Ray, size_t StackSize, size_t N>
    friend class Intersector;

    typedef typename AABBType::ValueType ValueType;
    static const size_t Dimension = AABBType::Dimension;

    uint32                          m_item_count;
    uint32                          m_index;
    uint32                          m_left_bbox_index;
    uint32                          m_left_bbox_count;
    uint32                          m_right_bbox_index;
    uint32                          m_right_bbox_count;

    APPLESEED_SIMD4_ALIGN ValueType m_bbox_data[4 * Dimension];
};


//
// Node class implementation.
//

template <typename AABB>
inline void Node<AABB>::make_interior()
{
    m_item_count = ~uint32(0);
}

template <typename AABB>
inline void Node<AABB>::make_leaf()
{
    if (m_item_count == ~uint32(0))
        m_item_count = 0;
}

template <typename AABB>
inline bool Node<AABB>::is_interior() const
{
    return m_item_count == ~uint32(0);
}

template <typename AABB>
inline bool Node<AABB>::is_leaf() const
{
    return m_item_count != ~uint32(0);
}

template <typename AABB>
inline void Node<AABB>::set_left_bbox(const AABBType& bbox)
{
    assert(is_interior());

    for (size_t i = 0; i < Dimension; ++i)
    {
        m_bbox_data[i * 4 + 0] = bbox.min[i];
        m_bbox_data[i * 4 + 2] = bbox.max[i];
    }
}

template <typename AABB>
inline void Node<AABB>::set_right_bbox(const AABBType& bbox)
{
    assert(is_interior());

    for (size_t i = 0; i < Dimension; ++i)
    {
        m_bbox_data[i * 4 + 1] = bbox.min[i];
        m_bbox_data[i * 4 + 3] = bbox.max[i];
    }
}

template <typename AABB>
inline AABB Node<AABB>::get_left_bbox() const
{
    assert(is_interior());

    AABBType bbox;

    for (size_t i = 0; i < Dimension; ++i)
    {
        bbox.min[i] = m_bbox_data[i * 4 + 0];
        bbox.max[i] = m_bbox_data[i * 4 + 2];
    }

    return bbox;
}

template <typename AABB>
inline AABB Node<AABB>::get_right_bbox() const
{
    assert(is_interior());

    AABBType bbox;

    for (size_t i = 0; i < Dimension; ++i)
    {
        bbox.min[i] = m_bbox_data[i * 4 + 1];
        bbox.max[i] = m_bbox_data[i * 4 + 3];
    }

    return bbox;
}

template <typename AABB>
inline void Node<AABB>::set_left_bbox_index(const size_t index)
{
    assert(is_interior());
    assert(index <= 0xFFFFFFFFUL);
    m_left_bbox_index = static_cast<uint32>(index);
}

template <typename AABB>
inline void Node<AABB>::set_left_bbox_count(const size_t count)
{
    assert(is_interior());
    assert(count <= 0xFFFFFFFFUL);
    m_left_bbox_count = static_cast<uint32>(count);
}

template <typename AABB>
inline void Node<AABB>::set_right_bbox_index(const size_t index)
{
    assert(is_interior());
    assert(index <= 0xFFFFFFFFUL);
    m_right_bbox_index = static_cast<uint32>(index);
}

template <typename AABB>
inline void Node<AABB>::set_right_bbox_count(const size_t count)
{
    assert(is_interior());
    assert(count <= 0xFFFFFFFFUL);
    m_right_bbox_count = static_cast<uint32>(count);
}

template <typename AABB>
inline size_t Node<AABB>::get_left_bbox_index() const
{
    assert(is_interior());
    return static_cast<uint32>(m_left_bbox_index);
}

template <typename AABB>
inline size_t Node<AABB>::get_left_bbox_count() const
{
    assert(is_interior());
    return static_cast<uint32>(m_left_bbox_count);
}

template <typename AABB>
inline size_t Node<AABB>::get_right_bbox_index() const
{
    assert(is_interior());
    return static_cast<uint32>(m_right_bbox_index);
}

template <typename AABB>
inline size_t Node<AABB>::get_right_bbox_count() const
{
    assert(is_interior());
    return static_cast<uint32>(m_right_bbox_count);
}

#define MAX_USER_DATA_SIZE (4 * Node<AABB>::Dimension * sizeof(typename AABB::ValueType))

template <typename AABB>
const size_t Node<AABB>::MaxUserDataSize = MAX_USER_DATA_SIZE;

template <typename AABB>
template <typename U>
inline void Node<AABB>::set_user_data(const U& data)
{
    assert(is_leaf());
    get_user_data<U>() = data;
}

template <typename AABB>
template <typename U>
inline const U& Node<AABB>::get_user_data() const
{
    static_assert(sizeof(U) <= MAX_USER_DATA_SIZE, "Not enough space in BVH node for user data");
    assert(is_leaf());
    return *reinterpret_cast<const U*>(m_bbox_data);
}

template <typename AABB>
template <typename U>
inline U& Node<AABB>::get_user_data()
{
    static_assert(sizeof(U) <= MAX_USER_DATA_SIZE, "Not enough space in BVH node for user data");
    assert(is_leaf());
    return *reinterpret_cast<U*>(m_bbox_data);
}

#undef MAX_USER_DATA_SIZE

template <typename AABB>
inline void Node<AABB>::set_child_node_index(const size_t index)
{
    assert(is_interior());
    assert(index <= 0xFFFFFFFFUL);
    m_index = static_cast<uint32>(index);
}

template <typename AABB>
inline size_t Node<AABB>::get_child_node_index() const
{
    assert(is_interior());
    return static_cast<size_t>(m_index);
}

template <typename AABB>
inline void Node<AABB>::set_item_index(const size_t index)
{
    assert(is_leaf());
    assert(index <= 0xFFFFFFFFUL);
    m_index = static_cast<uint32>(index);
}

template <typename AABB>
inline size_t Node<AABB>::get_item_index() const
{
    assert(is_leaf());
    return static_cast<size_t>(m_index);
}

template <typename AABB>
inline void Node<AABB>::set_item_count(const size_t count)
{
    assert(is_leaf());
    assert(count < 0xFFFFFFFFUL);
    m_item_count = static_cast<uint32>(count);
}

template <typename AABB>
inline size_t Node<AABB>::get_item_count() const
{
    assert(is_leaf());
    return static_cast<size_t>(m_item_count);
}

}   // namespace bvh
}   // namespace foundation
