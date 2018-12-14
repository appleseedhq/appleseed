
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
#include "foundation/platform/types.h"
#include "foundation/utility/casts.h"
#include "foundation/utility/typetraits.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation {
namespace bsp {

//
// Node (leaf node or interior node) of a BSP tree.
//

template <typename T>
class Node
{
  public:
    // Value type.
    typedef T ValueType;

    // Set/get the node type.
    void make_interior();
    void make_leaf();
    bool is_interior() const;
    bool is_leaf() const;

    // Set/get the child node index (interior nodes only).
    void set_child_node_index(const size_t index);
    size_t get_child_node_index() const;

    // Set/get the splitting dimension (interior nodes only).
    void set_split_dim(const size_t dim);
    size_t get_split_dim() const;

    // Set/get the splitting abscissa (interior nodes only).
    void set_split_abs(const ValueType abscissa);
    ValueType get_split_abs() const;

    // Set/get the leaf index (leaf nodes only).
    void set_leaf_index(const size_t index);
    size_t get_leaf_index() const;

    // Set/get the leaf size (leaf nodes only).
    void set_leaf_size(const size_t size);
    size_t get_leaf_size() const;

  private:

    //
    // The info field of the node is organized as follow:
    //
    //   interior node:
    //
    //     bits 0-1     splitting dimension
    //     bits 2-30    child node index
    //     bit  31      node type (1 for interior node)
    //
    //   leaf node:
    //
    //     bits 0-30    leaf index
    //     bit  31      node type (0 for leaf node)
    //
    // The maximum size of a single BSP tree is 2^29-1 = 536,870,912 nodes
    // and 2^31 = 2,147,483,648 items.
    //

    ValueType   m_abscissa;
    uint32      m_info;
};


//
// Node class implementation.
//

template <typename T>
inline void Node<T>::make_interior()
{
    m_info |= 0x80000000UL;
}

template <typename T>
inline void Node<T>::make_leaf()
{
    m_info &= 0x7FFFFFFFUL;
}

template <typename T>
inline bool Node<T>::is_interior() const
{
    return (m_info & 0x80000000UL) != 0;
}

template <typename T>
inline bool Node<T>::is_leaf() const
{
    return (m_info & 0x80000000UL) == 0;
}

template <typename T>
inline void Node<T>::set_child_node_index(const size_t index)
{
    assert(is_interior());
    assert(index < (1UL << 29));
    m_info &= 0x80000003UL;
    m_info |= static_cast<uint32>(index) << 2;
}

template <typename T>
inline size_t Node<T>::get_child_node_index() const
{
    assert(is_interior());
    return static_cast<size_t>((m_info & 0x7FFFFFFFUL) >> 2);
}

template <typename T>
inline void Node<T>::set_split_dim(const size_t dim)
{
    assert(is_interior());
    assert(dim < 4);
    m_info &= 0xFFFFFFFCUL;
    m_info |= static_cast<uint32>(dim);
}

template <typename T>
inline size_t Node<T>::get_split_dim() const
{
    assert(is_interior());
    return static_cast<size_t>(m_info & 0x00000003UL);
}

template <typename T>
inline void Node<T>::set_split_abs(const ValueType abscissa)
{
    assert(is_interior());
    m_abscissa = abscissa;
}

template <typename T>
inline T Node<T>::get_split_abs() const
{
    assert(is_interior());
    return m_abscissa;
}

template <typename T>
inline void Node<T>::set_leaf_index(const size_t index)
{
    assert(is_leaf());
    assert(index < (1UL << 31));
    m_info &= 0x80000000UL;
    m_info |= static_cast<uint32>(index);
}

template <typename T>
inline size_t Node<T>::get_leaf_index() const
{
    assert(is_leaf());
    return static_cast<size_t>(m_info & 0x7FFFFFFFUL);
}

template <typename T>
inline void Node<T>::set_leaf_size(const size_t size)
{
    assert(is_leaf());
    typedef typename TypeConv<T>::UInt UInt;
    m_abscissa = binary_cast<T>(static_cast<UInt>(size));
}

template <typename T>
inline size_t Node<T>::get_leaf_size() const
{
    assert(is_leaf());
    typedef typename TypeConv<T>::UInt UInt;
    return static_cast<size_t>(binary_cast<UInt>(m_abscissa));
}

}   // namespace bsp
}   // namespace foundation
