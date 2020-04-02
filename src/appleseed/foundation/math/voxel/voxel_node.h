
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

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace foundation {
namespace voxel {

//
// Node (leaf node or interior node) of a voxel tree.
//
// By convention, a splitting plane always belongs to the right node only.
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

    // Set/get the solid bit.
    void set_solid_bit(const bool solid);
    bool is_empty() const;
    bool is_solid() const;

    // Set/get the child node index (interior nodes only).
    void set_child_node_index(const size_t index);
    size_t get_child_node_index() const;

    // Set/get the splitting dimension (interior nodes only).
    void set_split_dim(const size_t dim);
    size_t get_split_dim() const;

    // Set/get the splitting abscissa (interior nodes only).
    void set_split_abs(const ValueType abscissa);
    ValueType get_split_abs() const;

  private:
    template <typename T_, size_t N>
    friend class Tree;

    //
    // The info field of the node is organized as follow:
    //
    //   interior node:
    //
    //     bits 0-1     splitting dimension (00=x, 01=y, 10=z)
    //     bits 2-30    child node index
    //     bit  31      solid bit (0 for empty, 1 for solid)
    //
    //   leaf node:
    //
    //     bits 0-1     equal to 11 (indicating a leaf node)
    //     bits 2-30    unused
    //     bit  31      solid bit (0 for empty, 1 for solid)
    //
    // The maximum size of a single voxel tree is 2^29 = 536,870,912 nodes.
    //

    ValueType       m_abscissa;
    std::uint32_t   m_info;
};


//
// Node class implementation.
//

template <typename T>
inline void Node<T>::make_interior()
{
    if ((m_info & 0x00000003u) == 0x00000003u)
        m_info &= 0xFFFFFFFCu;
}

#if __GNUC__ >= 7
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif

    template <typename T>
    inline void Node<T>::make_leaf()
    {
        m_info |= 0x00000003u;
    }

#if __GNUC__ >= 7
#pragma GCC diagnostic pop
#endif

template <typename T>
inline bool Node<T>::is_interior() const
{
    return (m_info & 0x00000003u) != 0x00000003u;
}

template <typename T>
inline bool Node<T>::is_leaf() const
{
    return (m_info & 0x00000003u) == 0x00000003u;
}

template <typename T>
inline void Node<T>::set_solid_bit(const bool solid)
{
    if (solid)
         m_info |= 0x80000000u;
    else m_info &= 0x7FFFFFFFu;
}

template <typename T>
inline bool Node<T>::is_empty() const
{
    return (m_info & 0x80000000u) == 0;
}

template <typename T>
inline bool Node<T>::is_solid() const
{
    return (m_info & 0x80000000u) != 0;
}

template <typename T>
inline void Node<T>::set_child_node_index(const size_t index)
{
    assert(is_interior());
    assert(index < (1UL << 29));
    m_info &= 0x80000003u;
    m_info |= static_cast<std::uint32_t>(index) << 2;
}

template <typename T>
inline size_t Node<T>::get_child_node_index() const
{
    assert(is_interior());
    return static_cast<size_t>((m_info & 0x7FFFFFFCu) >> 2);
}

template <typename T>
inline void Node<T>::set_split_dim(const size_t dim)
{
    assert(is_interior());
    assert(dim < 4);
    m_info &= 0xFFFFFFFCu;
    m_info |= static_cast<std::uint32_t>(dim);
}

template <typename T>
inline size_t Node<T>::get_split_dim() const
{
    assert(is_interior());
    return static_cast<size_t>(m_info & 0x00000003u);
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

}   // namespace voxel
}   // namespace foundation
