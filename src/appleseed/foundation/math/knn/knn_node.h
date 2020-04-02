
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
namespace knn {

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

    // Set/get the index of the first point.
    void set_point_index(const size_t index);
    size_t get_point_index() const;

    // Set/get the number of points.
    void set_point_count(const size_t size);
    size_t get_point_count() const;

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
    //     bits 0-30    point index
    //     bit  31      node type (0 for leaf node)
    //

    ValueType       m_abscissa;
    std::uint32_t   m_info;
    std::uint32_t   m_point_index;
    std::uint32_t   m_point_count;
};


//
// Node class implementation.
//

template <typename T>
inline void Node<T>::make_interior()
{
    m_info |= 0x80000000u;
}

template <typename T>
inline void Node<T>::make_leaf()
{
    m_info &= 0x7FFFFFFFu;
}

template <typename T>
inline bool Node<T>::is_interior() const
{
    return (m_info & 0x80000000u) != 0;
}

template <typename T>
inline bool Node<T>::is_leaf() const
{
    return (m_info & 0x80000000u) == 0;
}

template <typename T>
inline void Node<T>::set_child_node_index(const size_t index)
{
    assert(index < (1u << 29));
    m_info &= 0x80000003u;
    m_info |= static_cast<std::uint32_t>(index) << 2;
}

template <typename T>
inline size_t Node<T>::get_child_node_index() const
{
    return static_cast<size_t>((m_info & 0x7FFFFFFFu) >> 2);
}

template <typename T>
inline void Node<T>::set_split_dim(const size_t dim)
{
    assert(dim < 4);
    m_info &= 0xFFFFFFFCu;
    m_info |= static_cast<std::uint32_t>(dim);
}

template <typename T>
inline size_t Node<T>::get_split_dim() const
{
    return static_cast<size_t>(m_info & 0x00000003u);
}

template <typename T>
inline void Node<T>::set_split_abs(const ValueType abscissa)
{
    m_abscissa = abscissa;
}

template <typename T>
inline T Node<T>::get_split_abs() const
{
    return m_abscissa;
}

template <typename T>
inline void Node<T>::set_point_index(const size_t index)
{
    m_point_index = static_cast<std::uint32_t>(index);
}

template <typename T>
inline size_t Node<T>::get_point_index() const
{
    return static_cast<size_t>(m_point_index);
}

template <typename T>
inline void Node<T>::set_point_count(const size_t size)
{
    m_point_count = static_cast<std::uint32_t>(size);
}

template <typename T>
inline size_t Node<T>::get_point_count() const
{
    return static_cast<size_t>(m_point_count);
}

}   // namespace knn
}   // namespace foundation
