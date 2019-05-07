
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
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

namespace foundation {
namespace knn {

//
// A binary max-heap.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/Binary_heap
//

template <typename T>
class Answer
  : public NonCopyable
{
  public:
    typedef T ValueType;

    struct Entry
    {
        size_t          m_index;
        ValueType       m_square_dist;

        void swap(Entry& rhs);

        bool operator<(const Entry& rhs) const;
    };

    explicit Answer(const size_t max_size);

    ~Answer();

    bool empty() const;

    size_t size() const;

    void clear();

    void array_insert(
        const size_t    index,
        const ValueType square_dist);

    void make_heap();

    void heap_insert(
        const size_t    index,
        const ValueType square_dist);

    void sort();

    const Entry& get(const size_t i) const;

    const Entry& top() const;

  private:
    template <typename, size_t> friend class Query;

    const size_t        m_max_size;
    Entry*              m_entries;
    size_t              m_size;

#ifndef NDEBUG
    bool                m_heap;
#endif

    void heapify(const size_t index);
};


//
// Implementation.
//

template <typename T>
inline void Answer<T>::Entry::swap(Entry& rhs)
{
    Entry tmp = *this;
    *this = rhs;
    rhs = tmp;
}

template <typename T>
inline bool Answer<T>::Entry::operator<(const Entry& rhs) const
{
    return m_square_dist < rhs.m_square_dist;
}

template <typename T>
inline Answer<T>::Answer(const size_t max_size)
  : m_max_size(max_size)
  , m_entries(new Entry[max_size])
{
    clear();
}

template <typename T>
inline Answer<T>::~Answer()
{
    delete[] m_entries;
}

template <typename T>
inline bool Answer<T>::empty() const
{
    return m_size == 0;
}

template <typename T>
inline size_t Answer<T>::size() const
{
    return m_size;
}

template <typename T>
inline void Answer<T>::clear()
{
    m_size = 0;

#ifndef NDEBUG
    m_heap = false;
#endif
}

template <typename T>
inline void Answer<T>::array_insert(
    const size_t        index,
    const ValueType     square_dist)
{
    assert(m_size < m_max_size);
    assert(!m_heap);

    Entry& entry = m_entries[m_size++];
    entry.m_index = index;
    entry.m_square_dist = square_dist;
}

template <typename T>
inline void Answer<T>::make_heap()
{
    assert(m_size == m_max_size);
    assert(!m_heap);

    size_t i = m_size / 2;

    while (i--)
        heapify(i);

#ifndef NDEBUG
    m_heap = true;
#endif
}

template <typename T>
inline void Answer<T>::heap_insert(
    const size_t        index,
    const ValueType     square_dist)
{
    assert(m_heap);

    m_entries->m_index = index;
    m_entries->m_square_dist = square_dist;

    heapify(0);
}

template <typename T>
inline void Answer<T>::sort()
{
    if (m_size > 1)
        std::sort(m_entries, m_entries + m_size);
}

template <typename T>
inline const typename Answer<T>::Entry& Answer<T>::get(const size_t i) const
{
    assert(i < m_size);

    return m_entries[i];
}

template <typename T>
inline const typename Answer<T>::Entry& Answer<T>::top() const
{
    assert(m_heap);

    return m_entries[0];
}

template <typename T>
inline void Answer<T>::heapify(const size_t index)
{
    size_t i = index;

    while (true)
    {
        const size_t left = 2 * i + 1;
        const size_t right = left + 1;

        size_t largest = i;

        if (left < m_size && m_entries[i] < m_entries[left])
           largest = left;

        if (right < m_size && m_entries[largest] < m_entries[right])
           largest = right;

        if (largest == i)
            return;

       m_entries[i].swap(m_entries[largest]);

       i = largest;
    }
}

}   // namespace knn
}   // namespace foundation
