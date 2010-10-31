
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_KNN_KNN_ANSWER_H
#define APPLESEED_FOUNDATION_MATH_KNN_KNN_ANSWER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

DECLARE_TEST_CASE(Foundation_Math_Knn_Answer, Size_AfterOneInsertion_ReturnsOne);
DECLARE_TEST_CASE(Foundation_Math_Knn_Answer, Empty_AfterOneInsertion_ReturnsFalse);
DECLARE_TEST_CASE(Foundation_Math_Knn_Answer, Clear_GivenOneItem_EmptiesAnswer);
DECLARE_TEST_CASE(Foundation_Math_Knn_Answer, Sort_GivenFourItemsInSizeFiveAnswer_SortsItems);
DECLARE_TEST_CASE(Foundation_Math_Knn_Answer, BuildHeap_GivenVector_TransformsVectorToHeap);
DECLARE_TEST_CASE(Foundation_Math_Knn_Answer, Insert_GivenCloserItem_KeepsItem);

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
        size_t      m_index;
        ValueType   m_distance;

        void swap(Entry& rhs);

        bool operator<(const Entry& rhs) const;
    };

    explicit Answer(const size_t max_size);

    ~Answer();

    bool empty() const;

    size_t size() const;

    const Entry& get(const size_t i) const;

    void sort();

  private:
    template <typename, size_t> friend class Query;

    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Knn_Answer, Size_AfterOneInsertion_ReturnsOne);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Knn_Answer, Empty_AfterOneInsertion_ReturnsFalse);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Knn_Answer, Clear_GivenOneItem_EmptiesAnswer);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Knn_Answer, Sort_GivenFourItemsInSizeFiveAnswer_SortsItems);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Knn_Answer, BuildHeap_GivenVector_TransformsVectorToHeap);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Knn_Answer, Insert_GivenCloserItem_KeepsItem);

    const size_t    m_max_size;
    Entry*          m_entries;
    size_t          m_size;
    bool            m_heap;

    void clear();

    void insert(const size_t index, const ValueType distance);

    void build_heap();

    void heapify(const size_t i, const size_t end);

    const Entry& top() const;
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
    return m_distance < rhs.m_distance;
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
    delete [] m_entries;
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
inline const typename Answer<T>::Entry& Answer<T>::get(const size_t i) const
{
    assert(i < m_size);
    return m_entries[i];
}

template <typename T>
inline void Answer<T>::sort()
{
/*
    if (m_heap)
    {
        size_t i = m_size;

        while (i-- > 1)
        {
            m_entries[0].swap(m_entries[i]);
            heapify(0, i);
        }
    }
    else std::sort(m_entries, m_entries + m_size);
*/
    std::sort(m_entries, m_entries + m_size);
}

template <typename T>
inline void Answer<T>::clear()
{
    m_size = 0;
    m_heap = false;
}

template <typename T>
inline void Answer<T>::insert(const size_t index, const ValueType distance)
{
    if (m_size < m_max_size)
    {
        Entry& entry = m_entries[m_size++];
        entry.m_index = index;
        entry.m_distance = distance;
    }
    else
    {
        if (!m_heap)
            build_heap();

        if (distance < m_entries->m_distance)
        {
            m_entries->m_index = index;
            m_entries->m_distance = distance;
            heapify(0, m_size);
        }
    }
}

template <typename T>
inline void Answer<T>::build_heap()
{
    assert(!empty());
    assert(!m_heap);

    size_t i = m_size / 2;

    while (i--)
        heapify(i, m_size);

    m_heap = true;
}

template <typename T>
inline void Answer<T>::heapify(const size_t i, const size_t end)
{
    const size_t left = 2 * i + 1;
    const size_t right = left + 1;

    size_t largest = i;

    if (left < end && m_entries[i] < m_entries[left])
        largest = left;

    if (right < end && m_entries[largest] < m_entries[right])
        largest = right;

    if (largest != i)
    {
        m_entries[i].swap(m_entries[largest]);
        heapify(largest, end);
    }
}

template <typename T>
inline const typename Answer<T>::Entry& Answer<T>::top() const
{
    assert(!empty());
    assert(m_heap);

    return m_entries[0];
}

}       // namespace knn
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_KNN_KNN_ANSWER_H
