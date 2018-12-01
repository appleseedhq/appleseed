
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
#include "foundation/platform/system.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

namespace foundation
{

//
// Thread local storage.
//
// This template class allows to create a thread local object.
//

template <typename T>
class TLS
{
  public:
    // Constructor.
    // thread_count is the number of threads having access to this object.
    explicit TLS(const size_t thread_count);

    // Set the number of threads having access to this object.
    void set_thread_count(const size_t thread_count);

    // Get the number of threads having access to this object.
    size_t get_thread_count() const;

    // Direct access to the storage area for a given thread.
    T& operator[](const size_t thread_index);
    const T& operator[](const size_t thread_index) const;

  private:
    size_t          m_thread_count;
    size_t          m_stride;
    std::vector<T>  m_elements;
};


//
// TLS class implementation.
//

template <typename T>
inline TLS<T>::TLS(const size_t thread_count)
{
    set_thread_count(thread_count);
}

template <typename T>
inline void TLS<T>::set_thread_count(const size_t thread_count)
{
    // Get cache line size (in bytes) of current CPU.
    // todo: pass ID of current CPU to get_l1_data_cache_line_size().
    const size_t cache_line_size = System::get_l1_data_cache_line_size();

    // Compute stride to avoid cache aliasing (false sharing).
    // todo: make stride a power of two, and use bit shifting.
    if (sizeof(T) < cache_line_size)
         m_stride = cache_line_size / sizeof(T);
    else m_stride = 1;

    m_elements.resize(thread_count * m_stride);
    m_thread_count = thread_count;
}

template <typename T>
inline size_t TLS<T>::get_thread_count() const
{
    return m_thread_count;
}

template <typename T>
inline T& TLS<T>::operator[](const size_t thread_index)
{
    assert(thread_index < m_thread_count);
    return m_elements[thread_index * m_stride];
}

template <typename T>
inline const T& TLS<T>::operator[](const size_t thread_index) const
{
    assert(thread_index < m_thread_count);
    return m_elements[thread_index * m_stride];
}

}   // namespace foundation
