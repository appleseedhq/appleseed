
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/array/array.h"
#include "foundation/array/arrayview.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#if APPLESEED_COMPILER_CXX_GENERALIZED_INITIALIZERS
#include <initializer_list>
#endif

namespace foundation
{

//
// Typed non-const reference to an array.
//

template <typename T>
class ArrayRef
{
  public:
    typedef T value_type;
    typedef value_type& reference;
    typedef const value_type& const_reference;
    typedef size_t size_type;

    // Constructor.
    explicit ArrayRef(Array& array)
      : m_array(const_cast<Array&>(array))
    {
    }

    // Implicit conversion to ArrayView.
    operator const ArrayView<T>() const
    {
        return ArrayView<T>(m_array);
    }

    // Return true if the array is empty.
    bool empty() const
    {
        return m_array.empty();
    }

    // Return the number of items in the array.
    size_t size() const
    {
        return m_array.size();
    }

    // Return the size in bytes of an item in the array.
    size_t item_size() const
    {
        return sizeof(T);
    }

    // Return a pointer to the beginning of the array.
    const T* begin() const
    {
        return reinterpret_cast<const T*>(m_array.begin());
    }

    // Return a pointer to the end of the array.
    const T* end() const
    {
        return reinterpret_cast<const T*>(m_array.end());
    }

    // Return a pointer to the beginning of the array.
    T* begin()
    {
        return reinterpret_cast<T*>(m_array.begin());
    }

    // Return a pointer to the end of the array.
    T* end()
    {
        return reinterpret_cast<T*>(m_array.end());
    }

    // Array element access.
    const T& operator[](const size_t i) const
    {
        assert(i < size());
        return begin()[i];
    }

    T& operator[](const size_t i)
    {
        assert(i < size());
        return begin()[i];
    }

    // Remove all items from the array.
    void clear()
    {
        m_array.clear();
    }

    // Return the capacity of the array.
    size_t capacity() const
    {
        return m_array.capacity();
    }

    // Reserve memory for n items.
    void reserve(const size_t n)
    {
        m_array.reserve(n);
    }

    // Resize the array to n items.
    void resize(const size_t n)
    {
        m_array.resize(n);
    }

    // Remove excess capacity from the array.
    void shrink_to_fit()
    {
        m_array.shrink_to_fit();
    }

    // Add new items to the array.
    void push_back(const T& x)
    {
        m_array.push_back(&x);
    }

#if APPLESEED_COMPILER_CXX_GENERALIZED_INITIALIZERS
    // Add new items to the array.
    void push_back(const std::initializer_list<T>& l)
    {
        for (const auto& x : l)
            m_array.push_back(&x);
    }
#endif

    void fill(const size_t size, const T& value)
    {
        m_array.resize(size);
        std::fill(begin(), end(), value);
    }

    template <typename Iterator>
    void assign(Iterator first, Iterator last)
    {
        m_array.clear();
        m_array.reserve(std::distance(first, last));
        std::copy(first, last, std::back_inserter(*this));
    }

#if APPLESEED_COMPILER_CXX_GENERALIZED_INITIALIZERS
    void assign(std::initializer_list<T> l)
    {
        assign(l.begin(), l.end());
    }
#endif

  private:
    Array& m_array;
};

}       // namespace foundation
