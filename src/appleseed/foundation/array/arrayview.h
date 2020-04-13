
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

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

//
// Typed const view of an array contents.
//

template <typename T>
class ArrayView
{
  public:
    typedef T value_type;

    // Constructor.
    explicit ArrayView(const Array& array)
      : m_begin(reinterpret_cast<const T*>(array.begin()))
      , m_end  (reinterpret_cast<const T*>(array.end()))
    {
        assert(array.type() == ArrayTraits<T>::array_type());
    }

    // Return true if the array is empty.
    bool empty() const
    {
        return m_begin == m_end;
    }

    // Return the number of items in the array.
    size_t size() const
    {
        return m_end - m_begin;
    }

    // Return the size in bytes of an item in the array.
    size_t item_size() const
    {
        return sizeof(T);
    }

    // Return a pointer to the beginning of the array.
    const T* begin() const
    {
        return m_begin;
    }

    // Return a pointer to the end of the array.
    const T* end() const
    {
        return m_end;
    }

    // Array element access.
    const T& operator[](const size_t i) const
    {
        assert(i < size());
        return begin()[i];
    }

  protected:
    const T*    m_begin;
    const T*    m_end;
};

}       // namespace foundation
