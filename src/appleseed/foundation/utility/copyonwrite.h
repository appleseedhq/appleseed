
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

#ifndef APPLESEED_FOUNDATION_UTILITY_COPYONWRITE_H
#define APPLESEED_FOUNDATION_UTILITY_COPYONWRITE_H

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Standard headers.
#include <algorithm>
#include <atomic>
#include <cassert>
#include <utility>

namespace foundation
{

//
// A copy-on-write wrapper for any type that models RegularType.
// This class is based on stlab's copy-on-write class:
// https://github.com/stlab/libraries
//

template <typename T> // T models RegularType.
class CopyOnWrite
{
  public:
    // Constructors.
    CopyOnWrite()
      : m_model(nullptr)
    {
    }

    explicit CopyOnWrite(const T& x)
      : m_model(new Model(x))
    {
    }

    explicit CopyOnWrite(T&& x)
      : m_model(new Model(std::forward<T>(x)))
    {
    }

    // Destructor.
    ~CopyOnWrite()
    {
        if (m_model && (--m_model->m_count == 0))
            delete m_model;
    }

    // Copy constructor.
    CopyOnWrite(const CopyOnWrite& other)
      : m_model(other.m_model)
    {
        assert(m_model);
        m_model->m_count++;
    }

    // Move constructor.
    CopyOnWrite(CopyOnWrite&& other)
      : m_model(other.m_model)
    {
        other.m_model = nullptr;
    }

    // Copy assignment.
    CopyOnWrite& operator=(const CopyOnWrite& other)
    {
        CopyOnWrite tmp(other);
        swap(*this, tmp);
        return *this;
    }

    // Move assignment.
    CopyOnWrite& operator=(CopyOnWrite&& other)
    {
        CopyOnWrite tmp(std::move(other));
        swap(*this, tmp);
        return *this;
    }

    friend inline void swap(CopyOnWrite& x, CopyOnWrite& y)
    {
        std::swap(x.m_model, y.m_model);
    }

    // Return true if the wrapper is empty.
    bool empty() const
    {
        return m_model == nullptr;
    }

    // Return true if the wrapped object is not shared
    // by other copy on write wrappers.
    bool unique() const
    {
        assert(m_model);
        return m_model->m_count == 1;
    }

    // Return a const reference to the wrapped object.
    const T& read() const
    {
        assert(m_model);
        return m_model->m_value;
    }

    // Return a non const reference to the wrapped object.
    // If the object is shared, this method will make a copy of it.
    T& write()
    {
        assert(m_model);

        if (!unique())
            *this = CopyOnWrite(read());

        return m_model->m_value;
    }

  private:
    struct Model
    {
        explicit Model(const T& x)
          : m_value(x)
          , m_count(1)
        {
        }

        explicit Model(T&& x)
          : m_value(x)
          , m_count(1)
        {
        }

        T                        m_value;
        std::atomic<std::size_t> m_count;
    };

    Model* m_model;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_COPYONWRITE_H
