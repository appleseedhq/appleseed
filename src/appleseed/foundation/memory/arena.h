
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
#include "foundation/core/exceptions/exception.h"
#include "foundation/memory/memory.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace foundation
{

//
// An arena is a temporary heap providing extremely cheap memory allocation.
//

class Arena
{
  public:
    Arena();

    void clear();

    void* allocate(const size_t size);

    template <typename T> T* allocate();
    template <typename T> T* allocate_noinit();

    const std::uint8_t* get_storage() const;

  private:
    enum { ArenaSize = 384 * 1024 };    // bytes

    APPLESEED_SIMD4_ALIGN std::uint8_t  m_storage[ArenaSize];
    const std::uint8_t*                 m_end;
    std::uint8_t*                       m_current;
};


//
// Arena class implementation.
//

inline Arena::Arena()
  : m_end(m_storage + ArenaSize)
  , m_current(m_storage)
{
}

inline void Arena::clear()
{
    m_current = m_storage;
}

inline void* Arena::allocate(const size_t size)
{
    if (m_current + size > m_end)
        throw Exception("out of arena memory");

    void* ptr = m_current;
    m_current += align(size, 16);

    assert(is_aligned(ptr, 16));

    return ptr;
}

template <typename T>
inline T* Arena::allocate()
{
    T* ptr = static_cast<T*>(allocate(sizeof(T)));

    new (ptr) T();

    return ptr;
}

template <typename T>
inline T* Arena::allocate_noinit()
{
    return static_cast<T*>(allocate(sizeof(T)));
}

inline const std::uint8_t* Arena::get_storage() const
{
    return m_storage;
}

}   // namespace foundation
