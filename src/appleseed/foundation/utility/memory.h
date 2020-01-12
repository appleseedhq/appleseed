
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
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>

namespace foundation
{

//
// Memory-alignment related functions.
//

// Align a given pointer to a given boundary.
template <typename T>
T align(const T ptr, const size_t alignment);

// Compute the alignment of a given pointer.
template <typename T>
size_t alignment(const T ptr, const size_t max_alignment = 256);

// Return whether a given pointer is aligned on a given boundary.
template <typename T>
bool is_aligned(const T ptr, const size_t alignment);

// Allocate memory on a specified alignment boundary.
void* aligned_malloc(const size_t size, size_t alignment);

// Free a block of memory that was allocated with aligned_malloc().
void aligned_free(void* aligned_ptr);


//
// STL containers related functions.
//

// Ensure that a container has a minimum given size.
template <typename Container>
void ensure_minimum_size(
    Container&      container,
    const size_t    minimum_size);
template <typename Container, typename T>
void ensure_minimum_size(
    Container&      container,
    const size_t    minimum_size,
    const T&        value);

// Clear a container and release the memory.
template <typename Container>
void clear_release_memory(Container& container);

// Clear a container but keep memory allocated.
template <typename Container>
void clear_keep_memory(Container& container);

// Shrink the capacity of a container to its size.
template <typename Container>
void shrink_to_fit(Container& container);


//
// Utility classes to read/write typed data from/to unstructured memory blocks.
//

class MemoryReader
{
  public:
    explicit MemoryReader(const void* source);

    const void* read(const size_t size);

    template <typename T>
    const T& read();

    size_t offset() const;

    MemoryReader& operator+=(const isize_t offset);
    MemoryReader& operator-=(const isize_t offset);

  private:
    const std::uint8_t* const   m_base;
    const std::uint8_t*         m_ptr;
};

class MemoryWriter
{
  public:
    explicit MemoryWriter(void* dest);

    void write(const void* src, const size_t size);

    template <typename T>
    void write(const T& value);

    size_t offset() const;

  private:
    const std::uint8_t* const   m_base;
    std::uint8_t*               m_ptr;
};


//
// Memory-alignment related functions implementation.
//
// C-style casts below allow to cast `ptr` regardless of whether it is an integer or a pointer.
// Neither static_cast<>() nor reinterpret_cast<>() has this ability.
//

template <typename T>
inline T align(const T ptr, const size_t alignment)
{
    assert(alignment > 0);
    assert(is_pow2(alignment));

    const size_t a = alignment - 1;
    const uintptr_t aligned = ((uintptr_t)ptr + a) & ~a;

    return (T)aligned;
}

template <typename T>
size_t alignment(const T ptr, const size_t max_alignment)
{
    assert(max_alignment > 0);
    assert(is_pow2(max_alignment));

    const uintptr_t p = (uintptr_t)ptr;
    size_t a = 1;

    while (a < max_alignment && (p & a) == 0)
        a <<= 1;

    return a;
}

template <typename T>
inline bool is_aligned(const T ptr, const size_t alignment)
{
    assert(alignment > 0);
    assert(is_pow2(alignment));

    const uintptr_t p = (uintptr_t)ptr;
    return (p & (alignment - 1)) == 0;
}


//
// STL containers related functions implementation.
//

template <typename Container>
inline void ensure_minimum_size(
    Container&      container,
    const size_t    minimum_size)
{
    if (container.size() < minimum_size)
        container.resize(minimum_size);
}

template <typename Container, typename T>
inline void ensure_minimum_size(
    Container&      container,
    const size_t    minimum_size,
    const T&        value)
{
    if (container.size() < minimum_size)
        container.resize(minimum_size, value);
}

template <typename Container>
inline void clear_release_memory(Container& container)
{
    Container(container.get_allocator()).swap(container);

    assert(container.empty());
}

template <typename Container>
inline void clear_keep_memory(Container& container)
{
#ifndef NDEBUG
    const size_t old_capacity = container.capacity();
#endif

    container.erase(container.begin(), container.end());

    assert(container.capacity() == old_capacity);
}

template <typename Container>
inline void shrink_to_fit(Container& container)
{
    Container(container).swap(container);
}


//
// MemoryReader class implementation.
//

inline MemoryReader::MemoryReader(const void* source)
  : m_base(reinterpret_cast<const std::uint8_t*>(source))
  , m_ptr(reinterpret_cast<const std::uint8_t*>(source))
{
}

inline const void* MemoryReader::read(const size_t size)
{
    const void* result = m_ptr;
    m_ptr += size;
    return result;
}

template <typename T>
inline const T& MemoryReader::read()
{
    return *reinterpret_cast<const T*>(read(sizeof(T)));
}

inline size_t MemoryReader::offset() const
{
    return m_ptr - m_base;
}

inline MemoryReader& MemoryReader::operator+=(const isize_t offset)
{
    m_ptr += offset;
    return *this;
}

inline MemoryReader& MemoryReader::operator-=(const isize_t offset)
{
    m_ptr -= offset;
    return *this;
}


//
// MemoryWriter class implementation.
//

inline MemoryWriter::MemoryWriter(void* dest)
  : m_base(reinterpret_cast<std::uint8_t*>(dest))
  , m_ptr(reinterpret_cast<std::uint8_t*>(dest))
{
}

inline void MemoryWriter::write(const void* src, const size_t size)
{
    std::memcpy(m_ptr, src, size);
    m_ptr += size;
}

template <typename T>
inline void MemoryWriter::write(const T& value)
{
    write(&value, sizeof(T));
}

inline size_t MemoryWriter::offset() const
{
    return m_ptr - m_base;
}

}   // namespace foundation
