
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_UTILITY_MEMORY_H
#define APPLESEED_FOUNDATION_UTILITY_MEMORY_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

//
// dynamic_sizeof() returns the size of a given object, including
// the dynamically allocated memory owned by the object.
//
// If dynamic_sizeof() is not implemented for a particular object
// type, it is equivalent to sizeof(). Like for sizeof(), the size
// of an object is expressed as a multiple of the size of a char.
//

template <typename T>
size_t dynamic_sizeof(const T& object);

template <typename T>
size_t dynamic_sizeof(T* object);


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
void* aligned_malloc(const size_t size, const size_t alignment);

// Free a block of memory that was allocated with aligned_malloc().
void aligned_free(void* aligned_ptr);


//
// STL containers related functions.
//

// Ensure that a container has a minimum given size.
template <typename Vector>
void ensure_size(
    Vector&         vector,
    const size_t    minimum_size);
template <typename Vector, typename T>
void ensure_size(
    Vector&         vector,
    const size_t    minimum_size,
    const T&        value);

// Clear a container and release the memory.
template <typename Container>
void clear_release_memory(Container& container);

// Clear a container but keep memory allocated.
template <typename Container>
void clear_keep_memory(Container& container);


//
// dynamic_sizeof() functions implementation.
//

template <typename T>
inline size_t dynamic_sizeof(const T& object)
{
    return sizeof(object);
}

template <typename T>
inline size_t dynamic_sizeof(T* object)
{
    return dynamic_sizeof(*object);
}


//
// Memory-alignment related functions implementation.
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

template <typename Vector>
inline void ensure_size(
    Vector&         vector,
    const size_t    minimum_size)
{
    if (vector.size() < minimum_size)
        vector.resize(minimum_size);
}

template <typename Vector, typename T>
inline void ensure_size(
    Vector&         vector,
    const size_t    minimum_size,
    const T&        value)
{
    if (vector.size() < minimum_size)
        vector.resize(minimum_size, value);
}

template <typename Container>
inline void clear_release_memory(Container& container)
{
    Container().swap(container);

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

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_MEMORY_H
