
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/memory/memory.h"

// Standard headers.
#include <cstddef>
#include <limits>
#include <new>

namespace foundation
{

//
// A standard-conformant allocator allocating aligned, padded memory.
//

template <typename T>
class ArrayAllocator
{
  public:
    typedef T                   value_type;
    typedef value_type*         pointer;
    typedef const value_type*   const_pointer;
    typedef value_type&         reference;
    typedef const value_type&   const_reference;
    typedef size_t              size_type;
    typedef std::ptrdiff_t      difference_type;

    // Alignment and padding must be powers of 2.
    static const size_t Alignment = 16;
    static const size_t Padding = 16;

    template <typename U>
    struct rebind
    {
        typedef ArrayAllocator<U> other;
    };

    ArrayAllocator() = default;

    template <typename U>
    ArrayAllocator(const ArrayAllocator<U>&)
    {
    }

    bool operator==(const ArrayAllocator<T>&) const
    {
        return true;
    }

    template <typename U>
    bool operator==(const ArrayAllocator<U>&) const
    {
        return false;
    }

    template <typename U>
    bool operator!=(const ArrayAllocator<U>&) const
    {
        return true;
    }

    pointer address(reference x) const
    {
        return &x;
    }

    const_pointer address(const_reference x) const
    {
        return &x;
    }

    pointer allocate(size_type n, const_pointer hint = nullptr)
    {
        if (n == 0)
            return nullptr;

        const size_type padded_size = next_multiple(n * sizeof(T), Padding);
        pointer p = static_cast<pointer>(aligned_malloc(padded_size, Alignment));

        if (p == nullptr)
             throw std::bad_alloc();

        return p;
    }

    void deallocate(pointer p, size_type n)
    {
        if (p)
            aligned_free(p);
    }

    size_type max_size() const
    {
        return std::numeric_limits<size_type>::max() / sizeof(T);
    }

    void construct(pointer p, const_reference x)
    {
        new(p) value_type(x);
    }

    void destroy(pointer p)
    {
        p->~value_type();
    }
};

// A partial specialization for the void value type is required for rebinding
// to another, different value type.
template <>
class ArrayAllocator<void>
{
  public:
    typedef void                value_type;
    typedef value_type*         pointer;
    typedef const value_type*   const_pointer;

    template <typename U>
    struct rebind
    {
        typedef ArrayAllocator<U> other;
    };

    ArrayAllocator() = default;

    template <typename U>
    ArrayAllocator(const ArrayAllocator<U>&)
    {
    }
};

}   // namespace foundation
