
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

// appleseed.foundation headers.
#include "foundation/utility/memory.h"

// Standard headers.
#include <cstddef>
#include <exception>
#include <new>

using namespace foundation;
using namespace std;


//
// Globally override all variants of the new and delete operators
// to provide custom-aligned memory allocations.
//

namespace
{
    // Alignment boundary of all memory allocations, in bytes.
    const size_t Alignment = 16;

    void* new_impl(size_t size)
    {
        if (size < 1)
            size = 1;

        void* ptr = aligned_malloc(size, Alignment);

        if (!ptr)
            throw bad_alloc();

        return ptr;
    }

    void delete_impl(void* ptr)
    {
        if (!ptr)
            return;

        aligned_free(ptr);
    }
}

void* operator new(size_t size)
  throw(bad_alloc)
{
    return new_impl(size);
}

void* operator new[](size_t size)
  throw(bad_alloc)
{
    return new_impl(size);
}

void* operator new(size_t size, const nothrow_t&)
  throw()
{
    return new_impl(size);
}

void* operator new[](size_t size, const nothrow_t&)
  throw()
{
    return new_impl(size);
}

void operator delete(void* ptr)
  throw()
{
    delete_impl(ptr);
}

void operator delete[](void* ptr)
  throw()
{
    delete_impl(ptr);
}

void operator delete(void* ptr, const nothrow_t&)
  throw()
{
    delete_impl(ptr);
}

void operator delete[](void* ptr, const nothrow_t&)
  throw()
{
    delete_impl(ptr);
}
