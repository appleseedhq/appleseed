
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

// Interface header.
#include "memory.h"

// appleseed.main headers.
#include "main/allocator.h"

// Standard headers.
#include <cstdlib>

namespace foundation
{

//
// Memory-alignment related functions implementation.
//
// todo: consider using _aligned_malloc() and _aligned_free() from <malloc.h> on Windows.
//

void* aligned_malloc(const size_t size, size_t alignment)
{
    // Often, the value of alignment is set to the line size of the L1 data cache as
    // reported by foundation::System::get_l1_data_cache_line_size().  It may happen
    // that this function returns 0 if the CPU cache information cannot be retrieved.
    // For convenience we handle this situation here by using a reasonable alignment
    // value when this happens.
    if (alignment == 0)
        alignment = 16;

    assert(size > 0);
    assert(is_pow2(alignment));

    // Compute the total number of bytes of memory we need to allocate.
    const size_t total_size = size + sizeof(void*) + (alignment - 1);

    // Allocate the memory.
    void** const unaligned_ptr = reinterpret_cast<void**>(malloc(total_size));

    // Handle allocation failures.
    if (!unaligned_ptr)
    {
        log_allocation_failure(total_size);
        return nullptr;
    }

    // Compute the next aligned address.
    void** const aligned_ptr = align(unaligned_ptr + 1, alignment);

    // Store the address of the unaligned memory block.
    *(aligned_ptr - 1) = unaligned_ptr;

    log_allocation(unaligned_ptr, aligned_ptr, total_size);

    return aligned_ptr;
}

void aligned_free(void* aligned_ptr)
{
    assert(aligned_ptr);

    // Retrieve the address of the unaligned memory block.
    void* unaligned_ptr = *(reinterpret_cast<void**>(aligned_ptr) - 1);

    log_deallocation(unaligned_ptr, aligned_ptr);

    // Deallocate the memory.
    free(unaligned_ptr);
}

}   // namespace foundation
