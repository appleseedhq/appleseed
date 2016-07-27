
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_PLATFORM_ATOMIC_H
#define APPLESEED_FOUNDATION_PLATFORM_ATOMIC_H

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif
#include "foundation/utility/casts.h"
#include "foundation/utility/memory.h"

// Boost headers.
#include "boost/atomic/atomic.hpp"
#include "boost/interprocess/detail/atomic.hpp"
#include "boost/static_assert.hpp"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// Atomic operations on native types.
//

uint32 atomic_read(
    volatile uint32*    ptr);

void atomic_write(
    volatile uint32*    ptr,
    const uint32        value);

uint32 atomic_inc(
    volatile uint32*    ptr);

uint32 atomic_dec(
    volatile uint32*    ptr);

bool atomic_cas(
    volatile uint32*    ptr,
    const uint32        initial_value,
    const uint32        new_value);

void atomic_add(
    volatile float*     ptr,
    const float         operand);


//
// Implementation.
//

APPLESEED_FORCE_INLINE uint32 atomic_read(
    volatile uint32*    ptr)
{
    return boost::interprocess::ipcdetail::atomic_read32(ptr);
}

APPLESEED_FORCE_INLINE void atomic_write(
    volatile uint32*    ptr,
    const uint32        value)
{
    boost::interprocess::ipcdetail::atomic_write32(ptr, value);
}

APPLESEED_FORCE_INLINE uint32 atomic_inc(
    volatile uint32*    ptr)
{
    return boost::interprocess::ipcdetail::atomic_inc32(ptr);
}

APPLESEED_FORCE_INLINE uint32 atomic_dec(
    volatile uint32*    ptr)
{
    return boost::interprocess::ipcdetail::atomic_dec32(ptr);
}

APPLESEED_FORCE_INLINE bool atomic_cas(
    volatile uint32*    ptr,
    const uint32        expected_value,
    const uint32        new_value)
{
#if defined _WIN32
    BOOST_STATIC_ASSERT(sizeof(LONG) == sizeof(uint32));
    return
        InterlockedCompareExchange(
            reinterpret_cast<volatile LONG*>(ptr),
            new_value,
            expected_value)
        == expected_value;
#elif defined __GNUC__
    return __sync_bool_compare_and_swap(ptr, expected_value, new_value);
#else
    #error Unsupported platform.
#endif
}

APPLESEED_FORCE_INLINE void atomic_add(
    volatile float*     ptr,
    const float         operand)
{
    assert(is_aligned(ptr, 4));

    while (true)
    {
        const float forg = *ptr;
        const float fnew = forg + operand;
        const uint32 iorg = binary_cast<uint32>(forg);
        const uint32 inew = binary_cast<uint32>(fnew);
        if (atomic_cas(reinterpret_cast<volatile uint32*>(ptr), iorg, inew))
            break;
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_ATOMIC_H
