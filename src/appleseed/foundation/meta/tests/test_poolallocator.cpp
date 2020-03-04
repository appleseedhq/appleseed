
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

// appleseed.foundation headers.
#include "foundation/memory/poolallocator.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <memory>

using namespace foundation;

TEST_SUITE(Foundation_Utility_PoolAllocator)
{
    TEST_CASE(IdenticalAllocatorsAreEqual)
    {
        PoolAllocator<int, 128> a1, a2;

        EXPECT_TRUE(a1 == a2);
        EXPECT_FALSE(a1 != a2);
    }

    TEST_CASE(AllocatorsWithDifferentValueTypesAreNotEqual)
    {
        PoolAllocator<int, 128, std::allocator<int>> a1;
        PoolAllocator<unsigned int, 256, std::allocator<int>> a2;

        EXPECT_FALSE(a1 == a2);
        EXPECT_TRUE(a1 != a2);
    }

    TEST_CASE(AllocatorsWithDifferentPageSizesAreNotEqual)
    {
        PoolAllocator<int, 128> a1;
        PoolAllocator<int, 256> a2;

        EXPECT_FALSE(a1 == a2);
        EXPECT_TRUE(a1 != a2);
    }

    TEST_CASE(AllocatorsWithDifferentFallbackAllocatorsAreNotEqual)
    {
        PoolAllocator<int, 128, PoolAllocator<int, 128>> a1;
        PoolAllocator<int, 128, std::allocator<int>> a2;

        EXPECT_FALSE(a1 == a2);
        EXPECT_TRUE(a1 != a2);
    }

    TEST_CASE(AllocateDeallocateSingleItem)
    {
        PoolAllocator<int, 2> allocator;

        int* p = allocator.allocate(1);
        EXPECT_NEQ(0, p);

        allocator.deallocate(p, 1);
    }

    TEST_CASE(AllocateDeallocateArrayOfItems)
    {
        PoolAllocator<int, 2> allocator;

        const size_t N = 10;

        int* p = allocator.allocate(N);
        EXPECT_NEQ(0, p);

        allocator.deallocate(p, N);
    }

    TEST_CASE(RebindVoidAllocatorToIntAllocator)
    {
        PoolAllocator<void, 2>::rebind<int>::other allocator;

        int* p = allocator.allocate(1);
        EXPECT_NEQ(0, p);

        allocator.deallocate(p, 1);
    }
}
