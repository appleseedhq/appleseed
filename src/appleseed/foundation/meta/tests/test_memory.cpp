
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
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/memory/alignedallocator.h"
#include "foundation/memory/memory.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <cstdlib>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Utility_Memory)
{
    TEST_CASE(Align_GivenIntegerZero_ReturnsIntegerZero)
    {
        EXPECT_EQ(0, align(0, 4));
    }

    TEST_CASE(Align_GivenUnalignedInteger_ReturnsAlignedInteger)
    {
        EXPECT_EQ(16, align(13, 4));
    }

    TEST_CASE(Align_GivenAlignedInteger_ReturnsIntegerUnmodified)
    {
        EXPECT_EQ(16, align(16, 4));
    }

    TEST_CASE(Align_GivenNullPointer_ReturnsNullPointer)
    {
        EXPECT_EQ(static_cast<void*>(nullptr), align(static_cast<void*>(nullptr), 4));
    }

    TEST_CASE(Align_GivenUnalignedPointer_ReturnsAlignedPointer)
    {
        EXPECT_EQ(reinterpret_cast<void*>(16), align(reinterpret_cast<void*>(13), 4));
    }

    TEST_CASE(Align_GivenAlignedPointer_ReturnsPointerUnmodified)
    {
        EXPECT_EQ(reinterpret_cast<void*>(16), align(reinterpret_cast<void*>(16), 4));
    }

    TEST_CASE(Alignment_GivenIntegerZero_ReturnsMaxAlignment)
    {
        EXPECT_EQ(32, alignment(0, 32));
    }

    TEST_CASE(Alignment_GivenAlignedInteger_ReturnsAlignment)
    {
        EXPECT_EQ(16, alignment(16, 32));
    }

    TEST_CASE(Alignment_GivenNonAlignedInteger_ReturnsOne)
    {
        EXPECT_EQ(1, alignment(17, 32));
    }

    TEST_CASE(Alignment_GivenNullPointer_ReturnsMaxAlignment)
    {
        EXPECT_EQ(32, alignment(static_cast<void*>(nullptr), 32));
    }

    TEST_CASE(Alignment_GivenAlignedPointer_ReturnsAlignment)
    {
        EXPECT_EQ(16, alignment(reinterpret_cast<void*>(16), 32));
    }

    TEST_CASE(Alignment_GivenNonAlignedPointer_ReturnsOne)
    {
        EXPECT_EQ(1, alignment(reinterpret_cast<void*>(17), 32));
    }

    TEST_CASE(IsAligned_GivenIntegerZero_ReturnsTrue)
    {
        EXPECT_TRUE(is_aligned(0, 32));
    }

    TEST_CASE(IsAligned_GivenAlignedInteger_ReturnsTrue)
    {
        EXPECT_TRUE(is_aligned(64, 32));
    }

    TEST_CASE(IsAligned_GivenNonAlignedInteger_ReturnsFalse)
    {
        EXPECT_FALSE(is_aligned(65, 32));
    }

    TEST_CASE(IsAligned_GivenNullPointer_ReturnsTrue)
    {
        EXPECT_TRUE(is_aligned(static_cast<void*>(nullptr), 32));
    }

    TEST_CASE(IsAligned_GivenAlignedPointer_ReturnsTrue)
    {
        EXPECT_TRUE(is_aligned(reinterpret_cast<void*>(64), 32));
    }

    TEST_CASE(IsAligned_GivenNonAlignedPointer_ReturnsFalse)
    {
        EXPECT_FALSE(is_aligned(reinterpret_cast<void*>(65), 32));
    }

#ifdef APPLESEED_USE_SSE

    TEST_CASE(Malloc_AlwaysReturns16ByteAlignedPointer)
    {
        const size_t AllocationCount = 100;

        bool aligned = true;
        void* pointers[AllocationCount];

        MersenneTwister rng;

        for (size_t i = 0; i < AllocationCount; ++i)
        {
            const size_t alloc_size = rand_int1(rng, 1, 1024);
            pointers[i] = malloc(alloc_size);
            aligned = aligned && is_aligned(pointers[i], 16);
        }

        for (size_t i = 0; i < AllocationCount; ++i)
            free(pointers[i]);

        EXPECT_TRUE(aligned);
    }

#endif

    TEST_CASE(EnsureMinimumSize_GivenEmptyVector_ResizesVectorByInsertingDefaultValue)
    {
        std::vector<int> v;

        ensure_minimum_size(v, 2);

        EXPECT_EQ(2, v.size());
    }

    TEST_CASE(EnsureMinimumSize_GivenEmptyVector_ResizesVectorByInsertingProvidedValue)
    {
        std::vector<int> v;

        ensure_minimum_size(v, 2, 42);

        ASSERT_EQ(2, v.size());
        EXPECT_EQ(42, v[0]);
        EXPECT_EQ(42, v[1]);
    }

    TEST_CASE(EnsureMinimumSize_GivenMinimumSizeSmallerThanCurrentVectorSize_DoesNothing)
    {
        std::vector<int> v(12);

        ensure_minimum_size(v, 3);

        EXPECT_EQ(12, v.size());
    }

    TEST_CASE(ClearReleaseMemory_GivenVectorWithThousandElements_ClearsVector)
    {
        std::vector<int> v(1000);

        clear_release_memory(v);

        EXPECT_TRUE(v.empty());
    }

    TEST_CASE(ClearReleaseMemory_GivenVectorWithThousandElements_ResetsVectorCapacityToDefaultValue)
    {
        std::vector<int> v;
        const size_t default_capacity = v.capacity();

        v.resize(1000);

        clear_release_memory(v);

        EXPECT_EQ(default_capacity, v.capacity());
    }

    TEST_CASE(ClearReleaseMemory_GivenVectorWithThousandElements_UsingAlignedAllocator_ResetsVectorCapacityToDefaultValue)
    {
        typedef AlignedAllocator<int> Allocator;

        std::vector<int, AlignedAllocator<int>> v(Allocator(32));
        const size_t default_capacity = v.capacity();

        v.resize(1000);

        clear_release_memory(v);

        EXPECT_EQ(default_capacity, v.capacity());
    }

    TEST_CASE(ClearKeepMemory_GivenVectorWithThousandElements_ClearsVector)
    {
        std::vector<int> v(1000);

        clear_keep_memory(v);

        EXPECT_TRUE(v.empty());
    }

    TEST_CASE(ClearKeepMemory_GivenVectorWithThousandElements_RetainsVectorCapacity)
    {
        std::vector<int> v(1000);

        const size_t old_capacity = v.capacity();

        clear_keep_memory(v);

        EXPECT_EQ(old_capacity, v.capacity());
    }
}
