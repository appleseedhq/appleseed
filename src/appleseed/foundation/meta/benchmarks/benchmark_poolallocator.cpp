
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
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <memory>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Utility_PoolAllocator)
{
    const size_t N = 100;

    template <typename Allocator>
    struct Fixture
    {
        Allocator m_allocator;

        void repeated_allocation_deallocation()
        {
            std::uint32_t* p = m_allocator.allocate(1);
            m_allocator.deallocate(p, 1);
        }

        void first_allocated_first_deallocated_batch()
        {
            std::uint32_t* p[N];

            for (size_t i = 0; i < N; ++i)
                p[i] = m_allocator.allocate(1);

            for (size_t i = 0; i < N; ++i)
                m_allocator.deallocate(p[i], 1);
        }

        void first_allocated_last_deallocated_batch()
        {
            std::uint32_t* p[N];

            for (size_t i = 0; i < N; ++i)
                p[i] = m_allocator.allocate(1);

            for (size_t i = N; i; --i)
                m_allocator.deallocate(p[i - 1], 1);
        }
    };

    typedef std::allocator<std::uint32_t> DefaultAllocator;
    typedef PoolAllocator<std::uint32_t, N> PoolAllocator;

    BENCHMARK_CASE_F(RepeatedAllocation_PoolAllocator, Fixture<PoolAllocator>)
    {
        m_allocator.allocate(1);
    }

    BENCHMARK_CASE_F(RepeatedAllocationDeallocation_DefaultAllocator, Fixture<DefaultAllocator>)
    {
        repeated_allocation_deallocation();
    }

    BENCHMARK_CASE_F(RepeatedAllocationDeallocation_PoolAllocator, Fixture<PoolAllocator>)
    {
        repeated_allocation_deallocation();
    }

    BENCHMARK_CASE_F(FirstAllocatedFirstDeallocatedBatch_DefaultAllocator, Fixture<DefaultAllocator>)
    {
        first_allocated_first_deallocated_batch();
    }

    BENCHMARK_CASE_F(FirstAllocatedFirstDeallocatedBatch_PoolAllocator, Fixture<PoolAllocator>)
    {
        first_allocated_first_deallocated_batch();
    }

    BENCHMARK_CASE_F(FirstAllocatedLastDeallocatedBatch_DefaultAllocator, Fixture<DefaultAllocator>)
    {
        first_allocated_last_deallocated_batch();
    }

    BENCHMARK_CASE_F(FirstAllocatedLastDeallocatedBatch_PoolAllocator, Fixture<PoolAllocator>)
    {
        first_allocated_last_deallocated_batch();
    }
}
