
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
#include "foundation/math/permutation.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <cstring>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_Permutation)
{
    template <size_t ItemSize>      // in 4-byte words
    struct FixtureBase
    {
        static const size_t ItemCount = 256;

        struct Item
        {
            std::uint32_t data[ItemSize];
        };

        size_t  m_ordering1[ItemCount];
        size_t  m_ordering2[ItemCount];
        Item    m_items[ItemCount];

        FixtureBase()
        {
            // Generate orderings.
            MersenneTwister rng;
            random_permutation(ItemCount, m_ordering1, rng);
            random_permutation(ItemCount, m_ordering2, rng);

            // Initialize items.
            memset(m_items, 0, sizeof(m_items));
        }
    };

    template <size_t ItemSize>
    struct SmallItemFixture
      : public FixtureBase<ItemSize>
    {
        typedef FixtureBase<ItemSize> Base;

        typename Base::Item m_temp[Base::ItemCount];

        void payload()
        {
            small_item_reorder(Base::m_items, m_temp, Base::m_ordering1, Base::ItemCount);
            small_item_reorder(Base::m_items, m_temp, Base::m_ordering2, Base::ItemCount);
        }
    };

    template <size_t ItemSize>
    struct LargeItemFixture
      : public FixtureBase<ItemSize>
    {
        typedef FixtureBase<ItemSize> Base;

        size_t m_tags[Base::ItemCount];

        void payload()
        {
            large_item_reorder(Base::m_items, m_tags, Base::m_ordering1, Base::ItemCount);
            large_item_reorder(Base::m_items, m_tags, Base::m_ordering2, Base::ItemCount);
        }
    };

    BENCHMARK_CASE_F(BenchmarkSmallItemReorder4Bytes, SmallItemFixture<1>)
    {
        payload();
    }

    BENCHMARK_CASE_F(BenchmarkLargeItemReorder4Bytes, LargeItemFixture<1>)
    {
        payload();
    }

    BENCHMARK_CASE_F(BenchmarkSmallItemReorder8Bytes, SmallItemFixture<2>)
    {
        payload();
    }

    BENCHMARK_CASE_F(BenchmarkLargeItemReorder8Bytes, LargeItemFixture<2>)
    {
        payload();
    }

    BENCHMARK_CASE_F(BenchmarkSmallItemReorder16Bytes, SmallItemFixture<4>)
    {
        payload();
    }

    BENCHMARK_CASE_F(BenchmarkLargeItemReorder16Bytes, LargeItemFixture<4>)
    {
        payload();
    }

    BENCHMARK_CASE_F(BenchmarkSmallItemReorder32Bytes, SmallItemFixture<8>)
    {
        payload();
    }

    BENCHMARK_CASE_F(BenchmarkLargeItemReorder32Bytes, LargeItemFixture<8>)
    {
        payload();
    }

    BENCHMARK_CASE_F(BenchmarkSmallItemReorder64Bytes, SmallItemFixture<16>)
    {
        payload();
    }

    BENCHMARK_CASE_F(BenchmarkLargeItemReorder64Bytes, LargeItemFixture<16>)
    {
        payload();
    }
}
