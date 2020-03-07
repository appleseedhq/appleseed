
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/hash/hash.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstdint>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Hash_Hash)
{
    const std::uint32_t N = 1000;

    struct Fixture
    {
        std::uint32_t m_result;

        Fixture()
          : m_result(0)
        {
        }
    };

    BENCHMARK_CASE_F(HashUInt32, Fixture)
    {
        for (std::uint32_t i = 0; i < N; ++i)
            m_result += hash_uint32(i);
    }

    BENCHMARK_CASE_F(HashUInt32Wang, Fixture)
    {
        for (std::uint32_t i = 0; i < N; ++i)
            m_result += hash_uint32_wang(i);
    }

    BENCHMARK_CASE_F(MixUInt32, Fixture)
    {
        for (std::uint32_t i = 0; i < N; ++i)
            m_result += mix_uint32(i, m_result);
    }
}
