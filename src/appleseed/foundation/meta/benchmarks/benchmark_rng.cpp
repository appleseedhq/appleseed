
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/rng/lcg.h"
#include "foundation/math/rng/pcg.h"
#include "foundation/math/rng/serialmersennetwister.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/math/rng/simdmersennetwister.h"
#endif
#include "foundation/math/rng/xoroshiro128plus.h"
#include "foundation/math/rng/xorshift32.h"
#include "foundation/math/rng/xorshift64.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_RNG)
{
    template <typename RNG>
    struct Fixture
    {
        RNG             m_rng;
        std::uint32_t   m_dummy;

        Fixture()
          : m_dummy(0)
        {
        }
    };

    BENCHMARK_CASE_F(LCG_RandUint32, Fixture<LCG>)
    {
        for (size_t i = 0; i < 250000; ++i)
        {
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
        }
    }

    BENCHMARK_CASE_F(PCG_RandUint32, Fixture<PCG>)
    {
        for (size_t i = 0; i < 250000; ++i)
        {
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
        }
    }

    BENCHMARK_CASE_F(SerialMersenneTwister_RandUint32, Fixture<SerialMersenneTwister>)
    {
        for (size_t i = 0; i < 250000; ++i)
        {
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
        }
    }

#ifdef APPLESEED_USE_SSE

    BENCHMARK_CASE_F(SimdMersenneTwister_RandUint32, Fixture<SimdMersenneTwister>)
    {
        for (size_t i = 0; i < 250000; ++i)
        {
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
        }
    }

#endif

    BENCHMARK_CASE_F(Xoroshiro128plus, Fixture<Xoroshiro128plus>)
    {
        for (size_t i = 0; i < 250000; ++i)
        {
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
        }
    }

    BENCHMARK_CASE_F(Xorshift32_RandUint32, Fixture<Xorshift32>)
    {
        for (size_t i = 0; i < 250000; ++i)
        {
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
        }
    }

    BENCHMARK_CASE_F(Xorshift64_RandUint32, Fixture<Xorshift64>)
    {
        for (size_t i = 0; i < 250000; ++i)
        {
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
            m_dummy ^= m_rng.rand_uint32();
        }
    }
}
