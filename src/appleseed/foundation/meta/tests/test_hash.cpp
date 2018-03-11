
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/hash.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdio>

using namespace foundation;

TEST_SUITE(Foundation_Math_Hash)
{
    const size_t N = 1000000;
    const size_t BinCount = 100;
    const float ScaleToUnit = 2.3283063e-010f;

    void plot_histogram(const char* filepath, const size_t bins[])
    {
        FILE* file = fopen(filepath, "wt");
        assert(file);

        fprintf(file, FMT_SIZE_T " values, " FMT_SIZE_T " bins\n\n", N, BinCount);

        const double Expected = N / BinCount;
        double deviation = 0.0;
        for (size_t i = 0; i < BinCount; ++i)
            deviation += square(bins[i] - Expected) / Expected;
        fprintf(file, "Deviation from expected value: %f\n\n", deviation);

        for (size_t i = 0; i < BinCount; ++i)
        {
            const size_t value = bins[i];
            const size_t Scale = (N / BinCount) / 100;
            fprintf(file, FMT_SIZE_T "\t", i);
            for (size_t j = 0; j < value / Scale; ++j)
                fprintf(file, "#");
            fprintf(file, "\n");
        }

        fclose(file);
    }

    TEST_CASE(MersenneTwister)
    {
        size_t bins[BinCount];
        for (size_t i = 0; i < BinCount; ++i)
            bins[i] = 0;

        MersenneTwister rng;

        for (uint32 i = 0; i < N; ++i)
        {
            const float x = rng.rand_uint32() * ScaleToUnit;
            assert(x >= 0.0f && x < 1.0f);
            ++bins[truncate<size_t>(x * BinCount)];
        }

        plot_histogram("unit tests/outputs/test_hash_mersennetwister.txt", bins);
    }

    TEST_CASE(HashUInt32_Counter)
    {
        size_t bins[BinCount];
        for (size_t i = 0; i < BinCount; ++i)
            bins[i] = 0;

        for (uint32 i = 0; i < N; ++i)
        {
            const float x = hash_uint32(i) * ScaleToUnit;
            assert(x >= 0.0f && x < 1.0f);
            ++bins[truncate<size_t>(x * BinCount)];
        }

        plot_histogram("unit tests/outputs/test_hash_hashuint32_counter.txt", bins);
    }

    TEST_CASE(HashUInt32_RandomNumbers)
    {
        size_t bins[BinCount];
        for (size_t i = 0; i < BinCount; ++i)
            bins[i] = 0;

        MersenneTwister rng;

        for (uint32 i = 0; i < N; ++i)
        {
            const float x = hash_uint32(rng.rand_uint32()) * ScaleToUnit;
            assert(x >= 0.0f && x < 1.0f);
            ++bins[truncate<size_t>(x * BinCount)];
        }

        plot_histogram("unit tests/outputs/test_hash_hashuint32_random.txt", bins);
    }

    TEST_CASE(HashUInt32Pixar_Counter)
    {
        size_t bins[BinCount];
        for (size_t i = 0; i < BinCount; ++i)
            bins[i] = 0;

        for (uint32 i = 0; i < N; ++i)
        {
            const float x = hash_uint32_pixar(i, 12345678);
            assert(x >= 0.0f && x < 1.0f);
            ++bins[truncate<size_t>(x * BinCount)];
        }

        plot_histogram("unit tests/outputs/test_hash_hashuint32pixar_counter.txt", bins);
    }

    TEST_CASE(HashUInt32Pixar_RandomNumbers)
    {
        size_t bins[BinCount];
        for (size_t i = 0; i < BinCount; ++i)
            bins[i] = 0;

        MersenneTwister rng;

        for (uint32 i = 0; i < N; ++i)
        {
            const float x = hash_uint32_pixar(rng.rand_uint32(), 12345678);
            assert(x >= 0.0f && x < 1.0f);
            ++bins[truncate<size_t>(x * BinCount)];
        }

        plot_histogram("unit tests/outputs/test_hash_hashuint32pixar_random.txt", bins);
    }

    TEST_CASE(MixUInt32)
    {
        size_t bins[BinCount];
        for (size_t i = 0; i < BinCount; ++i)
            bins[i] = 0;

        for (uint32 i = 0; i < N / 10; ++i)
        {
            for (uint32 j = 0; j < 10; ++j)
            {
                const float x = mix_uint32(i, j) * ScaleToUnit;
                assert(x >= 0.0f && x < 1.0f);
                ++bins[truncate<size_t>(x * BinCount)];
            }
        }

        plot_histogram("unit tests/outputs/test_hash_mixuint32.txt", bins);
    }
}
