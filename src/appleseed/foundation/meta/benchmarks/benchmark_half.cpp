
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/half.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/xoroshiro128plus.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_Half)
{
    //
    // Float -> half benchmarks.
    //

    template <size_t Size>
    struct FloatToHalfFixture
    {
        std::vector<float> m_input;
        std::vector<Half>  m_output;

        FloatToHalfFixture()
        {
            Xoroshiro128plus rng;

            m_input.resize(Size);

            for (size_t i = 0, e = m_input.size(); i < e; ++i)
                m_input[i] = rand_float1(rng, -1.0f, 100.0f);

            m_output.resize(m_input.size());
        }
    };

    //
    // Float -> half, 2x2 RGBA block.
    //

    BENCHMARK_CASE_F(FloatToHalf_2x2RGBABlock, FloatToHalfFixture<2 * 2 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = float_to_half(m_input[i]);
    }

    BENCHMARK_CASE_F(FloatToHalfAlt_2x2RGBABlock, FloatToHalfFixture<2 * 2 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = float_to_half_alt(m_input[i]);
    }

    BENCHMARK_CASE_F(FastFloatToHalf_2x2RGBABlock, FloatToHalfFixture<2 * 2 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = fast_float_to_half(m_input[i]);
    }

    //
    // Float -> half, 32x32 RGBA tile.
    //

    BENCHMARK_CASE_F(FloatToHalf_32x32RGBATile, FloatToHalfFixture<32 * 32 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = float_to_half(m_input[i]);
    }

    BENCHMARK_CASE_F(FloatToHalfAlt_32x32RGBATile, FloatToHalfFixture<32 * 32 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = float_to_half_alt(m_input[i]);
    }

    BENCHMARK_CASE_F(FastFloatToHalf_32x32RGBATile, FloatToHalfFixture<32 * 32 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = fast_float_to_half(m_input[i]);
    }

    //
    // Float -> half, 2048x2048 RGBA texture.
    //

    BENCHMARK_CASE_F(FloatToHalf_2Kx2KRGBATexture, FloatToHalfFixture<2048 * 2048 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = float_to_half(m_input[i]);
    }

    BENCHMARK_CASE_F(FloatToHalfAlt_2Kx2KRGBATexture, FloatToHalfFixture<2048 * 2048 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = float_to_half_alt(m_input[i]);
    }

    BENCHMARK_CASE_F(FastFloatToHalf_2Kx2KRGBATexture, FloatToHalfFixture<2048 * 2048 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = fast_float_to_half(m_input[i]);
    }

    //
    // Half -> float benchmarks.
    //

    template <size_t Size>
    struct HalfToFloatFixture
    {
        std::vector<Half>  m_input;
        std::vector<float> m_output;

        HalfToFloatFixture()
        {
            Xoroshiro128plus rng;

            m_input.resize(Size);

            for (size_t i = 0, e = m_input.size(); i < e; ++i)
                m_input[i] = float_to_half(rand_float1(rng, -1.0f, 100.0f));

            m_output.resize(m_input.size());
        }
    };

    //
    // Half -> float, 2x2 RGBA block.
    //

    BENCHMARK_CASE_F(HalfToFloat_2x2RGBABlock, HalfToFloatFixture<2 * 2 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = half_to_float(m_input[i]);
    }

    BENCHMARK_CASE_F(HalfToFloatAlt_2x2RGBABlock, HalfToFloatFixture<2 * 2 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = half_to_float_alt(m_input[i]);
    }

    //
    // Half -> float, 32x32 RGBA tile.
    //

    BENCHMARK_CASE_F(HalfToFloat_32x32RGBATile, HalfToFloatFixture<32 * 32 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = half_to_float(m_input[i]);
    }

    BENCHMARK_CASE_F(HalfToFloatAlt_32x32RGBATile, HalfToFloatFixture<32 * 32 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = half_to_float_alt(m_input[i]);
    }

    //
    // Half -> float, 2048x2048 RGBA texture.
    //

    BENCHMARK_CASE_F(HalfToFloat_2Kx2KRGBATexture, HalfToFloatFixture<2048 * 2048 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = half_to_float(m_input[i]);
    }

    BENCHMARK_CASE_F(HalfToFloatAlt_2Kx2KRGBATexture, HalfToFloatFixture<2048 * 2048 * 4>)
    {
        for (size_t i = 0, e = m_input.size(); i < e; ++i)
            m_output[i] = half_to_float_alt(m_input[i]);
    }
}
