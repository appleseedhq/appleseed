
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/utility/shadowterminator.h"

// appleseed.foundation headers.
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/xorshift32.h"
#include "foundation/utility/benchmark.h"

using namespace foundation;
using namespace renderer;

BENCHMARK_SUITE(Renderer_Utility_ShadowTerminator)
{
    constexpr float Correction = 0.1f;
    constexpr float K = 1.0f / (1.0f - Correction);

    struct Fixture
    {
        Xorshift32  m_rng;
        float       m_dummy = 1.0f;
    };

    BENCHMARK_CASE_F(ShiftCosIn, Fixture)
    {
        const float cos_in = rand_float2(m_rng);
        m_dummy *= shift_cos_in(cos_in, Correction);
    }

    BENCHMARK_CASE_F(ShiftCosInFast, Fixture)
    {
        const float cos_in = rand_float2(m_rng);
        m_dummy *= shift_cos_in_fast(cos_in, K);
    }
}
