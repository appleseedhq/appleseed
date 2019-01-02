
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
#include "foundation/math/combination.h"
#include "foundation/math/fp.h"
#include "foundation/math/minmax.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/utility/casts.h"
#include "foundation/utility/test.h"
#include "foundation/utility/typetraits.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <utility>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_MinMax)
{
    TEST_CASE(Min_ThreeArguments)
    {
        EXPECT_EQ(0, min(0, 1, 2));
        EXPECT_EQ(0, min(0, 2, 1));
        EXPECT_EQ(0, min(1, 0, 2));
        EXPECT_EQ(0, min(1, 2, 0));
        EXPECT_EQ(0, min(2, 0, 1));
        EXPECT_EQ(0, min(2, 1, 0));
    }

    TEST_CASE(Max_ThreeArguments)
    {
        EXPECT_EQ(2, max(0, 1, 2));
        EXPECT_EQ(2, max(0, 2, 1));
        EXPECT_EQ(2, max(1, 0, 2));
        EXPECT_EQ(2, max(1, 2, 0));
        EXPECT_EQ(2, max(2, 0, 1));
        EXPECT_EQ(2, max(2, 1, 0));
    }

    pair<int, int> minmax_pair(const int a, const int b)
    {
        int min, max;
        minmax(a, b, min, max);
        return make_pair(min, max);
    }

    pair<int, int> minmax_pair(const int a, const int b, const int c)
    {
        int min, max;
        minmax(a, b, c, min, max);
        return make_pair(min, max);
    }

    TEST_CASE(MinMax_TwoArguments)
    {
        EXPECT_EQ(0, minmax_pair(0, 2).first);
        EXPECT_EQ(2, minmax_pair(0, 2).second);
        EXPECT_EQ(0, minmax_pair(2, 0).first);
        EXPECT_EQ(2, minmax_pair(2, 0).second);
    }

    TEST_CASE(MinMax_ThreeArguments)
    {
        EXPECT_EQ(0, minmax_pair(0, 1, 2).first);
        EXPECT_EQ(2, minmax_pair(0, 1, 2).second);
        EXPECT_EQ(0, minmax_pair(0, 2, 1).first);
        EXPECT_EQ(2, minmax_pair(0, 2, 1).second);
        EXPECT_EQ(0, minmax_pair(1, 0, 2).first);
        EXPECT_EQ(2, minmax_pair(1, 0, 2).second);
        EXPECT_EQ(0, minmax_pair(1, 2, 0).first);
        EXPECT_EQ(2, minmax_pair(1, 2, 0).second);
        EXPECT_EQ(0, minmax_pair(2, 0, 1).first);
        EXPECT_EQ(2, minmax_pair(2, 0, 1).second);
        EXPECT_EQ(0, minmax_pair(2, 1, 0).first);
        EXPECT_EQ(2, minmax_pair(2, 1, 0).second);
    }

#ifdef APPLESEED_USE_SSE

    float ssemin_reference(const float a, const float b)
    {
        return _mm_cvtss_f32(_mm_min_ss(_mm_set_ss(a), _mm_set_ss(b)));
    }

    float ssemax_reference(const float a, const float b)
    {
        return _mm_cvtss_f32(_mm_max_ss(_mm_set_ss(a), _mm_set_ss(b)));
    }

    TEST_CASE(SSEMinAndSSEMax_GivenPermutationsOfSpecialFloatingPointNumbers_MatchMINSSAndMAXSS)
    {
        typedef TypeConv<float>::UInt UInt;

        static UInt Values[] =
        {
            binary_cast<UInt>(1.0f),
            binary_cast<UInt>(2.0f),
            binary_cast<UInt>(FP<float>::pos_zero()),
            binary_cast<UInt>(FP<float>::neg_zero()),
            binary_cast<UInt>(FP<float>::pos_min()),
            binary_cast<UInt>(FP<float>::neg_min()),
            binary_cast<UInt>(FP<float>::pos_inf()),
            binary_cast<UInt>(FP<float>::neg_inf()),
            binary_cast<UInt>(FP<float>::snan()),
            binary_cast<UInt>(FP<float>::qnan())
        };

        const size_t N = countof(Values);

        sort(&Values[0], &Values[N]);

        UInt* first = &Values[0];
        UInt* middle = &Values[2];
        UInt* last = &Values[N];

        size_t permutation_count = 0;

        do
        {
            const float a = binary_cast<float>(*first);
            const float b = binary_cast<float>(*middle);

            const UInt expected_min = binary_cast<UInt>(ssemin_reference(a, b));
            const UInt obtained_min = binary_cast<UInt>(ssemin(a, b));

            EXPECT_EQ(expected_min, obtained_min);

            const UInt expected_max = binary_cast<UInt>(ssemax_reference(a, b));
            const UInt obtained_max = binary_cast<UInt>(ssemax(a, b));

            EXPECT_EQ(expected_max, obtained_max);

            ++permutation_count;
        }
        while (next_partial_permutation(first, middle, last));

        const size_t ExpectedPermutationCount = factorial(N) / factorial(N - 2);

        EXPECT_EQ(ExpectedPermutationCount, permutation_count);
    }

#endif  // APPLESEED_USE_SSE

    TEST_CASE(MinIndex_TwoArguments)
    {
        EXPECT_EQ(0, min_index(0, 2));
        EXPECT_EQ(1, min_index(2, 0));
    }

    TEST_CASE(MinIndex_ThreeArguments)
    {
        EXPECT_EQ(0, min_index(0, 1, 2));
        EXPECT_EQ(0, min_index(0, 2, 1));
        EXPECT_EQ(1, min_index(1, 0, 2));
        EXPECT_EQ(2, min_index(1, 2, 0));
        EXPECT_EQ(1, min_index(2, 0, 1));
        EXPECT_EQ(2, min_index(2, 1, 0));
    }

    TEST_CASE(MaxIndex_TwoArguments)
    {
        EXPECT_EQ(1, max_index(0, 2));
        EXPECT_EQ(0, max_index(2, 0));
    }

    TEST_CASE(MaxIndex_ThreeArguments)
    {
        EXPECT_EQ(2, max_index(0, 1, 2));
        EXPECT_EQ(1, max_index(0, 2, 1));
        EXPECT_EQ(2, max_index(1, 0, 2));
        EXPECT_EQ(1, max_index(1, 2, 0));
        EXPECT_EQ(0, max_index(2, 0, 1));
        EXPECT_EQ(0, max_index(2, 1, 0));
    }
}
