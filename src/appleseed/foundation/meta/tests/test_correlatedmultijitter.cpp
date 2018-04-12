
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Kevin Masson, The appleseedhq Organization
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
#include "foundation/math/correlatedmultijitter.h"
#include "foundation/utility/test.h"
#include "foundation/utility/testutils.h"

// Standard headers.
#include <cmath>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_CorrelatedMultiJitter)
{
    TEST_CASE(Permute)
    {
        EXPECT_EQ(0,    cmj_permute(0, 1, 2));
        EXPECT_EQ(4,    cmj_permute(0, 12, 64));
        EXPECT_EQ(2,    cmj_permute(15, 3, 6));
        EXPECT_EQ(14,   cmj_permute(1, 64, 62));
    }

    TEST_CASE(Randfloat)
    {
        EXPECT_FEQ(0.967455,        cmj_randfloat(0, 64));
        EXPECT_FEQ(0.981134,        cmj_randfloat(32, 64));
        EXPECT_FEQ(0.872778,        cmj_randfloat(32, 32));
        EXPECT_FEQ(0.0821941,       cmj_randfloat(32, 0));
    }

    TEST_CASE(CMJSampling)
    {
        Vector2f sample;

        sample = cmj_generate_sample<float>(0, 64, 0);
        EXPECT_FEQ(0.0136372,       sample.x);
        EXPECT_FEQ(0.0136372,       sample.y);
        sample = cmj_generate_sample<float>(1, 64, 0);
        EXPECT_FEQ(0.628435,        sample.x);
        EXPECT_FEQ(0.0971851,       sample.y);
        sample = cmj_generate_sample<float>(16, 17, 0);
        EXPECT_FEQ(0.229956,        sample.x);
        EXPECT_FEQ(0.270536,        sample.y);
        sample = cmj_generate_sample<float>(0, 64, 1);
        EXPECT_FEQ(0.00946122,      sample.x);
        EXPECT_FEQ(0.229073,        sample.y);
        sample = cmj_generate_sample<float>(1, 64, 3);
        EXPECT_FEQ(0.335739,        sample.x);
        EXPECT_FEQ(0.430547,        sample.y);
        sample = cmj_generate_sample<float>(16, 17, 5);
        EXPECT_FEQ(0.972767,        sample.x);
        EXPECT_FEQ(0.430547,        sample.y);
    }

    void generate_cmj_image(
        const size_t    sample_count,
        const size_t    pattern)
    {
        vector<Vector2d> points;

        // Compute m and n
        unsigned int m, n;
        cmj_compute_mn(m, n, sample_count);

        for (size_t i = 0; i < sample_count; ++i)
            points.push_back(cmj_generate_sample<double>(i, m, n, sample_count, pattern));

        write_point_cloud_image(
            "unit tests/outputs/test_cmj_sampling_"
            + to_string(sample_count)
            + "_" + to_string(pattern)
            + ".png",
            points);
    }

    TEST_CASE(Generate2DSamplingImages)
    {
        generate_cmj_image(8, 0);
        generate_cmj_image(8, 1);
        generate_cmj_image(8, 2);
        generate_cmj_image(16, 1);
        generate_cmj_image(17, 1);
        generate_cmj_image(64, 1);
        generate_cmj_image(64, 478585081);
        generate_cmj_image(65, 1);
        generate_cmj_image(120, 1);
    }
}
