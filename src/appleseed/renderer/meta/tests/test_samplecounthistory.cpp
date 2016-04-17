
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/rendering/progressive/samplecounthistory.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

using namespace renderer;

TEST_SUITE(Renderer_Kernel_Rendering_Progressive_SampleCountHistory)
{
    TEST_CASE(GetSamplesPerSecond_GivenZeroValue_ReturnsZero)
    {
        SampleCountHistory<4> history;

        EXPECT_EQ(0.0, history.get_samples_per_second());
    }

    TEST_CASE(GetSamplesPerSecond_GivenOneValue_ReturnsZero)
    {
        SampleCountHistory<4> history;
        history.insert(0.0, 10);

        EXPECT_EQ(0.0, history.get_samples_per_second());
    }

    TEST_CASE(GetSamplesPerSecond_GivenTwoEqualValues_ReturnsZero)
    {
        SampleCountHistory<4> history;
        history.insert(0.0, 10);
        history.insert(1.0, 10);

        EXPECT_EQ(0.0, history.get_samples_per_second());
    }

    TEST_CASE(GetSamplesPerSecond_GivenTwoValues_GivenDeltaTimeZero_ReturnsZero)
    {
        SampleCountHistory<4> history;
        history.insert(0.0, 10);
        history.insert(0.0, 20);

        EXPECT_EQ(0.0, history.get_samples_per_second());
    }

    TEST_CASE(GetSamplesPerSecond_GivenTwoValues_GivenDeltaTimeOne_ReturnsDeltaValues)
    {
        SampleCountHistory<4> history;
        history.insert(0.0, 10);
        history.insert(1.0, 20);

        EXPECT_FEQ(10.0, history.get_samples_per_second());
    }

    TEST_CASE(GetSamplesPerSecond_GivenNValues_ReturnsAverage)
    {
        SampleCountHistory<4> history;
        history.insert(0.0, 10);
        history.insert(1.0, 20);
        history.insert(2.0, 30);
        history.insert(3.0, 40);

        EXPECT_FEQ(10.0, history.get_samples_per_second());
    }

    TEST_CASE(GetSamplesPerSecond_GivenNPlusOneValues_ReturnsAverage)
    {
        SampleCountHistory<4> history;
        history.insert(0.0, 10);
        history.insert(1.0, 20);
        history.insert(2.0, 30);
        history.insert(3.0, 40);
        history.insert(4.0, 80);

        EXPECT_FEQ(20.0, history.get_samples_per_second());
    }
}
