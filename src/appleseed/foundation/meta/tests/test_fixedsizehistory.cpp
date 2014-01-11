
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/fixedsizehistory.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_FixedSizeHistory)
{
    struct Fixture
    {
        FixedSizeHistory<double, 2> m_history;
    };

    TEST_CASE_F(Constructor_SetsSizeToZero, Fixture)
    {
        EXPECT_EQ(0, m_history.size());
    }

    TEST_CASE_F(ComputeAverage_GivenEmptyHistory_ReturnsZero, Fixture)
    {
        EXPECT_EQ(0.0, m_history.compute_average());
    }

    TEST_CASE_F(ComputeAverage_GivenSingleValue_ReturnsValue, Fixture)
    {
        m_history.insert(5.0);

        EXPECT_EQ(5.0, m_history.compute_average());
    }

    TEST_CASE_F(ComputeAverage_GivenTwoValuesInSizeTwoHistory_ReturnsAverageValue, Fixture)
    {
        m_history.insert(5.0);
        m_history.insert(7.0);

        EXPECT_FEQ(6.0, m_history.compute_average());
    }

    TEST_CASE_F(ComputeAverage_GivenThreeValuesInSizeTwoHistory_ReturnsAverageOfLastTwoValues, Fixture)
    {
        m_history.insert(5.0);
        m_history.insert(7.0);
        m_history.insert(1.0);

        EXPECT_FEQ(4.0, m_history.compute_average());
    }
}
