
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
#include "foundation/math/cdf.h"
#include "foundation/math/fp.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_CDF)
{
    typedef foundation::CDF<int, double> CDF;

    TEST_CASE(Empty_GivenCDFInInitialState_ReturnsTrue)
    {
        CDF cdf;

        EXPECT_TRUE(cdf.empty());
    }

    TEST_CASE(Valid_GivenCDFInInitialState_ReturnsFalse)
    {
        CDF cdf;

        EXPECT_FALSE(cdf.valid());
    }

    TEST_CASE(Empty_GivenCDFWithOneItem_ReturnsFalse)
    {
        CDF cdf;
        cdf.insert(1, 0.5);

        EXPECT_FALSE(cdf.empty());
    }

    TEST_CASE(Valid_GivenCDFWithOneItemWithPositiveWeight_ReturnsTrue)
    {
        CDF cdf;
        cdf.insert(1, 0.5);

        EXPECT_TRUE(cdf.valid());
    }

    TEST_CASE(Valid_GivenCDFWithOneItemWithZeroWeight_ReturnsFalse)
    {
        CDF cdf;
        cdf.insert(1, 0.0);

        EXPECT_FALSE(cdf.valid());
    }

    TEST_CASE(Clear_GivenCDFWithOneItem_RemovesItem)
    {
        CDF cdf;
        cdf.insert(1, 0.5);
        cdf.clear();

        EXPECT_TRUE(cdf.empty());
    }

    TEST_CASE(Clear_GivenCDFWithOneItem_MakesCDFInvalid)
    {
        CDF cdf;
        cdf.insert(1, 0.5);
        cdf.clear();

        EXPECT_FALSE(cdf.valid());
    }

    TEST_CASE(Sample_GivenCDFWithOneItemWithPositiveWeight_ReturnsItem)
    {
        CDF cdf;
        cdf.insert(1, 0.5);
        cdf.prepare();

        const CDF::ItemWeightPair result = cdf.sample(0.5);

        EXPECT_EQ(1, result.first);
        EXPECT_FEQ(1.0, result.second);
    }

    struct Fixture
    {
        CDF m_cdf;

        Fixture()
        {
            m_cdf.insert(1, 0.4);
            m_cdf.insert(2, 1.6);
            m_cdf.prepare();
        }
    };

    TEST_CASE_F(Sample_GivenInputEqualToZero_ReturnsItems1, Fixture)
    {
        const CDF::ItemWeightPair result = m_cdf.sample(0.0);

        EXPECT_EQ(1, result.first);
        EXPECT_FEQ(0.2, result.second);
    }

    TEST_CASE_F(Sample_GivenInputEqualTo0_2_ReturnsItems2, Fixture)
    {
        const CDF::ItemWeightPair result = m_cdf.sample(0.2);

        EXPECT_EQ(2, result.first);
        EXPECT_FEQ(0.8, result.second);
    }

    TEST_CASE_F(Sample_GivenInputNearOne_ReturnsItem2, Fixture)
    {
        const CDF::ItemWeightPair result = m_cdf.sample(0.99);

        EXPECT_EQ(2, result.first);
        EXPECT_FEQ(0.8, result.second);
    }

    TEST_CASE_F(Sample_GivenInputOneUlpBeforeOne_ReturnsItem2, Fixture)
    {
        const double almost_one = shift(1.0, -1);
        const CDF::ItemWeightPair result = m_cdf.sample(almost_one);

        EXPECT_EQ(2, result.first);
        EXPECT_FEQ(0.8, result.second);
    }

    TEST_CASE(TwoDimensional_CDF_Exploration)
    {
        CDF child[2];

        child[0].insert(1, 0.3);
        child[0].insert(2, 0.7);
        child[0].prepare();

        child[1].insert(3, 0.1);
        child[1].insert(4, 0.4);
        child[1].prepare();

        CDF parent;
        parent.insert(0, child[0].weight());
        parent.insert(1, child[1].weight());
        parent.prepare();

        const double x = 0.3;   // will choose child[0]
        const double y = 0.6;   // will choose value 2

        const CDF::ItemWeightPair u = parent.sample(x);
        const CDF::ItemWeightPair v = child[u.first].sample(y);

        const int value = v.first;
        const double prob = u.second * v.second;

        EXPECT_EQ(2, value);
        EXPECT_FEQ(0.7 * (2.0 / 3), prob);
    }

    TEST_CASE(SampleCDF)
    {
        static const double Cdf[] =
        {
            0.03, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0
        };

        static const double Weights[] =
        {
            0.0, 0.07, 0.15, 0.23, 0.41, 0.77, 0.98
        };

        static const size_t Result[] =
        {
            0, 1, 2, 3, 5, 8, 10
        };

        const double* begin = Cdf;
        const double* end = Cdf + countof(Cdf);

        for (size_t i = 0, e = countof(Result); i < e; ++i)
            EXPECT_EQ(Result[i], sample_cdf(begin, end, Weights[i]));
    }

    TEST_CASE(SampleCDFLinearSearch)
    {
        static const double Cdf[] =
        {
            0.03, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0
        };

        static const double Weights[] =
        {
            0.0, 0.07, 0.15, 0.23, 0.41, 0.77, 0.98
        };

        static const size_t Result[] =
        {
            0, 1, 2, 3, 5, 8, 10
        };

        for (size_t i = 0, e = countof(Result); i < e; ++i)
            EXPECT_EQ(Result[i], sample_cdf_linear_search(Cdf, Weights[i]));
    }
}
