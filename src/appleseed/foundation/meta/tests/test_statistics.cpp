
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/population.h"
#include "foundation/platform/types.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Utility_Statistics)
{
    TEST_CASE(EmptyStatistics)
    {
        Statistics stats;

        EXPECT_EQ("  no statistics", stats.to_string());
    }

    TEST_CASE(SingleUnsignedIntegerStatistic)
    {
        Statistics stats;

        stats.insert<uint64>("some value", 17000);

        EXPECT_EQ("  some value                    17,000", stats.to_string());
    }

    TEST_CASE(SingleFloatingPointStatistic)
    {
        Statistics stats;

        stats.insert("some value", 42.6);

        EXPECT_EQ("  some value                    42.6", stats.to_string());
    }

    TEST_CASE(SingleStringStatistic)
    {
        Statistics stats;

        stats.insert<string>("some value", "bunny");

        EXPECT_EQ("  some value                    bunny", stats.to_string());
    }

    TEST_CASE(SingleDefaultInitializedUnsignedIntegerPopulationStatistic)
    {
        Statistics stats;

        stats.insert("some value", Population<size_t>());

        EXPECT_EQ("  some value                    avg 0.0  min 0  max 0  dev 0.0", stats.to_string());
    }

    TEST_CASE(SingleUnsignedIntegerPopulationStatistic)
    {
        Statistics stats;

        Population<size_t> pop;
        pop.insert(1);
        pop.insert(2);
        pop.insert(3);

        stats.insert("some value", pop);

        EXPECT_EQ("  some value                    avg 2.0  min 1  max 3  dev 0.8", stats.to_string());
    }

    TEST_CASE(SingleUnsignedIntegerPopulationStatisticWithUnit)
    {
        Statistics stats;

        Population<size_t> pop;
        pop.insert(1);
        pop.insert(2);
        pop.insert(3);

        stats.insert("some value", pop, "%");

        EXPECT_EQ("  some value                    avg 2.0%  min 1%  max 3%  dev 0.8%", stats.to_string());
    }

    TEST_CASE(SingleFloatingPointPopulationStatistic)
    {
        Statistics stats;

        Population<double> pop;
        pop.insert(0.1);
        pop.insert(0.2);
        pop.insert(0.3);

        stats.insert("some value", pop);

        EXPECT_EQ("  some value                    avg 0.2  min 0.1  max 0.3  dev 0.1", stats.to_string());
    }

    TEST_CASE(SingleFloatingPointPopulationStatisticWithUnit)
    {
        Statistics stats;

        Population<double> pop;
        pop.insert(0.1);
        pop.insert(0.2);
        pop.insert(0.3);

        stats.insert("some value", pop, "%");

        EXPECT_EQ("  some value                    avg 0.2%  min 0.1%  max 0.3%  dev 0.1%", stats.to_string());
    }

    TEST_CASE(SingleStatisticWithLongTitle)
    {
        Statistics stats;

        stats.insert<uint64>("the name of this value is too long to fit", 17);

        EXPECT_EQ("  the name of this value is too 17", stats.to_string());
    }

    TEST_CASE(MultipleStatistics)
    {
        Statistics stats;

        stats.insert<uint64>("first value", 17);
        stats.insert("second value", 42.6);

        EXPECT_EQ("  first value                   17\n  second value                  42.6", stats.to_string());
    }

    TEST_CASE(Merge_GivenNewStatistic_InsertsIt)
    {
        Statistics stats;
        stats.insert<uint64>("existing value", 17);

        Statistics other_stats;
        other_stats.insert<uint64>("new value", 42);

        stats.merge(other_stats);

        EXPECT_EQ("  existing value                17\n  new value                     42", stats.to_string());
    }

    TEST_CASE(Merge_GivenExistingStatisticOfSameType_MergesIt)
    {
        Statistics stats;
        stats.insert<uint64>("existing value", 17000);

        Statistics other_stats;
        other_stats.insert<uint64>("existing value", 42);

        stats.merge(other_stats);

        EXPECT_EQ("  existing value                17,042", stats.to_string());
    }
}

TEST_SUITE(Foundation_Utility_StatisticsVector)
{
    TEST_CASE(ToString_GivenTwoitems)
    {
        Statistics stats1;
        stats1.insert<uint64>("counter 1", 17);

        Statistics stats2;
        stats2.insert<uint64>("counter 2", 42);

        StatisticsVector vec;
        vec.insert("stats 1", stats1);
        vec.insert("stats 2", stats2);

        EXPECT_EQ("stats 1:\n  counter 1                     17\nstats 2:\n  counter 2                     42", vec.to_string());
    }
}
