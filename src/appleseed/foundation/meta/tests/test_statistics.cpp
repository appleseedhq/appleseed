
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
        Statistics stats("title");

        EXPECT_EQ("title:\n  no statistics\n", stats.to_string());
    }

    TEST_CASE(SingleUnsignedIntegerStatistic)
    {
        Statistics stats("title");

        stats.add<uint64>("some_value", "some value", 17);

        EXPECT_EQ("title:\n  some value       17\n", stats.to_string());
    }

    TEST_CASE(SingleUnsignedIntegerStatistic_SetAfterDeclaration)
    {
        Statistics stats("title");

        uint64* counter = stats.add<uint64>("some_value", "some value");

        *counter = 17;

        EXPECT_EQ("title:\n  some value       17\n", stats.to_string());
    }

    TEST_CASE(SingleFloatingPointStatistic)
    {
        Statistics stats("title");

        stats.add<double>("some_value", "some value", 42.6);

        EXPECT_EQ("title:\n  some value       42.6\n", stats.to_string());
    }

    TEST_CASE(SingleStringStatistic)
    {
        Statistics stats("title");

        stats.add<string>("some_value", "some value", "bunny");

        EXPECT_EQ("title:\n  some value       bunny\n", stats.to_string());
    }

    TEST_CASE(SingleUnsignedIntegerPopulationStatistic)
    {
        Statistics stats("title");

        Population<size_t> pop;
        pop.insert(1);
        pop.insert(2);
        pop.insert(3);

        stats.add_population<size_t>("some_value", "some value", pop);

        EXPECT_EQ("title:\n  some value       avg 2.0  min 1  max 3  dev 0.8\n", stats.to_string());
    }

    TEST_CASE(SingleUnsignedIntegerPopulationStatisticWithUnit)
    {
        Statistics stats("title");

        Population<size_t> pop;
        pop.insert(1);
        pop.insert(2);
        pop.insert(3);

        stats.add_population<size_t>("some_value", "some value", "%", pop);

        EXPECT_EQ("title:\n  some value       avg 2.0%  min 1%  max 3%  dev 0.8%\n", stats.to_string());
    }

    TEST_CASE(SingleFloatingPointPopulationStatistic)
    {
        Statistics stats("title");

        Population<double> pop;
        pop.insert(0.1);
        pop.insert(0.2);
        pop.insert(0.3);

        stats.add_population<double>("some_value", "some value", pop);

        EXPECT_EQ("title:\n  some value       avg 0.2  min 0.1  max 0.3  dev 0.1\n", stats.to_string());
    }

    TEST_CASE(SingleFloatingPointPopulationStatisticWithUnit)
    {
        Statistics stats("title");

        Population<double> pop;
        pop.insert(0.1);
        pop.insert(0.2);
        pop.insert(0.3);

        stats.add_population<double>("some_value", "some value", "%", pop);

        EXPECT_EQ("title:\n  some value       avg 0.2%  min 0.1%  max 0.3%  dev 0.1%\n", stats.to_string());
    }

    TEST_CASE(SingleStatisticWithLongTitle)
    {
        Statistics stats("title");

        stats.add<uint64>("some_value", "the title of this value is too long to fit", 17);

        EXPECT_EQ("title:\n  the title of thi 17\n", stats.to_string());
    }

    TEST_CASE(MultipleStatistics)
    {
        Statistics stats("title");

        stats.add<uint64>("first_value", "first value", 17);
        stats.add<double>("second_value", "second value", 42.6);

        EXPECT_EQ("title:\n  first value      17\n  second value     42.6\n", stats.to_string());
    }

    TEST_CASE(Merge_GivenNewStatistic_InsertsIt)
    {
        Statistics stats("title");
        stats.add<uint64>("existing_value", "existing value", 17);

        Statistics other_stats("other title");
        other_stats.add<uint64>("new_value", "new value", 42);

        stats.merge(other_stats);

        EXPECT_EQ("title:\n  existing value   17\n  new value        42\n", stats.to_string());
    }

    TEST_CASE(Merge_GivenExistingStatisticOfSameType_MergesIt)
    {
        Statistics stats("title");
        stats.add<uint64>("existing_value", "existing value", 17);

        Statistics other_stats("other title");
        other_stats.add<uint64>("existing_value", "existing value", 42);

        stats.merge(other_stats);

        EXPECT_EQ("title:\n  existing value   59\n", stats.to_string());
    }
}
