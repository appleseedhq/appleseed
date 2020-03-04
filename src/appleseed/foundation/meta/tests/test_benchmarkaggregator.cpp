
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
#include "foundation/containers/dictionary.h"
#include "foundation/platform/datetime.h"
#include "foundation/utility/benchmark/benchmarkaggregator.h"
#include "foundation/utility/benchmark/benchmarkdatapoint.h"
#include "foundation/utility/benchmark/benchmarkseries.h"
#include "foundation/utility/test.h"
#include "foundation/utility/uid.h"

// Boost headers.
#include "boost/date_time/gregorian/gregorian.hpp"
#include "boost/date_time/posix_time/posix_time.hpp"

using namespace boost;
using namespace boost::gregorian;
using namespace boost::posix_time;
using namespace foundation;

TEST_SUITE(Foundation_Utility_Benchmark_BenchmarkDataPoint)
{
    TEST_CASE(GetDate_ReturnsDatePassedToConstructor)
    {
        const ptime ExpectedDate(date(2010, 6, 22), time_duration(17, 45, 31));
        const double ExpectedTicks = 1234.5678;
        const BenchmarkDataPoint data_point(ExpectedDate, ExpectedTicks);

        const ptime date = data_point.get_date();

        EXPECT_EQ(ExpectedDate, date);
    }

    TEST_CASE(GetTicks_ReturnsTicksPassedToConstructor)
    {
        const ptime ExpectedDate(date(2010, 6, 22), time_duration(17, 45, 31));
        const double ExpectedTicks = 1234.5678;
        const BenchmarkDataPoint data_point(ExpectedDate, ExpectedTicks);

        const double ticks = data_point.get_ticks();

        EXPECT_EQ(ExpectedTicks, ticks);
    }
}

TEST_SUITE(Foundation_Utility_Benchmark_BenchmarkAggregator)
{
    TEST_CASE(EmptyDirectory)
    {
        BenchmarkAggregator aggregator;
        aggregator.scan_directory("unit tests/inputs/test_benchmarkaggregator/empty directory");
        aggregator.sort_series();

        EXPECT_TRUE(aggregator.get_benchmarks().empty());
    }

    TEST_CASE(IncompleteBenchmarkFile)
    {
        BenchmarkAggregator aggregator;
        aggregator.scan_directory("unit tests/inputs/test_benchmarkaggregator/incomplete benchmark file/");
        aggregator.sort_series();

        const Dictionary& benchmarks = aggregator.get_benchmarks();

        const BenchmarkSeries& series =
            aggregator.get_series(
                benchmarks.dictionaries()
                    .get("Release").dictionaries()
                    .get("Suite")
                    .get<UniqueID>("Case"));

        EXPECT_EQ(1, series.size());
    }

    TEST_CASE(NonBenchmarkFile)
    {
        BenchmarkAggregator aggregator;
        aggregator.scan_directory("unit tests/inputs/test_benchmarkaggregator/non benchmark file/");
        aggregator.sort_series();

        EXPECT_TRUE(aggregator.get_benchmarks().empty());
    }

    TEST_CASE(SingleBenchmarkFile)
    {
        BenchmarkAggregator aggregator;
        aggregator.scan_directory("unit tests/inputs/test_benchmarkaggregator/single benchmark file/");
        aggregator.sort_series();

        const ptime Date(date(2010, 6, 22), time_duration(17, 45, 31));

        const Dictionary& benchmarks = aggregator.get_benchmarks();

        const BenchmarkSeries& series1 =
            aggregator.get_series(
                benchmarks.dictionaries()
                    .get("Release").dictionaries()
                    .get("Suite")
                    .get<UniqueID>("Case1"));

        ASSERT_EQ(1, series1.size());
        EXPECT_EQ(Date, series1[0].get_date());
        EXPECT_EQ(779.34, series1[0].get_ticks());

        const BenchmarkSeries& series2 =
            aggregator.get_series(
                benchmarks.dictionaries()
                    .get("Release").dictionaries()
                    .get("Suite")
                    .get<UniqueID>("Case2"));

        ASSERT_EQ(1, series2.size());
        EXPECT_EQ(Date, series2[0].get_date());
        EXPECT_EQ(877.22, series2[0].get_ticks());
    }

    TEST_CASE(MultipleBenchmarkFiles)
    {
        BenchmarkAggregator aggregator;
        aggregator.scan_directory("unit tests/inputs/test_benchmarkaggregator/multiple benchmark files/");
        aggregator.sort_series();

        const Dictionary& benchmarks = aggregator.get_benchmarks();

        const BenchmarkSeries& series =
            aggregator.get_series(
                benchmarks.dictionaries()
                    .get("Release").dictionaries()
                    .get("Suite")
                    .get<UniqueID>("Case"));

        ASSERT_EQ(2, series.size());

        EXPECT_EQ(ptime(date(2009, 5, 21), time_duration(16, 44, 30)), series[0].get_date());
        EXPECT_EQ(779.34, series[0].get_ticks());

        EXPECT_EQ(ptime(date(2010, 6, 22), time_duration(17, 45, 31)), series[1].get_date());
        EXPECT_EQ(877.22, series[1].get_ticks());
    }

    TEST_CASE(Clear_GivenOneBenchmark_RemovesBenchmark)
    {
        BenchmarkAggregator aggregator;
        aggregator.scan_directory("unit tests/inputs/test_benchmarkaggregator/single benchmark file/");
        aggregator.sort_series();

        aggregator.clear();

        ASSERT_TRUE(aggregator.get_benchmarks().empty());
    }
}
