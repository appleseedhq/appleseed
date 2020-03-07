
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

// Interface header.
#include "loggerbenchmarklistener.h"

// appleseed.foundation headers.
#include "foundation/log/log.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/benchmark/benchmarklistenerbase.h"
#include "foundation/utility/benchmark/benchmarksuite.h"
#include "foundation/utility/benchmark/ibenchmarkcase.h"
#include "foundation/utility/benchmark/timingresult.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <ios>
#include <string>
#include <vector>

namespace foundation
{

namespace
{
    std::string pretty_callrate(
        const TimingResult&      timing_result,
        const std::streamsize    precision = 1)
    {
        assert(timing_result.m_ticks > 0.0);

        const double KHz = 1000.0;
        const double MHz = 1000.0 * KHz;
        const double GHz = 1000.0 * MHz;
        const double THz = 1000.0 * GHz;

        const double rate = timing_result.m_frequency / timing_result.m_ticks;

        if (rate <= 1.0)
        {
            return pretty_scalar(rate) + " call/s";
        }
        else if (rate < KHz)
        {
            return pretty_scalar(rate) + " calls/s";
        }
        else if (rate < MHz)
        {
            return pretty_ratio(rate, KHz, precision) + "K calls/s";
        }
        else if (rate < GHz)
        {
            return pretty_ratio(rate, MHz, precision) + "M calls/s";
        }
        else if (rate < THz)
        {
            return pretty_ratio(rate, GHz, precision) + "G calls/s";
        }
        else
        {
            return pretty_ratio(rate, THz, precision) + "T calls/s";
        }
    }

    TEST_SUITE(Foundation_Utility_Benchmark)
    {
        std::string pretty_callrate_helper(const double rate)
        {
            TimingResult result;
            result.m_ticks = 1.0;
            result.m_frequency = rate;
            return pretty_callrate(result);
        }

        TEST_CASE(TestPrettyCallRate)
        {
            EXPECT_EQ("1.0 call/s", pretty_callrate_helper(1.0));
            EXPECT_EQ("10.0 calls/s", pretty_callrate_helper(10.0));
            EXPECT_EQ("100.0 calls/s", pretty_callrate_helper(100.0));
            EXPECT_EQ("1.0K calls/s", pretty_callrate_helper(1000.0));
            EXPECT_EQ("10.0K calls/s", pretty_callrate_helper(10000.0));
            EXPECT_EQ("10.0M calls/s", pretty_callrate_helper(10000000.0));
            EXPECT_EQ("10.0G calls/s", pretty_callrate_helper(10000000000.0));
            EXPECT_EQ("10.0T calls/s", pretty_callrate_helper(10000000000000.0));
        }
    }


    //
    // LoggerBenchmarkListener class implementation.
    //

    class LoggerBenchmarkListener
      : public BenchmarkListenerBase
    {
      public:
        explicit LoggerBenchmarkListener(Logger& logger)
          : m_logger(logger)
          , m_suite_name_printed(false)
        {
        }

        void release() override
        {
            delete this;
        }

        void begin_suite(
            const BenchmarkSuite&   benchmark_suite) override
        {
            m_suite_name_printed = false;
        }

        void write(
            const BenchmarkSuite&   benchmark_suite,
            const IBenchmarkCase&   benchmark_case,
            const char*             file,
            const size_t            line,
            const char*             message) override
        {
            print_suite_name(benchmark_suite);

            // Print the message header.
            LOG_ERROR(
                m_logger,
                "while benchmarking %s::%s: error in %s at line " FMT_SIZE_T ":",
                benchmark_suite.get_name(),
                benchmark_case.get_name(),
                file,
                line);

            // Split the message into multiple components, one for each line.
            std::vector<std::string> tokens;
            split(message, "\n", tokens);

            // Print the message.
            for (const_each<std::vector<std::string>> i = tokens; i; ++i)
                LOG_ERROR(m_logger, "    %s", i->c_str());
        }

        void write(
            const BenchmarkSuite&   benchmark_suite,
            const IBenchmarkCase&   benchmark_case,
            const char*             file,
            const size_t            line,
            const TimingResult&     timing_result) override
        {
            std::string callrate_string;

            if (timing_result.m_ticks > 0.0)
            {
                const double freq_mhz = timing_result.m_frequency * 1.0e-6;

                callrate_string =
                    format("({0} at {1} MHz, {2} {3})",
                        pretty_callrate(timing_result, 3),
                        pretty_scalar(freq_mhz, 3),
                        pretty_uint(timing_result.m_measurement_count),
                        plural(timing_result.m_measurement_count, "measurement"));
            }

            print_suite_name(benchmark_suite);

            LOG_INFO(
                m_logger,
                "  %s: %s %s %s",
                benchmark_case.get_name(),
                timing_result.m_ticks >= 1000.0
                    ? pretty_uint(static_cast<std::uint64_t>(timing_result.m_ticks)).c_str()
                    : pretty_scalar(timing_result.m_ticks).c_str(),
                plural(timing_result.m_ticks, "tick").c_str(),
                callrate_string.c_str());
        }

      private:
        Logger&     m_logger;
        bool        m_suite_name_printed;

        void print_suite_name(const BenchmarkSuite& benchmark_suite)
        {
            if (!m_suite_name_printed)
            {
                LOG_INFO(m_logger, "%s:", benchmark_suite.get_name());
                m_suite_name_printed = true;
            }
        }
    };
}

IBenchmarkListener* create_logger_benchmark_listener(Logger& logger)
{
    return new LoggerBenchmarkListener(logger);
}

}   // namespace foundation
