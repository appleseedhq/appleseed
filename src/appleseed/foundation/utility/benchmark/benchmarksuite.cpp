
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "benchmarksuite.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"
#include "foundation/platform/timers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/benchmark/benchmarkresult.h"
#include "foundation/utility/benchmark/ibenchmarkcase.h"
#include "foundation/utility/benchmark/ibenchmarkcasefactory.h"
#include "foundation/utility/benchmark/timingresult.h"
#include "foundation/utility/filter.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <exception>
#include <limits>
#include <memory>
#include <string>
#include <vector>

using namespace std;

namespace foundation
{

//
// BenchmarkSuite class implementation.
//

namespace
{
    // An empty benchmark case used for measuring the overhead of calling IBenchmarkCase::run().
    struct EmptyBenchmarkCase
      : public IBenchmarkCase
    {
        virtual const char* get_name() const
        {
            return "Empty";
        }

        virtual void run()
        {
        }
    };
}

struct BenchmarkSuite::Impl
{
#if APPLESEED_X86
    typedef Stopwatch<X86Timer> StopwatchType;
#else
    typedef Stopwatch<DefaultProcessorTimer> StopwatchType;
#endif

    string                          m_name;
    vector<IBenchmarkCaseFactory*>  m_factories;

    static double measure_runtime_seconds(
        IBenchmarkCase*         benchmark,
        StopwatchType&          stopwatch,
        const size_t            iteration_count)
    {
        stopwatch.start();

        for (size_t i = 0; i < iteration_count; ++i)
            benchmark->run();

        stopwatch.measure();

        return stopwatch.get_seconds();
    }

    static double measure_runtime_ticks(
        IBenchmarkCase*         benchmark,
        StopwatchType&          stopwatch,
        const size_t            iteration_count)
    {
        stopwatch.start();

        for (size_t i = 0; i < iteration_count; ++i)
            benchmark->run();

        stopwatch.measure();

        return static_cast<double>(stopwatch.get_ticks());
    }

    template <typename MeasurementFunction>
    static double measure_runtime(
        IBenchmarkCase*         benchmark,
        StopwatchType&          stopwatch,
        MeasurementFunction&    measurement_function,
        const size_t            iteration_count,
        const size_t            measurement_count)
    {
        double lowest_runtime = numeric_limits<double>::max();

        for (size_t i = 0; i < measurement_count; ++i)
        {
            const double runtime =
                measurement_function(
                    benchmark,
                    stopwatch,
                    iteration_count);

            lowest_runtime = min(lowest_runtime, runtime);
        }

        return lowest_runtime;
    }

    struct BenchmarkParams
    {
        size_t  m_iteration_count;
        size_t  m_measurement_count;
    };

    static void estimate_benchmark_params(
        IBenchmarkCase*         benchmark,
        StopwatchType&          stopwatch,
        BenchmarkParams&        params)
    {
        const size_t InitialIterationCount = 1;
        const size_t InitialMeasurementCount = 3;
        const double TargetMeasurementTime = 1.0e-3;    // seconds
        const double TargetTotalTime = 0.5;             // seconds

        // Measure the runtime for the initial number of iterations.
        const double time =
            measure_runtime(
                benchmark,
                stopwatch,
                BenchmarkSuite::Impl::measure_runtime_seconds,
                InitialIterationCount,
                InitialMeasurementCount);

        // Compute the number of iterations.
        const double iteration_time = time / InitialIterationCount;
        params.m_iteration_count = max<size_t>(1, static_cast<size_t>(TargetMeasurementTime / iteration_time));

        // Compute the number of measurements.
        const double measurement_time = iteration_time * params.m_iteration_count;
        params.m_measurement_count = max<size_t>(1, static_cast<size_t>(TargetTotalTime / measurement_time));
    }

    static double measure_iteration_runtime(
        IBenchmarkCase*         benchmark,
        StopwatchType&          stopwatch,
        const BenchmarkParams&  params)
    {
        return
            measure_runtime(
                benchmark,
                stopwatch,
                BenchmarkSuite::Impl::measure_runtime_ticks,
                params.m_iteration_count,
                params.m_measurement_count)
            / params.m_iteration_count;
    }

    // Measure and return the overhead (in ticks) of running an empty benchmark case.
    static double measure_call_overhead(
        StopwatchType&          stopwatch,
        const BenchmarkParams&  params)
    {
        auto_ptr<IBenchmarkCase> empty_case(new EmptyBenchmarkCase());
        return measure_iteration_runtime(empty_case.get(), stopwatch, params);
    }
};

BenchmarkSuite::BenchmarkSuite(const char* name)
  : impl(new Impl())
{
    assert(name);
    impl->m_name = name;
}

BenchmarkSuite::~BenchmarkSuite()
{
    delete impl;
}

const char* BenchmarkSuite::get_name() const
{
    return impl->m_name.c_str();
}

void BenchmarkSuite::register_case(IBenchmarkCaseFactory* factory)
{
    assert(factory);
    impl->m_factories.push_back(factory);
}

void BenchmarkSuite::run(BenchmarkResult& suite_result) const
{
    PassThroughFilter filter;
    run(filter, suite_result);
}

void BenchmarkSuite::run(
    const IFilter&      filter,
    BenchmarkResult&    suite_result) const
{
    BenchmarkingThreadContext benchmarking_context;
    bool has_begun_suite = false;

    for (size_t i = 0; i < impl->m_factories.size(); ++i)
    {
        IBenchmarkCaseFactory* factory = impl->m_factories[i];

        // Skip benchmark cases that aren't let through by the filter.
        if (!filter.accepts(factory->get_name()))
            continue;

        if (!has_begun_suite)
        {
            // Tell the listeners that a benchmark suite is about to be executed.
            suite_result.begin_suite(*this);
            suite_result.signal_suite_execution();
            has_begun_suite = true;
        }

        // Instantiate the benchmark case.
        auto_ptr<IBenchmarkCase> benchmark(factory->create());

        // Recreate the stopwatch (and the underlying timer) for every benchmark
        // case, since the CPU frequency will fluctuate quite a bit depending on
        // the CPU load.  We need an up-to-date frequency estimation in order to
        // compute accurate call rates.
        Impl::StopwatchType stopwatch(100000);

        // Tell the listeners that a benchmark case is about to be executed.
        suite_result.begin_case(*this, *benchmark.get());

#ifdef NDEBUG
        try
#endif
        {
            suite_result.signal_case_execution();

            // Estimate benchmarking parameters.
            Impl::BenchmarkParams params;
            Impl::estimate_benchmark_params(
                benchmark.get(),
                stopwatch,
                params);

            // Measure the overhead of calling IBenchmarkCase::run().
            const double overhead =
                Impl::measure_call_overhead(stopwatch, params);

            // Run the benchmark case.
            const double execution_time =
                Impl::measure_iteration_runtime(
                    benchmark.get(),
                    stopwatch,
                    params);

            // Gather the timing results.
            TimingResult timing_result;
            timing_result.m_iteration_count = params.m_iteration_count;
            timing_result.m_measurement_count = params.m_measurement_count;
            timing_result.m_frequency = static_cast<double>(stopwatch.get_timer().frequency());
            timing_result.m_ticks = execution_time > overhead ? execution_time - overhead : 0.0;

            // Post the timing result.
            suite_result.write(
                *this,
                *benchmark.get(),
                __FILE__,
                __LINE__,
                timing_result);
        }
#ifdef NDEBUG
        catch (const exception& e)
        {
            suite_result.write(
                *this,
                *benchmark.get(),
                __FILE__,
                __LINE__,
                "an unexpected exception was caught: %s.",
                e.what());

            suite_result.signal_case_failure();
        }
        catch (...)
        {
            suite_result.write(
                *this,
                *benchmark.get(),
                __FILE__,
                __LINE__,
                "an unexpected exception was caught (no details available).");

            suite_result.signal_case_failure();
        }
#endif

        // Tell the listeners that the benchmark case execution has ended.
        suite_result.end_case(*this, *benchmark.get());
    }

    if (has_begun_suite)
    {
        // Report a benchmark suite failure if one or more benchmark cases failed.
        if (suite_result.get_case_failure_count() > 0)
            suite_result.signal_suite_failure();

        // Tell the listeners that the benchmark suite execution has ended.
        suite_result.end_suite(*this);
    }
}

}   // namespace foundation
