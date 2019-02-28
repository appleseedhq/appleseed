
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class BenchmarkSuite; }
namespace foundation    { class IBenchmarkCase; }
namespace foundation    { class IBenchmarkListener; }
namespace foundation    { class TimingResult; }

namespace foundation
{

//
// The object through which benchmarks report their results.
//

class APPLESEED_DLLSYMBOL BenchmarkResult
  : public NonCopyable
{
  public:
    // Constructor.
    BenchmarkResult();

    // Destructor.
    ~BenchmarkResult();

    // Report and retrieve benchmark suite execution metrics.
    void signal_suite_execution();
    void signal_suite_failure();
    size_t get_suite_execution_count() const;
    size_t get_suite_failure_count() const;

    // Report and retrieve benchmark case execution metrics.
    void signal_case_execution();
    void signal_case_failure();
    size_t get_case_execution_count() const;
    size_t get_case_failure_count() const;

    // Merge another benchmark result into this one.
    void merge(const BenchmarkResult& rhs);

    // Add a benchmark listener.
    void add_listener(IBenchmarkListener* listener);

    // Add the benchmark listeners from another benchmark result object.
    void add_listeners(const BenchmarkResult& rhs);

    // Called before each benchmark suite is run.
    void begin_suite(
        const BenchmarkSuite&   benchmark_suite);

    // Called after each benchmark suite is run.
    void end_suite(
        const BenchmarkSuite&   benchmark_suite);

    // Called before each benchmark case is run.
    void begin_case(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case);

    // Called after each benchmark case is run.
    void end_case(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case);

    // Write a message.
    void write(
        const BenchmarkSuite&             benchmark_suite,
        const IBenchmarkCase&             benchmark_case,
        const char*                       file,
        const size_t                      line,
        APPLESEED_PRINTF_FMT const char*  format, ...)
        APPLESEED_PRINTF_FMT_ATTR(6, 7);

    // Write a timing result.
    void write(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case,
        const char*             file,
        const size_t            line,
        const TimingResult&     timing_result);

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace foundation
