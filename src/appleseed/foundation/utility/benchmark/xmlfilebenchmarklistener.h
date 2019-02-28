
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
#include "foundation/utility/benchmark/benchmarklistenerbase.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IBenchmarkCase; }
namespace foundation    { class BenchmarkSuite; }
namespace foundation    { class TimingResult; }

namespace foundation
{

//
// A benchmark listener that outputs to a XML file.
//

class APPLESEED_DLLSYMBOL XMLFileBenchmarkListener
  : public BenchmarkListenerBase
{
  public:
    // Delete this instance.
    void release() override;

    // Called before each benchmark suite is run.
    void begin_suite(
        const BenchmarkSuite&   benchmark_suite) override;

    // Called after each benchmark suite is run.
    void end_suite(
        const BenchmarkSuite&   benchmark_suite) override;

    // Called before each benchmark case is run.
    void begin_case(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case) override;

    // Called after each benchmark case is run.
    void end_case(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case) override;

    // Write a message.
    void write(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case,
        const char*             file,
        const size_t            line,
        const char*             message) override;

    // Write a timing result.
    void write(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case,
        const char*             file,
        const size_t            line,
        const TimingResult&     timing_result) override;

    bool open(const char* filename);

    void close();

    bool is_open() const;

  private:
    friend APPLESEED_DLLSYMBOL XMLFileBenchmarkListener* create_xmlfile_benchmark_listener();

    struct Impl;
    Impl* impl;

    // Constructor.
    XMLFileBenchmarkListener();

    // Destructor.
    ~XMLFileBenchmarkListener() override;

    void write_file_header();
    void write_file_footer();
};

// Create an instance of a benchmark listener that outputs to a XML file.
APPLESEED_DLLSYMBOL XMLFileBenchmarkListener* create_xmlfile_benchmark_listener();

}   // namespace foundation
