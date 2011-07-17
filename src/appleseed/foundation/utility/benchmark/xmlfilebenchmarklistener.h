
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_UTILITY_BENCHMARK_XMLFILEBENCHMARKLISTENER_H
#define APPLESEED_FOUNDATION_UTILITY_BENCHMARK_XMLFILEBENCHMARKLISTENER_H

// appleseed.foundation headers.
#include "foundation/utility/benchmark/benchmarklistenerbase.h"
#include "foundation/utility/implptr.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IBenchmarkCase; }
namespace foundation    { class BenchmarkSuite; }
namespace foundation    { class TimingResult; }

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

//
// A benchmark listener that outputs to a XML file.
//

class FOUNDATIONDLL XMLFileBenchmarkListener
  : public BenchmarkListenerBase
{
  public:
    // Delete this instance.
    virtual void release();

    // Called before each benchmark suite is run.
    virtual void begin_suite(
        const BenchmarkSuite&   benchmark_suite);

    // Called after each benchmark suite is run.
    virtual void end_suite(
        const BenchmarkSuite&   benchmark_suite);

    // Called before each benchmark case is run.
    virtual void begin_case(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case);

    // Called after each benchmark case is run.
    virtual void end_case(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case);

    // Write a message.
    virtual void write(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case,
        const char*             file,
        const size_t            line,
        const char*             message);

    // Write a timing result.
    virtual void write(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case,
        const char*             file,
        const size_t            line,
        const TimingResult&     timing_result);

    bool open(const char* filename);

    void close();

    bool is_open() const;

  private:
    friend FOUNDATIONDLL XMLFileBenchmarkListener* create_xmlfile_benchmark_listener();

    PIMPL(XMLFileBenchmarkListener);

    // Constructor.
    XMLFileBenchmarkListener();

    // Destructor.
    ~XMLFileBenchmarkListener();

    void write_file_header();
    void write_file_footer();
};

// Create an instance of a benchmark listener that outputs to a XML file.
FOUNDATIONDLL XMLFileBenchmarkListener* create_xmlfile_benchmark_listener();

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_BENCHMARK_XMLFILEBENCHMARKLISTENER_H
