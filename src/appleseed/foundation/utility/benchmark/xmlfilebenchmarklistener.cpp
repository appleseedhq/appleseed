
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "xmlfilebenchmarklistener.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/platform/types.h"
#include "foundation/utility/benchmark/benchmarklistenerbase.h"
#include "foundation/utility/benchmark/benchmarksuite.h"
#include "foundation/utility/benchmark/ibenchmarkcase.h"
#include "foundation/utility/benchmark/timingresult.h"
#include "foundation/utility/indenter.h"

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace std;

namespace foundation
{

namespace
{

    //
    // XMLFileBenchmarkListener class implementation.
    //

    class XMLFileBenchmarkListener
      : public BenchmarkListenerBase
    {
      public:
        // Constructor.
        explicit XMLFileBenchmarkListener(FILE* file)
          : m_file(file)
          , m_indenter(4)
          , m_has_header(false)
        {
            assert(m_file);
        }

        // Destructor.
        ~XMLFileBenchmarkListener()
        {
            if (m_has_header)
                write_file_footer();
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Called before each benchmark suite is run.
        virtual void begin_suite(
            const BenchmarkSuite&   benchmark_suite)
        {
            if (!m_has_header)
            {
                write_file_header();
                m_has_header = true;
            }

            fprintf(
                m_file,
                "%s<benchmarksuite name=\"%s\">\n",
                m_indenter.c_str(),
                benchmark_suite.get_name());
            ++m_indenter;
        }

        // Called after each benchmark suite is run.
        virtual void end_suite(
            const BenchmarkSuite&   benchmark_suite)
        {
            --m_indenter;
            fprintf(
                m_file,
                "%s</benchmarksuite>\n",
                m_indenter.c_str());
        }

        // Called before each benchmark case is run.
        virtual void begin_case(
            const BenchmarkSuite&   benchmark_suite,
            const IBenchmarkCase&   benchmark_case)
        {
            fprintf(
                m_file,
                "%s<benchmarkcase name=\"%s\">\n",
                m_indenter.c_str(),
                benchmark_case.get_name());
            ++m_indenter;
        }

        // Called after each benchmark case is run.
        virtual void end_case(
            const BenchmarkSuite&   benchmark_suite,
            const IBenchmarkCase&   benchmark_case)
        {
            --m_indenter;
            fprintf(
                m_file,
                "%s</benchmarkcase>\n",
                m_indenter.c_str());
        }

        // Write a message.
        virtual void write(
            const BenchmarkSuite&   benchmark_suite,
            const IBenchmarkCase&   benchmark_case,
            const char*             file,
            const size_t            line,
            const char*             message)
        {
            // todo: properly escape the string.
            fprintf(
                m_file,
                "%s<message>%s</message>\n",
                m_indenter.c_str());
        }

        // Write a timing result.
        virtual void write(
            const BenchmarkSuite&   benchmark_suite,
            const IBenchmarkCase&   benchmark_case,
            const char*             file,
            const size_t            line,
            const TimingResult&     timing_result)
        {
            fprintf(m_file, "%s<results>\n", m_indenter.c_str());
            ++m_indenter;

            fprintf(m_file,
                "%s<iterations>" FMT_SIZE_T "</iterations>\n",
                m_indenter.c_str(),
                timing_result.m_iteration_count);

            fprintf(m_file,
                "%s<measurements>" FMT_SIZE_T "</measurements>\n",
                m_indenter.c_str(),
                timing_result.m_measurement_count);

            fprintf(m_file,
                "%s<frequency>%f</frequency>\n",
                m_indenter.c_str(),
                timing_result.m_frequency);

            fprintf(m_file,
                "%s<ticks>%f</ticks>\n",
                m_indenter.c_str(),
                timing_result.m_ticks);

            --m_indenter;
            fprintf(m_file, "%s</results>\n", m_indenter.c_str());
        }

      private:
        FILE*       m_file;
        Indenter    m_indenter;
        bool        m_has_header;

        void write_file_header()
        {
            fprintf(
                m_file,
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                "<!-- File generated by %s version %s (build " FMT_SIZE_T "). -->\n",
                Appleseed::get_lib_name(),
                Appleseed::get_lib_version(),
                Appleseed::get_lib_build_number());

            fprintf(
                m_file,
                "%s<benchmarkexecution configuration=\"%s\">\n",
                m_indenter.c_str(),
                Appleseed::get_lib_configuration());
            ++m_indenter;
        }

        void write_file_footer()
        {
            --m_indenter;
            fprintf(
                m_file,
                "%s</benchmarkexecution>\n",
                m_indenter.c_str());
        }
    };

}   // anonymous namespace

// Factory function.
IBenchmarkListener* create_xmlfile_benchmark_listener(FILE* file)
{
    return new XMLFileBenchmarkListener(file);
}

}   // namespace foundation
