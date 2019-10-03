
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
#include "xmlfilebenchmarklistener.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/platform/types.h"
#include "foundation/utility/benchmark/benchmarksuite.h"
#include "foundation/utility/benchmark/ibenchmarkcase.h"
#include "foundation/utility/benchmark/timingresult.h"
#include "foundation/utility/indenter.h"

// Standard headers.
#include <cassert>
#include <cstdio>

namespace foundation
{

//
// XMLFileBenchmarkListener class implementation.
//

struct XMLFileBenchmarkListener::Impl
{
    FILE*       m_file;
    Indenter    m_indenter;
    bool        m_has_header;

    Impl()
      : m_file(nullptr)
      , m_indenter(4)
      , m_has_header(false)
    {
    }
};

XMLFileBenchmarkListener::XMLFileBenchmarkListener()
  : impl(new Impl())
{
}

XMLFileBenchmarkListener::~XMLFileBenchmarkListener()
{
    close();

    delete impl;
}

void XMLFileBenchmarkListener::release()
{
    delete this;
}

void XMLFileBenchmarkListener::begin_suite(
    const BenchmarkSuite&   benchmark_suite)
{
    if (!impl->m_has_header)
    {
        write_file_header();
        impl->m_has_header = true;
    }

    fprintf(
        impl->m_file,
        "%s<benchmarksuite name=\"%s\">\n",
        impl->m_indenter.c_str(),
        benchmark_suite.get_name());

    ++impl->m_indenter;
}

void XMLFileBenchmarkListener::end_suite(
    const BenchmarkSuite&   benchmark_suite)
{
    --impl->m_indenter;

    fprintf(
        impl->m_file,
        "%s</benchmarksuite>\n",
        impl->m_indenter.c_str());
}

void XMLFileBenchmarkListener::begin_case(
    const BenchmarkSuite&   benchmark_suite,
    const IBenchmarkCase&   benchmark_case)
{
    fprintf(
        impl->m_file,
        "%s<benchmarkcase name=\"%s\">\n",
        impl->m_indenter.c_str(),
        benchmark_case.get_name());

    ++impl->m_indenter;
}

void XMLFileBenchmarkListener::end_case(
    const BenchmarkSuite&   benchmark_suite,
    const IBenchmarkCase&   benchmark_case)
{
    --impl->m_indenter;

    fprintf(
        impl->m_file,
        "%s</benchmarkcase>\n",
        impl->m_indenter.c_str());
}

void XMLFileBenchmarkListener::write(
    const BenchmarkSuite&   benchmark_suite,
    const IBenchmarkCase&   benchmark_case,
    const char*             file,
    const size_t            line,
    const char*             message)
{
    // todo: properly escape the string.
    fprintf(
        impl->m_file,
        "%s<message>%s</message>\n",
        impl->m_indenter.c_str(),
        message);
}

void XMLFileBenchmarkListener::write(
    const BenchmarkSuite&   benchmark_suite,
    const IBenchmarkCase&   benchmark_case,
    const char*             file,
    const size_t            line,
    const TimingResult&     timing_result)
{
    fprintf(impl->m_file, "%s<results>\n", impl->m_indenter.c_str());

    ++impl->m_indenter;

    fprintf(impl->m_file,
        "%s<iterations>" FMT_SIZE_T "</iterations>\n",
        impl->m_indenter.c_str(),
        timing_result.m_iteration_count);

    fprintf(impl->m_file,
        "%s<measurements>" FMT_SIZE_T "</measurements>\n",
        impl->m_indenter.c_str(),
        timing_result.m_measurement_count);

    fprintf(impl->m_file,
        "%s<frequency>%f</frequency>\n",
        impl->m_indenter.c_str(),
        timing_result.m_frequency);

    fprintf(impl->m_file,
        "%s<ticks>%f</ticks>\n",
        impl->m_indenter.c_str(),
        timing_result.m_ticks);

    --impl->m_indenter;

    fprintf(impl->m_file, "%s</results>\n", impl->m_indenter.c_str());
}

bool XMLFileBenchmarkListener::open(const char* filename)
{
    assert(filename);

    close();

    impl->m_file = fopen(filename, "wt");

    return impl->m_file != nullptr;
}

void XMLFileBenchmarkListener::close()
{
    if (impl->m_file)
    {
        if (impl->m_has_header)
            write_file_footer();

        fclose(impl->m_file);

        impl->m_file = nullptr;
    }
}

bool XMLFileBenchmarkListener::is_open() const
{
    return impl->m_file != nullptr;
}

void XMLFileBenchmarkListener::write_file_header()
{
    fprintf(
        impl->m_file,
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        "<!-- File generated by %s. -->\n",
        Appleseed::get_synthetic_version_string());

    fprintf(
        impl->m_file,
        "%s<benchmarkexecution configuration=\"%s\">\n",
        impl->m_indenter.c_str(),
        Appleseed::get_lib_configuration());

    ++impl->m_indenter;
}

void XMLFileBenchmarkListener::write_file_footer()
{
    --impl->m_indenter;

    fprintf(
        impl->m_file,
        "%s</benchmarkexecution>\n",
        impl->m_indenter.c_str());
}

XMLFileBenchmarkListener* create_xmlfile_benchmark_listener()
{
    return new XMLFileBenchmarkListener();
}

}   // namespace foundation
