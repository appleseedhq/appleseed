
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
#include "benchmarkresult.h"

// appleseed.foundation headers.
#include "foundation/platform/snprintf.h"
#include "foundation/utility/benchmark/ibenchmarklistener.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <cstdarg>
#include <list>


namespace foundation
{

//
// BenchmarkResult class implementation.
//

struct BenchmarkResult::Impl
{
    typedef std::list<IBenchmarkListener*> BenchmarkListenerContainer;

    BenchmarkListenerContainer  m_listeners;
    size_t                      m_suite_execution_count;
    size_t                      m_suite_failure_count;
    size_t                      m_case_execution_count;
    size_t                      m_case_failure_count;
};

BenchmarkResult::BenchmarkResult()
  : impl(new Impl())
{
    impl->m_suite_execution_count = 0;
    impl->m_suite_failure_count = 0;
    impl->m_case_execution_count = 0;
    impl->m_case_failure_count = 0;
}

BenchmarkResult::~BenchmarkResult()
{
    delete impl;
}

void BenchmarkResult::signal_suite_execution()
{
    ++impl->m_suite_execution_count;
}

void BenchmarkResult::signal_suite_failure()
{
    ++impl->m_suite_failure_count;
}

size_t BenchmarkResult::get_suite_execution_count() const
{
    return impl->m_suite_execution_count;
}

size_t BenchmarkResult::get_suite_failure_count() const
{
    return impl->m_suite_failure_count;
}

void BenchmarkResult::signal_case_execution()
{
    ++impl->m_case_execution_count;
}

void BenchmarkResult::signal_case_failure()
{
    ++impl->m_case_failure_count;
}

size_t BenchmarkResult::get_case_execution_count() const
{
    return impl->m_case_execution_count;
}

size_t BenchmarkResult::get_case_failure_count() const
{
    return impl->m_case_failure_count;
}

void BenchmarkResult::merge(const BenchmarkResult& rhs)
{
    impl->m_suite_execution_count += rhs.impl->m_suite_execution_count;
    impl->m_suite_failure_count += rhs.impl->m_suite_failure_count;
    impl->m_case_execution_count += rhs.impl->m_case_execution_count;
    impl->m_case_failure_count += rhs.impl->m_case_failure_count;
}

void BenchmarkResult::add_listener(IBenchmarkListener* listener)
{
    assert(listener);
    impl->m_listeners.push_back(listener);
}

void BenchmarkResult::add_listeners(const BenchmarkResult& rhs)
{
    impl->m_listeners.insert(
        impl->m_listeners.end(),
        rhs.impl->m_listeners.begin(),
        rhs.impl->m_listeners.end());
}

void BenchmarkResult::begin_suite(
    const BenchmarkSuite&   benchmark_suite)
{
    for (each<Impl::BenchmarkListenerContainer> i = impl->m_listeners; i; ++i)
        (*i)->begin_suite(benchmark_suite);
}

void BenchmarkResult::end_suite(
    const BenchmarkSuite&   benchmark_suite)
{
    for (each<Impl::BenchmarkListenerContainer> i = impl->m_listeners; i; ++i)
        (*i)->end_suite(benchmark_suite);
}

void BenchmarkResult::begin_case(
    const BenchmarkSuite&   benchmark_suite,
    const IBenchmarkCase&   benchmark_case)
{
    for (each<Impl::BenchmarkListenerContainer> i = impl->m_listeners; i; ++i)
        (*i)->begin_case(benchmark_suite, benchmark_case);
}

void BenchmarkResult::end_case(
    const BenchmarkSuite&   benchmark_suite,
    const IBenchmarkCase&   benchmark_case)
{
    for (each<Impl::BenchmarkListenerContainer> i = impl->m_listeners; i; ++i)
        (*i)->end_case(benchmark_suite, benchmark_case);
}

void BenchmarkResult::write(
    const BenchmarkSuite&             benchmark_suite,
    const IBenchmarkCase&             benchmark_case,
    const char*                       file,
    const size_t                      line,
    APPLESEED_PRINTF_FMT const char*  format, ...)
{
    // Size in bytes of the temporary message buffer.
    const size_t BufferSize = 4096;

    // Print the formatted message into the temporary buffer.
    va_list argptr;
    va_start(argptr, format);
    char message[BufferSize];
    portable_vsnprintf(message, BufferSize, format, argptr);

    // Send the message to all the listeners.
    for (each<Impl::BenchmarkListenerContainer> i = impl->m_listeners; i; ++i)
    {
        (*i)->write(
            benchmark_suite,
            benchmark_case,
            file,
            line,
            message);
    }
}

void BenchmarkResult::write(
    const BenchmarkSuite&   benchmark_suite,
    const IBenchmarkCase&   benchmark_case,
    const char*             file,
    const size_t            line,
    const TimingResult&     timing_result)
{
    // Send the timing result to all the listeners.
    for (each<Impl::BenchmarkListenerContainer> i = impl->m_listeners; i; ++i)
    {
        (*i)->write(
            benchmark_suite,
            benchmark_case,
            file,
            line,
            timing_result);
    }
}

}   // namespace foundation
