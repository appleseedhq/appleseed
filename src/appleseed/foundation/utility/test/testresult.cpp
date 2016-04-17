
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "testresult.h"

namespace foundation
{

//
// TestResult class implementation.
//

TestResult::TestResult()
  : m_suite_execution_count(0)
  , m_suite_failure_count(0)
  , m_case_execution_count(0)
  , m_case_failure_count(0)
  , m_assertion_execution_count(0)
  , m_assertion_failure_count(0)
{
}

void TestResult::signal_suite_execution()
{
    ++m_suite_execution_count;
}

void TestResult::signal_suite_failure()
{
    ++m_suite_failure_count;
}

size_t TestResult::get_suite_execution_count() const
{
    return m_suite_execution_count;
}

size_t TestResult::get_suite_failure_count() const
{
    return m_suite_failure_count;
}

void TestResult::signal_case_execution()
{
    ++m_case_execution_count;
}

void TestResult::signal_case_failure()
{
    ++m_case_failure_count;
}

size_t TestResult::get_case_execution_count() const
{
    return m_case_execution_count;
}

size_t TestResult::get_case_failure_count() const
{
    return m_case_failure_count;
}

void TestResult::signal_assertion_execution()
{
    ++m_assertion_execution_count;
}

void TestResult::signal_assertion_failure()
{
    ++m_assertion_failure_count;
}

size_t TestResult::get_assertion_execution_count() const
{
    return m_assertion_execution_count;
}

size_t TestResult::get_assertion_failure_count() const
{
    return m_assertion_failure_count;
}

void TestResult::merge(const TestResult& rhs)
{
    m_suite_execution_count += rhs.m_suite_execution_count;
    m_suite_failure_count += rhs.m_suite_failure_count;
    m_case_execution_count += rhs.m_case_execution_count;
    m_case_failure_count += rhs.m_case_failure_count;
    m_assertion_execution_count += rhs.m_assertion_execution_count;
    m_assertion_failure_count += rhs.m_assertion_failure_count;
}

}   // namespace foundation
