
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

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// The purpose of this class is to gather test execution results.
//

class APPLESEED_DLLSYMBOL TestResult
{
  public:
    // Constructor.
    TestResult();

    // Report and retrieve test suite execution metrics.
    void signal_suite_execution();
    void signal_suite_failure();
    size_t get_suite_execution_count() const;
    size_t get_suite_failure_count() const;

    // Report and retrieve test case execution metrics.
    void signal_case_execution();
    void signal_case_failure();
    size_t get_case_execution_count() const;
    size_t get_case_failure_count() const;

    // Report and retrieve assertion execution metrics.
    void signal_assertion_execution();
    void signal_assertion_failure();
    size_t get_assertion_execution_count() const;
    size_t get_assertion_failure_count() const;

    // Merge another test result into this one.
    void merge(const TestResult& rhs);

  private:
    size_t  m_suite_execution_count;
    size_t  m_suite_failure_count;
    size_t  m_case_execution_count;
    size_t  m_case_failure_count;
    size_t  m_assertion_execution_count;
    size_t  m_assertion_failure_count;
};

}   // namespace foundation
