
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_TEST_TESTSUITE_H
#define APPLESEED_FOUNDATION_UTILITY_TEST_TESTSUITE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IFilter; }
namespace foundation    { class ITestCaseFactory; }
namespace foundation    { class ITestListener; }
namespace foundation    { class TestResult; }

namespace foundation
{

//
// A test suite, as a collection of tests.
//

class APPLESEED_DLLSYMBOL TestSuite
  : public NonCopyable
{
  public:
    // Constructor.
    explicit TestSuite(const char* name);

    // Destructor.
    ~TestSuite();

    // Return the name of the test suite.
    const char* get_name() const;

    // Register a test case (via its factory).
    void register_case(ITestCaseFactory* factory);

    // Retrieve the number of registered test cases.
    size_t get_case_count() const;

    // Retrieve a given registered test case factory.
    ITestCaseFactory* get_case_factory(const size_t index) const;

    // Run all the registered test cases.
    void run(
        ITestListener&      test_listener,
        TestResult&         cumulated_result) const;

    // Run those test cases whose name pass a given filter.
    void run(
        const IFilter&      filter,
        ITestListener&      test_listener,
        TestResult&         cumulated_result) const;

  private:
    struct Impl;
    Impl* impl;

    void run_suite(
        const IFilter&      filter,
        ITestListener&      test_listener,
        TestResult&         test_suite_result,
        TestResult&         cumulated_result) const;

    void run_case(
        ITestCaseFactory&   test_case_factory,
        ITestListener&      test_listener,
        TestResult&         test_case_result) const;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_TEST_TESTSUITE_H
