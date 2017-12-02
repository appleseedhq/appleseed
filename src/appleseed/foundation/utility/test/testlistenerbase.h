
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

#ifndef APPLESEED_FOUNDATION_UTILITY_TEST_TESTLISTENERBASE_H
#define APPLESEED_FOUNDATION_UTILITY_TEST_TESTLISTENERBASE_H

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/test/itestlistener.h"
#include "foundation/utility/test/testmessage.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class TestResult; }
namespace foundation    { class TestSuite; }

namespace foundation
{

//
// A convenient base class for test listeners.
//

class APPLESEED_DLLSYMBOL TestListenerBase
  : public ITestListener
{
  public:
    // Called before each test suite is run.
    void begin_suite(
        const TestSuite&        test_suite) override
    {
    }

    // Called after each test suite is run.
    void end_suite(
        const TestSuite&        test_suite,
        const TestResult&       test_suite_result,
        const TestResult&       cumulated_result) override
    {
    }

    // Called before each test case is run.
    void begin_case(
        const TestSuite&        test_suite,
        const char*             test_case_name) override
    {
    }

    // Called after each test case is run.
    void end_case(
        const TestSuite&        test_suite,
        const char*             test_case_name,
        const TestResult&       test_suite_result,
        const TestResult&       test_case_result,
        const TestResult&       cumulated_result) override
    {
    }

    // Write a message.
    void write(
        const TestSuite&        test_suite,
        const char*             test_case_name,
        const char*             file,
        const size_t            line,
        const TestMessage::Type message_type,
        const char*             message) override
    {
    }
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_TEST_TESTLISTENERBASE_H
