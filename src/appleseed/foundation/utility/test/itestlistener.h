
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

#ifndef APPLESEED_FOUNDATION_UTILITY_TEST_ITESTLISTENER_H
#define APPLESEED_FOUNDATION_UTILITY_TEST_ITESTLISTENER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/iunknown.h"
#include "foundation/utility/test/testmessage.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class ITestCase; }
namespace foundation    { class TestResult; }
namespace foundation    { class TestSuite; }

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
// Interface of a test listener.
//

class FOUNDATIONDLL ITestListener
  : public IUnknown
{
  public:
    // Called before each test suite is run.
    virtual void begin_suite(
        const TestSuite&        test_suite) = 0;

    // Called after each test suite is run.
    virtual void end_suite(
        const TestSuite&        test_suite,
        const TestResult&       test_suite_result,
        const TestResult&       cumulated_result) = 0;

    // Called before each test case is run.
    virtual void begin_case(
        const TestSuite&        test_suite,
        const ITestCase&        test_case) = 0;

    // Called after each test case is run.
    virtual void end_case(
        const TestSuite&        test_suite,
        const ITestCase&        test_case,
        const TestResult&       test_suite_result,
        const TestResult&       test_case_result,
        const TestResult&       cumulated_result) = 0;

    // Write a message.
    virtual void write(
        const TestSuite&        test_suite,
        const ITestCase&        test_case,
        const char*             file,
        const size_t            line,
        const TestMessage::Type message_type,
        const char*             message) = 0;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_TEST_ITESTLISTENER_H
