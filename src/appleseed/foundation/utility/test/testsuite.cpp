
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
#include "testsuite.h"

// appleseed.foundation headers.
#include "foundation/utility/filter/ifilter.h"
#include "foundation/utility/filter/letthroughfilter.h"
#include "foundation/utility/test/exceptionassertionfailure.h"
#include "foundation/utility/test/itestcase.h"
#include "foundation/utility/test/itestcasefactory.h"
#include "foundation/utility/test/itestlistener.h"
#include "foundation/utility/test/testlistenerhelper.h"
#include "foundation/utility/test/testmessage.h"
#include "foundation/utility/test/testresult.h"

// Standard headers.
#include <cassert>
#include <exception>
#include <memory>
#include <string>
#include <vector>

using namespace std;

namespace foundation
{

//
// TestSuite class implementation.
//

struct TestSuite::Impl
{
    string                      m_name;
    vector<ITestCaseFactory*>   m_factories;
};

// Constructor.
TestSuite::TestSuite(const char* name)
  : impl(new Impl())
{
    assert(name);
    impl->m_name = name;
}

// Destructor.
TestSuite::~TestSuite()
{
    delete impl;
}

// Return the name of the test suite.
const char* TestSuite::get_name() const
{
    return impl->m_name.c_str();
}

// Register a test case (via its factory).
void TestSuite::register_case(ITestCaseFactory* factory)
{
    assert(factory);
    impl->m_factories.push_back(factory);
}

// Retrieve the number of registered test cases.
size_t TestSuite::get_case_count() const
{
    return impl->m_factories.size();
}

// Retrieve a given registered test case factory.
ITestCaseFactory* TestSuite::get_case_factory(const size_t index) const
{
    assert(index < impl->m_factories.size());
    return impl->m_factories[index];
}

// Run all the registered test cases.
void TestSuite::run(
    ITestListener&  test_listener,
    TestResult&     cumulated_result) const
{
    LetThroughFilter filter;
    run(filter, test_listener, cumulated_result);
}

// Run those test cases whose name pass a given filter.
void TestSuite::run(
    const IFilter&  filter,
    ITestListener&  test_listener,
    TestResult&     cumulated_result) const
{
    // Tell the listener that a test suite is about to be executed.
    test_listener.begin_suite(*this);

    // Run the test suite.
    TestResult test_suite_result;
    run_suite(
        filter,
        test_listener,
        test_suite_result,
        cumulated_result);

    // Accumulate the results.
    cumulated_result.merge(test_suite_result);

    // Tell the listener that the test suite execution has ended.
    test_listener.end_suite(
        *this,
        test_suite_result,
        cumulated_result);
}

void TestSuite::run_suite(
    const IFilter&  filter,
    ITestListener&  test_listener,
    TestResult&     test_suite_result,
    TestResult&     cumulated_result) const
{
    test_suite_result.signal_suite_execution();

    TestResult local_cumulated_result = cumulated_result;
    local_cumulated_result.merge(test_suite_result);

    for (size_t i = 0; i < impl->m_factories.size(); ++i)
    {
        ITestCaseFactory* factory = impl->m_factories[i];

        // Skip test cases that aren't let through by the filter.
        if (!filter.accepts(factory->get_name()))
            continue;

        // Instantiate the test case.
        auto_ptr<ITestCase> test_case(factory->create());

        // Tell the listener that a test case is about to be executed.
        test_listener.begin_case(*this, *test_case.get());

        // Run the test case.
        TestResult test_case_result;
        run_case(
            *test_case.get(),
            test_listener,
            test_case_result);

        // Accumulate the test results.
        test_suite_result.merge(test_case_result);
        local_cumulated_result.merge(test_case_result);

        // Tell the listener that the test case execution has ended.
        test_listener.end_case(
            *this,
            *test_case.get(),
            test_suite_result,
            test_case_result,
            local_cumulated_result);
    }

    // Report a test suite failure if one or more test cases failed.
    if (test_suite_result.get_case_failure_count() > 0)
        test_suite_result.signal_suite_failure();
}

void TestSuite::run_case(
    ITestCase&      test_case,
    ITestListener&  test_listener,
    TestResult&     test_case_result) const
{
    test_case_result.signal_case_execution();

    try
    {
        // Run the test case.
        test_case.run(test_listener, test_case_result);

        // Report a test case failure if one or more assertions failed.
        if (test_case_result.get_assertion_failure_count() > 0)
            test_case_result.signal_case_failure();
    }
    catch (const ExceptionAssertionFailure&)
    {
        test_case_result.signal_case_failure();
    }
    catch (const exception& e)
    {
        TestListenerHelper::write(
            test_listener,
            *this,
            test_case,
            __FILE__,
            __LINE__,
            TestMessage::TestCaseFailure,
            "An unexpected exception was caught: %s.",
            e.what());

        test_case_result.signal_case_failure();
    }
    catch (...)
    {
        TestListenerHelper::write(
            test_listener,
            *this,
            test_case,
            __FILE__,
            __LINE__,
            TestMessage::TestCaseFailure,
            "An unexpected exception was caught (no details available).");

        test_case_result.signal_case_failure();
    }
}

}   // namespace foundation
