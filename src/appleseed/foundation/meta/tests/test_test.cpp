
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

// appleseed.foundation headers.
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

#undef FOUNDATION_ENABLE_ASSERTIONS_UNIT_TESTS

using namespace foundation;

namespace
{
    struct FakeTestListener
      : public TestListenerBase
    {
        virtual void release()
        {
            delete this;
        }
    };

    struct FakeTestSuite
      : public TestSuite
    {
        FakeTestSuite()
          : TestSuite("FakeTestSuite")
        {
        }
    };
}

FOUNDATION_TEST_SUITE(Foundation_Utility_Test_TestResult)
{
    FOUNDATION_TEST_CASE(TestResultInitializesTestSuiteMetricsCorrectly)
    {
        TestResult result;

        FOUNDATION_EXPECT_EQ(0, result.get_suite_execution_count());
        FOUNDATION_EXPECT_EQ(0, result.get_suite_failure_count());
        FOUNDATION_EXPECT_EQ(0, result.get_case_execution_count());
        FOUNDATION_EXPECT_EQ(0, result.get_case_failure_count());
        FOUNDATION_EXPECT_EQ(0, result.get_assertion_execution_count());
        FOUNDATION_EXPECT_EQ(0, result.get_assertion_failure_count());
    }

    FOUNDATION_TEST_CASE(TestResultTracksTestSuiteMetricsCorrectly)
    {
        TestResult result;

        result.signal_suite_execution();
        result.signal_suite_execution();
        result.signal_suite_failure();

        FOUNDATION_EXPECT_EQ(2, result.get_suite_execution_count());
        FOUNDATION_EXPECT_EQ(1, result.get_suite_failure_count());
    }

    FOUNDATION_TEST_CASE(TestResultTracksTestCaseMetricsCorrectly)
    {
        TestResult result;

        result.signal_case_execution();
        result.signal_case_execution();
        result.signal_case_failure();

        FOUNDATION_EXPECT_EQ(2, result.get_case_execution_count());
        FOUNDATION_EXPECT_EQ(1, result.get_case_failure_count());
    }

    FOUNDATION_TEST_CASE(TestResultTracksAssertionMetricsCorrectly)
    {
        TestResult result;

        result.signal_assertion_execution();
        result.signal_assertion_execution();
        result.signal_assertion_failure();

        FOUNDATION_EXPECT_EQ(2, result.get_assertion_execution_count());
        FOUNDATION_EXPECT_EQ(1, result.get_assertion_failure_count());
    }

    FOUNDATION_TEST_CASE(TestResultsGetMergedCorrectly)
    {
        TestResult result1;
        result1.signal_suite_execution();
        result1.signal_suite_execution();
        result1.signal_suite_failure();
        result1.signal_case_execution();
        result1.signal_case_execution();
        result1.signal_case_failure();

        TestResult result2;
        result2.signal_assertion_execution();
        result2.signal_assertion_execution();
        result2.signal_assertion_failure();

        result2.merge(result1);

        FOUNDATION_EXPECT_EQ(2, result2.get_suite_execution_count());
        FOUNDATION_EXPECT_EQ(1, result2.get_suite_failure_count());
        FOUNDATION_EXPECT_EQ(2, result2.get_case_execution_count());
        FOUNDATION_EXPECT_EQ(1, result2.get_case_failure_count());
        FOUNDATION_EXPECT_EQ(2, result2.get_assertion_execution_count());
        FOUNDATION_EXPECT_EQ(1, result2.get_assertion_failure_count());
    }
}

FOUNDATION_TEST_SUITE(Foundation_Utility_Test_TestSuiteRepository)
{
    FOUNDATION_TEST_CASE(GetSuiteCount_GivenTestSuiteRepositoryWithOneSuite_ReturnsOne)
    {
        TestSuiteRepository repository;
        FakeTestSuite suite;
        repository.register_suite(&suite);

        const size_t suite_count = repository.get_suite_count();

        FOUNDATION_EXPECT_EQ(1, suite_count);
    }

    FOUNDATION_TEST_CASE(GetSuite_GivenTestSuiteRepositoryWithOneSuite_ReturnsSuite)
    {
        TestSuiteRepository repository;
        FakeTestSuite expected_suite;
        repository.register_suite(&expected_suite);

        const TestSuite* suite = repository.get_suite(0);

        FOUNDATION_EXPECT_EQ(&expected_suite, suite);
    }

    FOUNDATION_TEST_CASE(Run_GivenTestSuiteRepositoryWithOneTestSuite_RunsTestSuite)
    {
        TestSuiteRepository repository;
        FakeTestSuite suite;
        repository.register_suite(&suite);

        FakeTestListener listener;
        TestResult result;
        repository.run(listener, result);

        FOUNDATION_EXPECT_EQ(1, result.get_suite_execution_count());
        FOUNDATION_EXPECT_EQ(0, result.get_suite_failure_count());
    }
}

FOUNDATION_TEST_SUITE(Foundation_Utility_Test_TestSuite)
{
    struct FakeTestCase
      : public ITestCase
    {
        size_t& m_run_count;

        explicit FakeTestCase(size_t& run_count)
          : m_run_count(run_count)
        {
        }

        virtual const char* get_name() const
        {
            return "FakeTestCase";
        }

        virtual void run(
            ITestListener&  test_listener,
            TestResult&     case_result)
        {
            ++m_run_count;
        }
    };

    struct FakeTestCaseFactory
      : public ITestCaseFactory
    {
        size_t& m_run_count;

        explicit FakeTestCaseFactory(size_t& run_count)
          : m_run_count(run_count)
        {
        }

        virtual const char* get_name() const
        {
            return "FakeTestCase";
        }

        virtual FakeTestCase* create()
        {
            return new FakeTestCase(m_run_count);
        }
    };

    FOUNDATION_TEST_CASE(Run_GivenTestSuiteWithOneCase_RunsCase)
    {
        size_t run_count = 0;
        FakeTestCaseFactory test_case_factory(run_count);

        FakeTestSuite test_suite;
        test_suite.register_case(&test_case_factory);

        FakeTestListener listener;
        TestResult result;
        test_suite.run(listener, result);

        FOUNDATION_EXPECT_EQ(1, run_count);
    }
}

#ifdef FOUNDATION_ENABLE_ASSERTIONS_UNIT_TESTS

FOUNDATION_TEST_SUITE(Foundation_Utility_Test_Assertions)
{
    FOUNDATION_TEST_CASE(FoundationExpectTrueSucceeds)
    {
        FOUNDATION_EXPECT_TRUE(true);
    }

    FOUNDATION_TEST_CASE(FoundationExpectTrueFails)
    {
        FOUNDATION_EXPECT_TRUE(false);
    }

    FOUNDATION_TEST_CASE(FoundationExpectFalseSucceeds)
    {
        FOUNDATION_EXPECT_FALSE(false);
    }

    FOUNDATION_TEST_CASE(FoundationExpectFalseFails)
    {
        FOUNDATION_EXPECT_FALSE(true);
    }

    FOUNDATION_TEST_CASE(FoundationExpectEqSucceeds)
    {
        FOUNDATION_EXPECT_EQ(42, 42);
    }

    FOUNDATION_TEST_CASE(FoundationExpectEqFails)
    {
        FOUNDATION_EXPECT_EQ(42, 0);
    }

    FOUNDATION_TEST_CASE(FoundationExpectEq_ExpectingNonNullCStringButGivenNullCString_Fails)
    {
        const char* Expected = "bunny";
        const char* Obtained = 0;

        FOUNDATION_EXPECT_EQ(Expected, Obtained);
    }

    FOUNDATION_TEST_CASE(FoundationExpectEq_ExpectingNullCStringButGivenNonNullCString_Fails)
    {
        const char* Expected = 0;
        const char* Obtained = "bunny";

        FOUNDATION_EXPECT_EQ(Expected, Obtained);
    }

    FOUNDATION_TEST_CASE(FoundationExpectArrayEqSucceeds)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 3 };

        FOUNDATION_EXPECT_ARRAY_EQ(Expected, Array);
    }

    FOUNDATION_TEST_CASE(FoundationExpectArrayEq1Fails)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 0 };

        FOUNDATION_EXPECT_ARRAY_EQ(Expected, Array);
    }

    FOUNDATION_TEST_CASE(FoundationExpectArrayEq2Fails)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2 };

        FOUNDATION_EXPECT_ARRAY_EQ(Expected, Array);
    }

    FOUNDATION_TEST_CASE(FoundationExpectSequenceEqSucceeds)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 3 };

        FOUNDATION_EXPECT_SEQUENCE_EQ(3, Expected, Array);
    }

    FOUNDATION_TEST_CASE(FoundationExpectSequenceEqFails)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 0 };

        FOUNDATION_EXPECT_SEQUENCE_EQ(3, Expected, Array);
    }

    FOUNDATION_TEST_CASE(FoundationExpectNeqSucceeds)
    {
        FOUNDATION_EXPECT_NEQ(42, 0);
    }

    FOUNDATION_TEST_CASE(FoundationExpectNeqFails)
    {
        FOUNDATION_EXPECT_NEQ(42, 42);
    }

    FOUNDATION_TEST_CASE(FoundationExpectFeqSucceeds)
    {
        FOUNDATION_EXPECT_FEQ(42.0, 42.0);
    }

    FOUNDATION_TEST_CASE(FoundationExpectFeqFails)
    {
        FOUNDATION_EXPECT_FEQ(42.0, 0.0);
    }

    FOUNDATION_TEST_CASE(FoundationExpectFneqSucceeds)
    {
        FOUNDATION_EXPECT_FNEQ(42.0, 0.0);
    }

    FOUNDATION_TEST_CASE(FoundationExpectFneqFails)
    {
        FOUNDATION_EXPECT_FNEQ(42.0, 42.0);
    }

    struct Exception {};

    FOUNDATION_TEST_CASE(FoundationExpectExceptionSucceeds)
    {
        FOUNDATION_EXPECT_EXCEPTION(
            Exception,
            {
                throw Exception();
            }
        );
    }

    FOUNDATION_TEST_CASE(FoundationExpectExceptionFails)
    {
        FOUNDATION_EXPECT_EXCEPTION(
            Exception,
            {
            }
        );
    }

    FOUNDATION_TEST_CASE(TestThrowsUnexpectedException)
    {
        throw Exception();
    }
}

#endif  // FOUNDATION_ENABLE_ASSERTIONS_UNIT_TESTS
