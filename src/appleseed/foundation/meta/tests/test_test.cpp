
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

TEST_SUITE(Foundation_Utility_Test_TestResult)
{
    TEST_CASE(TestResultInitializesTestSuiteMetricsCorrectly)
    {
        TestResult result;

        EXPECT_EQ(0, result.get_suite_execution_count());
        EXPECT_EQ(0, result.get_suite_failure_count());
        EXPECT_EQ(0, result.get_case_execution_count());
        EXPECT_EQ(0, result.get_case_failure_count());
        EXPECT_EQ(0, result.get_assertion_execution_count());
        EXPECT_EQ(0, result.get_assertion_failure_count());
    }

    TEST_CASE(TestResultTracksTestSuiteMetricsCorrectly)
    {
        TestResult result;

        result.signal_suite_execution();
        result.signal_suite_execution();
        result.signal_suite_failure();

        EXPECT_EQ(2, result.get_suite_execution_count());
        EXPECT_EQ(1, result.get_suite_failure_count());
    }

    TEST_CASE(TestResultTracksTestCaseMetricsCorrectly)
    {
        TestResult result;

        result.signal_case_execution();
        result.signal_case_execution();
        result.signal_case_failure();

        EXPECT_EQ(2, result.get_case_execution_count());
        EXPECT_EQ(1, result.get_case_failure_count());
    }

    TEST_CASE(TestResultTracksAssertionMetricsCorrectly)
    {
        TestResult result;

        result.signal_assertion_execution();
        result.signal_assertion_execution();
        result.signal_assertion_failure();

        EXPECT_EQ(2, result.get_assertion_execution_count());
        EXPECT_EQ(1, result.get_assertion_failure_count());
    }

    TEST_CASE(TestResultsGetMergedCorrectly)
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

        EXPECT_EQ(2, result2.get_suite_execution_count());
        EXPECT_EQ(1, result2.get_suite_failure_count());
        EXPECT_EQ(2, result2.get_case_execution_count());
        EXPECT_EQ(1, result2.get_case_failure_count());
        EXPECT_EQ(2, result2.get_assertion_execution_count());
        EXPECT_EQ(1, result2.get_assertion_failure_count());
    }
}

TEST_SUITE(Foundation_Utility_Test_TestSuiteRepository)
{
    TEST_CASE(GetSuiteCount_GivenTestSuiteRepositoryWithOneSuite_ReturnsOne)
    {
        TestSuiteRepository repository;
        FakeTestSuite suite;
        repository.register_suite(&suite);

        const size_t suite_count = repository.get_suite_count();

        EXPECT_EQ(1, suite_count);
    }

    TEST_CASE(GetSuite_GivenTestSuiteRepositoryWithOneSuite_ReturnsSuite)
    {
        TestSuiteRepository repository;
        FakeTestSuite expected_suite;
        repository.register_suite(&expected_suite);

        const TestSuite* suite = repository.get_suite(0);

        EXPECT_EQ(&expected_suite, suite);
    }

    TEST_CASE(Run_GivenTestSuiteRepositoryWithOneTestSuite_RunsTestSuite)
    {
        TestSuiteRepository repository;
        FakeTestSuite suite;
        repository.register_suite(&suite);

        FakeTestListener listener;
        TestResult result;
        repository.run(listener, result);

        EXPECT_EQ(1, result.get_suite_execution_count());
        EXPECT_EQ(0, result.get_suite_failure_count());
    }
}

TEST_SUITE(Foundation_Utility_Test_TestSuite)
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

    TEST_CASE(Run_GivenTestSuiteWithOneCase_RunsCase)
    {
        size_t run_count = 0;
        FakeTestCaseFactory test_case_factory(run_count);

        FakeTestSuite test_suite;
        test_suite.register_case(&test_case_factory);

        FakeTestListener listener;
        TestResult result;
        test_suite.run(listener, result);

        EXPECT_EQ(1, run_count);
    }
}

#ifdef FOUNDATION_ENABLE_ASSERTIONS_UNIT_TESTS

TEST_SUITE(Foundation_Utility_Test_Assertions)
{
    TEST_CASE(FoundationExpectTrueSucceeds)
    {
        EXPECT_TRUE(true);
    }

    TEST_CASE(FoundationExpectTrueFails)
    {
        EXPECT_TRUE(false);
    }

    TEST_CASE(FoundationExpectFalseSucceeds)
    {
        EXPECT_FALSE(false);
    }

    TEST_CASE(FoundationExpectFalseFails)
    {
        EXPECT_FALSE(true);
    }

    TEST_CASE(FoundationExpectEqSucceeds)
    {
        EXPECT_EQ(42, 42);
    }

    TEST_CASE(FoundationExpectEqFails)
    {
        EXPECT_EQ(42, 0);
    }

    TEST_CASE(FoundationExpectEq_ExpectingNonNullCStringButGivenNullCString_Fails)
    {
        const char* Expected = "bunny";
        const char* Obtained = 0;

        EXPECT_EQ(Expected, Obtained);
    }

    TEST_CASE(FoundationExpectEq_ExpectingNullCStringButGivenNonNullCString_Fails)
    {
        const char* Expected = 0;
        const char* Obtained = "bunny";

        EXPECT_EQ(Expected, Obtained);
    }

    TEST_CASE(FoundationExpectArrayEqSucceeds)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 3 };

        EXPECT_ARRAY_EQ(Expected, Array);
    }

    TEST_CASE(FoundationExpectArrayEq1Fails)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 0 };

        EXPECT_ARRAY_EQ(Expected, Array);
    }

    TEST_CASE(FoundationExpectArrayEq2Fails)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2 };

        EXPECT_ARRAY_EQ(Expected, Array);
    }

    TEST_CASE(FoundationExpectSequenceEqSucceeds)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 3 };

        EXPECT_SEQUENCE_EQ(3, Expected, Array);
    }

    TEST_CASE(FoundationExpectSequenceEqFails)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 0 };

        EXPECT_SEQUENCE_EQ(3, Expected, Array);
    }

    TEST_CASE(FoundationExpectNeqSucceeds)
    {
        EXPECT_NEQ(42, 0);
    }

    TEST_CASE(FoundationExpectNeqFails)
    {
        EXPECT_NEQ(42, 42);
    }

    TEST_CASE(FoundationExpectFeqSucceeds)
    {
        EXPECT_FEQ(42.0, 42.0);
    }

    TEST_CASE(FoundationExpectFeqFails)
    {
        EXPECT_FEQ(42.0, 0.0);
    }

    TEST_CASE(FoundationExpectFneqSucceeds)
    {
        EXPECT_FNEQ(42.0, 0.0);
    }

    TEST_CASE(FoundationExpectFneqFails)
    {
        EXPECT_FNEQ(42.0, 42.0);
    }

    struct Exception {};

    TEST_CASE(FoundationExpectExceptionSucceeds)
    {
        EXPECT_EXCEPTION(Exception,
        {
            throw Exception();
        });
    }

    TEST_CASE(FoundationExpectExceptionFails)
    {
        EXPECT_EXCEPTION(Exception,
        {
        });
    }

    TEST_CASE(TestThrowsUnexpectedException)
    {
        throw Exception();
    }
}

#endif  // FOUNDATION_ENABLE_ASSERTIONS_UNIT_TESTS
