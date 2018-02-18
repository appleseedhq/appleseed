
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

// appleseed.foundation headers.
#include "foundation/utility/filter.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

#undef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS

using namespace foundation;

namespace
{
    struct FakeTestListener
      : public TestListenerBase
    {
        void release() override
        {
            delete this;
        }
    };

    struct FakeTestSuite
      : public TestSuite
    {
        FakeTestSuite()
          : TestSuite("FakeTestSuite") {}
    };

    struct FakeTestCase
      : public ITestCase
    {
        const char* get_name() const override
        {
            return "FakeTestCase";
        }

        void run(ITestListener& test_listener, TestResult& case_result) override {}
    };

    struct FakeTestCaseFactory
      : public ITestCaseFactory
    {
        const char* get_name() const override
        {
            return "FakeTestCase";
        }

        FakeTestCase* create() override
        {
            return new FakeTestCase();
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

    TEST_CASE(GetSuite_GivenTestSuiteRepositoryWithOneSuite_ReturnsTestSuite)
    {
        TestSuiteRepository repository;
        FakeTestSuite expected_suite;
        repository.register_suite(&expected_suite);

        const TestSuite* suite = repository.get_suite(0);

        EXPECT_EQ(&expected_suite, suite);
    }

    TEST_CASE(Run_GivenTestSuiteRepositoryWithOneEmptyTestSuite_DoesNotReportTestSuiteExecution)
    {
        FakeTestSuite suite;

        TestSuiteRepository repository;
        repository.register_suite(&suite);

        FakeTestListener listener;
        TestResult result;
        repository.run(listener, result);

        EXPECT_EQ(0, result.get_suite_execution_count());
    }

    TEST_CASE(Run_GivenTestSuiteRepositoryWithOneNonEmptyTestSuite_ReportsTestSuiteExecution)
    {
        FakeTestSuite suite;

        FakeTestCaseFactory test_case_factory;
        suite.register_case(&test_case_factory);

        TestSuiteRepository repository;
        repository.register_suite(&suite);

        FakeTestListener listener;
        TestResult result;
        repository.run(listener, result);

        EXPECT_EQ(1, result.get_suite_execution_count());
    }
}

TEST_SUITE(Foundation_Utility_Test_TestSuite)
{
    struct PassingTestCase
      : public ITestCase
    {
        size_t& m_run_count;

        explicit PassingTestCase(size_t& run_count)
          : m_run_count(run_count) {}

        const char* get_name() const override
        {
            return "PassingTestCase";
        }

        void run(ITestListener& test_listener, TestResult& case_result) override
        {
            ++m_run_count;
        }
    };

    struct PassingTestCaseFactory
      : public ITestCaseFactory
    {
        size_t& m_run_count;

        explicit PassingTestCaseFactory(size_t& run_count)
          : m_run_count(run_count) {}

        const char* get_name() const override
        {
            return "PassingTestCase";
        }

        PassingTestCase* create() override
        {
            return new PassingTestCase(m_run_count);
        }
    };

    TEST_CASE(Run_GivenTestSuiteWithOneCase_RunsCase)
    {
        size_t run_count = 0;
        PassingTestCaseFactory test_case_factory(run_count);

        FakeTestSuite test_suite;
        test_suite.register_case(&test_case_factory);

        FakeTestListener listener;
        TestResult result;
        test_suite.run(listener, result);

        EXPECT_EQ(1, run_count);
    }

    struct FailingTestCase
      : public ITestCase
    {
        const char* get_name() const override
        {
            return "FailingTestCase";
        }

        void run(ITestListener& test_listener, TestResult& case_result) override
        {
            case_result.signal_assertion_failure();
        }
    };

    struct FailingTestCaseFactory
      : public ITestCaseFactory
    {
        const char* get_name() const override
        {
            return "FailingTestCase";
        }

        FailingTestCase* create() override
        {
            return new FailingTestCase();
        }
    };

    struct TestListenerCapturingTestSuiteResults
      : public TestListenerBase
    {
        TestResult m_test_suite_result;

        void release() override
        {
            delete this;
        }

        void end_suite(
            const TestSuite&    test_suite,
            const TestResult&   test_suite_result,
            const TestResult&   cumulated_result) override
        {
            m_test_suite_result = test_suite_result;
        }
    };

    TEST_CASE(Run_GivenFailingTestCase_ReportsTestSuiteFailure)
    {
        FailingTestCaseFactory test_case_factory;

        FakeTestSuite test_suite;
        test_suite.register_case(&test_case_factory);

        TestListenerCapturingTestSuiteResults listener;
        TestResult result;
        test_suite.run(listener, result);

        EXPECT_EQ(1, listener.m_test_suite_result.get_suite_failure_count());
    }

    TEST_CASE(Run_GivenTestSuiteRejectedByFilter_DoesNotReportTestSuiteExecution)
    {
        FakeTestSuite test_suite;
        RejectAllFilter filter;
        FakeTestListener listener;
        TestResult result;

        test_suite.run(filter, listener, result);

        EXPECT_EQ(0, result.get_suite_execution_count());
    }
}

TEST_SUITE(Foundation_Utility_Test_Assertions)
{
    TEST_CASE(ExpectTrue_GivenTrue_Succeeds)
    {
        EXPECT_TRUE(true);
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectTrue_GivenFalse_Fails)
    {
        EXPECT_TRUE(false);
    }
#endif

    TEST_CASE(ExpectFalse_GivenFalse_Succeeds)
    {
        EXPECT_FALSE(false);
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectFalse_GivenTrue_Fails)
    {
        EXPECT_FALSE(true);
    }
#endif

    TEST_CASE(ExpectEq_GivenEqualValues_Succeeds)
    {
        EXPECT_EQ(42, 42);
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectEq_GivenDifferentValues_Fails)
    {
        EXPECT_EQ(42, 0);
    }
#endif

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectEq_ExpectingNonNullCStringButGivenNullCString_Fails)
    {
        const char* Expected = "bunny";
        const char* Obtained = 0;

        EXPECT_EQ(Expected, Obtained);
    }

    TEST_CASE(ExpectEq_ExpectingNullCStringButGivenNonNullCString_Fails)
    {
        const char* Expected = 0;
        const char* Obtained = "bunny";

        EXPECT_EQ(Expected, Obtained);
    }
#endif

    TEST_CASE(ExpectArrayEq_GivenEqualArrays_Succeeds)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 3 };

        EXPECT_ARRAY_EQ(Expected, Array);
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectArrayEq_GivenArraysOfSameSizeButDifferentContent_Fails)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 0 };

        EXPECT_ARRAY_EQ(Expected, Array);
    }

    TEST_CASE(ExpectArrayEq_GivenArraysOfDifferentSizes_Fails)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2 };

        EXPECT_ARRAY_EQ(Expected, Array);
    }
#endif

    TEST_CASE(ExpectSequenceEq_GivenEqualArrays_Succeeds)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 3 };

        EXPECT_SEQUENCE_EQ(3, Expected, Array);
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectSequenceEq_GivenDifferentArrays_Fails)
    {
        const int Expected[] = { 1, 2, 3 };
        const int Array[] = { 1, 2, 0 };

        EXPECT_SEQUENCE_EQ(3, Expected, Array);
    }
#endif

    TEST_CASE(ExpectNeq_GivenDifferentValues_Succeeds)
    {
        EXPECT_NEQ(42, 0);
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectNeq_GivenEqualValues_Fails)
    {
        EXPECT_NEQ(42, 42);
    }
#endif

    TEST_CASE(ExpectFeq_GivenEqualValues_Succeeds)
    {
        EXPECT_FEQ(42.0, 42.0);
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectFeq_GivenDifferentValues_Fails)
    {
        EXPECT_FEQ(42.0, 0.0);
    }
#endif

    TEST_CASE(ExpectArrayFeq_GivenEqualArrays_Succeeds)
    {
        const double Expected[] = { 1.0, 2.0, 3.0 };
        const double Array[] = { 1.0, 2.0, 3.0 };

        EXPECT_ARRAY_FEQ(Expected, Array);
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectArrayFeq_GivenArraysOfSameSizeButDifferentContent_Fails)
    {
        const double Expected[] = { 1.0, 2.0, 3.0 };
        const double Array[] = { 1.0, 2.0, 0.0 };

        EXPECT_ARRAY_FEQ(Expected, Array);
    }

    TEST_CASE(ExpectArrayFeq_GivenArraysOfDifferentSizes_Fails)
    {
        const double Expected[] = { 1.0, 2.0, 3.0 };
        const double Array[] = { 1.0, 2.0 };

        EXPECT_ARRAY_FEQ(Expected, Array);
    }
#endif

    TEST_CASE(ExpectSequenceFeq_GivenEqualArrays_Succeeds)
    {
        const double Expected[] = { 1.0, 2.0, 3.0 };
        const double Array[] = { 1.0, 2.0, 3.0 };

        EXPECT_SEQUENCE_FEQ(3, Expected, Array);
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectSequenceFeq_GivenDifferentArrays_Fails)
    {
        const double Expected[] = { 1.0, 2.0, 3.0 };
        const double Array[] = { 1.0, 2.0, 0.0 };

        EXPECT_SEQUENCE_FEQ(3, Expected, Array);
    }
#endif

    TEST_CASE(ExpectFneq_GivenDifferentValues_Succeeds)
    {
        EXPECT_FNEQ(42.0, 0.0);
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectFneq_GivenEqualValues_Fails)
    {
        EXPECT_FNEQ(42.0, 42.0);
    }
#endif

    struct Exception {};

    TEST_CASE(ExpectException_GivenCodeThatThrowsExpectedException_Succeeds)
    {
        EXPECT_EXCEPTION(Exception,
        {
            throw Exception();
        });
    }

#ifdef FOUNDATION_ENABLE_FALSE_ASSERTIONS_CHECKS
    TEST_CASE(ExpectException_GivenCodeThatDoesNotThrowExpectedException_Fails)
    {
        EXPECT_EXCEPTION(Exception,
        {
        });
    }

    TEST_CASE(Test_GivenCodeThatThrowsUnexpectedException_Fails)
    {
        throw Exception();
    }
#endif
}
