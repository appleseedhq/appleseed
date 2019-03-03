
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

// appleseed.foundation headers.
#include "foundation/utility/test/itestcase.h"
#include "foundation/utility/test/itestcasefactory.h"
#include "foundation/utility/test/testsuite.h"
#include "foundation/utility/test/testsuiterepository.h"

// Forward declarations.
namespace foundation    { class ITestListener; }
namespace foundation    { class TestResult; }

namespace foundation
{

//
// Define a test suite.
//

#define TEST_SUITE(Name)                                                                    \
    namespace TestSuite##Name                                                               \
    {                                                                                       \
        struct TestSuite##Name                                                              \
          : public foundation::TestSuite                                                    \
        {                                                                                   \
            TestSuite##Name()                                                               \
              : foundation::TestSuite(#Name)                                                \
            {                                                                               \
            }                                                                               \
        };                                                                                  \
                                                                                            \
        foundation::TestSuite& current_test_suite__()                                       \
        {                                                                                   \
            static TestSuite##Name suite;                                                   \
            return suite;                                                                   \
        }                                                                                   \
                                                                                            \
        struct RegisterTestSuite##Name                                                      \
        {                                                                                   \
            RegisterTestSuite##Name()                                                       \
            {                                                                               \
                using namespace foundation;                                                 \
                TestSuite& suite = current_test_suite__();                                  \
                TestSuiteRepository::instance().register_suite(&suite);                     \
            }                                                                               \
        };                                                                                  \
                                                                                            \
        static RegisterTestSuite##Name RegisterTestSuite##Name##_instance__;                \
    }                                                                                       \
                                                                                            \
    namespace TestSuite##Name


//
// Define a test case without fixture.
//

#define TEST_CASE(Name)                                                                     \
    struct TestCase##Name                                                                   \
      : public foundation::ITestCase                                                        \
    {                                                                                       \
        const char* get_name() const override                                               \
        {                                                                                   \
            return #Name;                                                                   \
        }                                                                                   \
                                                                                            \
        void run(                                                                           \
            foundation::ITestListener&  test_listener,                                      \
            foundation::TestResult&     case_result) override;                              \
    };                                                                                      \
                                                                                            \
    struct TestCase##Name##Factory                                                          \
      : public foundation::ITestCaseFactory                                                 \
    {                                                                                       \
        const char* get_name() const override                                               \
        {                                                                                   \
            return #Name;                                                                   \
        }                                                                                   \
                                                                                            \
        foundation::ITestCase* create() override                                            \
        {                                                                                   \
            return new TestCase##Name();                                                    \
        }                                                                                   \
    };                                                                                      \
                                                                                            \
    struct RegisterTestCase##Name                                                           \
    {                                                                                       \
        RegisterTestCase##Name()                                                            \
        {                                                                                   \
            using namespace foundation;                                                     \
            static TestCase##Name##Factory factory;                                         \
            current_test_suite__().register_case(&factory);                                 \
        }                                                                                   \
    };                                                                                      \
                                                                                            \
    static RegisterTestCase##Name RegisterTestCase##Name##_instance__;                      \
                                                                                            \
    void TestCase##Name::run(                                                               \
        foundation::ITestListener&  test_listener,                                          \
        foundation::TestResult&     case_result)


//
// Define a test case with fixture.
//

#define TEST_CASE_F(Name, FixtureName)                                                      \
    struct TestCase##Name                                                                   \
      : public foundation::ITestCase                                                        \
      , public FixtureName                                                                  \
    {                                                                                       \
        const char* get_name() const override                                               \
        {                                                                                   \
            return #Name;                                                                   \
        }                                                                                   \
                                                                                            \
        void run(                                                                           \
            foundation::ITestListener&  test_listener,                                      \
            foundation::TestResult&     case_result) override;                              \
    };                                                                                      \
                                                                                            \
    struct TestCase##Name##Factory                                                          \
      : public foundation::ITestCaseFactory                                                 \
    {                                                                                       \
        const char* get_name() const override                                               \
        {                                                                                   \
            return #Name;                                                                   \
        }                                                                                   \
                                                                                            \
        foundation::ITestCase* create() override                                            \
        {                                                                                   \
            return new TestCase##Name();                                                    \
        }                                                                                   \
    };                                                                                      \
                                                                                            \
    struct RegisterTestCase##Name                                                           \
    {                                                                                       \
        RegisterTestCase##Name()                                                            \
        {                                                                                   \
            using namespace foundation;                                                     \
            static TestCase##Name##Factory factory;                                         \
            current_test_suite__().register_case(&factory);                                 \
        }                                                                                   \
    };                                                                                      \
                                                                                            \
    static RegisterTestCase##Name RegisterTestCase##Name##_instance__;                      \
                                                                                            \
    void TestCase##Name::run(                                                               \
        foundation::ITestListener&  test_listener,                                          \
        foundation::TestResult&     case_result)


//
// Forward-declare a test case.
//

#define DECLARE_TEST_CASE(SuiteName, CaseName)                                              \
    namespace TestSuite##SuiteName { struct TestCase##CaseName; }


//
// Declare that a test case has access to the internals of a class.
//

#define GRANT_ACCESS_TO_TEST_CASE(SuiteName, CaseName)                                      \
    friend struct TestSuite##SuiteName::TestCase##CaseName

}   // namespace foundation
