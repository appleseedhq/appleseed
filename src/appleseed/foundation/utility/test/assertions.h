
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
#include "foundation/math/scalar.h"
#include "foundation/string/string.h"
#include "foundation/utility/countof.h"
#include "foundation/utility/test/exceptionassertionfailure.h"
#include "foundation/utility/test/testlistenerhelper.h"
#include "foundation/utility/test/testmessage.h"
#include "foundation/utility/test/testresult.h"

// Standard headers.
#include <string>

namespace foundation
{

//
// Emit an error message signaling that an assertion failed.
//

#define FOUNDATION_ASSERTION_FAILURE(op, expected, expr, stop)                          \
    do {                                                                                \
        case_result.signal_assertion_failure();                                         \
                                                                                        \
        foundation::TestListenerHelper::write(                                          \
            test_listener,                                                              \
            current_test_suite__(),                                                     \
            get_name(),                                                                 \
            __FILE__,                                                                   \
            __LINE__,                                                                   \
            foundation::TestMessage::AssertionFailure,                                  \
            "expected: %s %s %s\n"                                                      \
            "received: %s == %s",                                                       \
            #expr, op, foundation::to_string(expected).c_str(),                         \
            #expr, foundation::to_string(expr).c_str());                                \
                                                                                        \
        if (stop)                                                                       \
            throw foundation::ExceptionAssertionFailure();                              \
    } while (0)


//
// Expect a true boolean expression.
//

#define FOUNDATION_EXPECT_TRUE_IMPL(expr, stop)                                         \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
        if (!(expr))                                                                    \
            FOUNDATION_ASSERTION_FAILURE("==", true, expr, stop);                       \
    } while (0)

#define EXPECT_TRUE(expr)                                                               \
    FOUNDATION_EXPECT_TRUE_IMPL(expr, false)

#define ASSERT_TRUE(expr)                                                               \
    FOUNDATION_EXPECT_TRUE_IMPL(expr, true)


//
// Expect a false boolean expression.
//

#define FOUNDATION_EXPECT_FALSE_IMPL(expr, stop)                                        \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
        if (expr)                                                                       \
            FOUNDATION_ASSERTION_FAILURE("==", false, expr, stop);                      \
    } while (0)

#define EXPECT_FALSE(expr)                                                              \
    FOUNDATION_EXPECT_FALSE_IMPL(expr, false)

#define ASSERT_FALSE(expr)                                                              \
    FOUNDATION_EXPECT_FALSE_IMPL(expr, true)


//
// Expect exact equality of single objects.
//

#define FOUNDATION_EXPECT_EQ_IMPL(expected, expr, stop)                                 \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
        if (!((expr) == (expected)))                                                    \
            FOUNDATION_ASSERTION_FAILURE("==", expected, expr, stop);                   \
    } while (0)

#define EXPECT_EQ(expected, expr)                                                       \
    FOUNDATION_EXPECT_EQ_IMPL(expected, expr, false)

#define ASSERT_EQ(expected, expr)                                                       \
    FOUNDATION_EXPECT_EQ_IMPL(expected, expr, true)


//
// Expect exact equality of C arrays of objects.
//

#define FOUNDATION_EXPECT_ARRAY_EQ_IMPL(expected, expr, stop)                           \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
                                                                                        \
        const size_t expected_count__ = countof(expected);                              \
        const size_t expr_count__ = countof(expr);                                      \
                                                                                        \
        bool are_equal__ = expected_count__ == expr_count__;                            \
        if (are_equal__)                                                                \
        {                                                                               \
            for (size_t i__ = 0; i__ < expected_count__; ++i__)                         \
            {                                                                           \
                if (!((expr)[i__] == (expected)[i__]))                                  \
                {                                                                       \
                    are_equal__ = false;                                                \
                    break;                                                              \
                }                                                                       \
            }                                                                           \
        }                                                                               \
                                                                                        \
        if (!are_equal__)                                                               \
        {                                                                               \
            case_result.signal_assertion_failure();                                     \
                                                                                        \
            foundation::TestListenerHelper::write(                                      \
                test_listener,                                                          \
                current_test_suite__(),                                                 \
                get_name(),                                                             \
                __FILE__,                                                               \
                __LINE__,                                                               \
                foundation::TestMessage::AssertionFailure,                              \
                "expected: %s == %s\n"                                                  \
                "received: %s == %s",                                                   \
                #expr, foundation::to_string((expected), countof(expected)).c_str(),    \
                #expr, foundation::to_string((expr), countof(expr)).c_str());           \
                                                                                        \
            if (stop)                                                                   \
                throw foundation::ExceptionAssertionFailure();                          \
        }                                                                               \
    } while (0)

#define EXPECT_ARRAY_EQ(expected, expr)                                                 \
    FOUNDATION_EXPECT_ARRAY_EQ_IMPL(expected, expr, false)

#define ASSERT_ARRAY_EQ(expected, expr)                                                 \
    FOUNDATION_EXPECT_ARRAY_EQ_IMPL(expected, expr, true)


//
// Expect exact equality of sequences of objects.
//

#define FOUNDATION_EXPECT_SEQUENCE_EQ_IMPL(count, expected, expr, stop)                 \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
                                                                                        \
        bool are_equal__ = true;                                                        \
        for (size_t i__ = 0; i__ < count; ++i__)                                        \
        {                                                                               \
            if (!((expr)[i__] == (expected)[i__]))                                      \
            {                                                                           \
                are_equal__ = false;                                                    \
                break;                                                                  \
            }                                                                           \
        }                                                                               \
                                                                                        \
        if (!are_equal__)                                                               \
        {                                                                               \
            case_result.signal_assertion_failure();                                     \
                                                                                        \
            foundation::TestListenerHelper::write(                                      \
                test_listener,                                                          \
                current_test_suite__(),                                                 \
                get_name(),                                                             \
                __FILE__,                                                               \
                __LINE__,                                                               \
                foundation::TestMessage::AssertionFailure,                              \
                "expected: %s == %s\n"                                                  \
                "received: %s == %s",                                                   \
                #expr, foundation::to_string((expected), count).c_str(),                \
                #expr, foundation::to_string((expr), count).c_str());                   \
                                                                                        \
            if (stop)                                                                   \
                throw foundation::ExceptionAssertionFailure();                          \
        }                                                                               \
    } while (0)

#define EXPECT_SEQUENCE_EQ(count, expected, expr)                                       \
    FOUNDATION_EXPECT_SEQUENCE_EQ_IMPL(count, expected, expr, false)

#define ASSERT_SEQUENCE_EQ(count, expected, expr)                                       \
    FOUNDATION_EXPECT_SEQUENCE_EQ_IMPL(count, expected, expr, true)


//
// Expect exact inequality.
//

#define FOUNDATION_EXPECT_NEQ_IMPL(expected, expr, stop)                                \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
        if (!((expr) != (expected)))                                                    \
            FOUNDATION_ASSERTION_FAILURE("!=", expected, expr, stop);                   \
    } while (0)

#define EXPECT_NEQ(expected, expr)                                                      \
    FOUNDATION_EXPECT_NEQ_IMPL(expected, expr, false)

#define ASSERT_NEQ(expected, expr)                                                      \
    FOUNDATION_EXPECT_NEQ_IMPL(expected, expr, true)


//
// Expect less-than inequality.
//

#define FOUNDATION_EXPECT_LT_IMPL(expected, expr, stop)                                 \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
        if (!((expr) < (expected)))                                                     \
            FOUNDATION_ASSERTION_FAILURE("<", expected, expr, stop);                    \
    } while (0)

#define EXPECT_LT(expected, expr)                                                       \
    FOUNDATION_EXPECT_LT_IMPL(expected, expr, false)

#define ASSERT_LT(expected, expr)                                                       \
    FOUNDATION_EXPECT_LT_IMPL(expected, expr, true)


//
// Expect greater-than inequality.
//

#define FOUNDATION_EXPECT_GT_IMPL(expected, expr, stop)                                 \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
        if (!((expr) > (expected)))                                                     \
            FOUNDATION_ASSERTION_FAILURE(">", expected, expr, stop);                    \
    } while (0)

#define EXPECT_GT(expected, expr)                                                       \
    FOUNDATION_EXPECT_GT_IMPL(expected, expr, false)

#define ASSERT_GT(expected, expr)                                                       \
    FOUNDATION_EXPECT_GT_IMPL(expected, expr, true)


//
// Expect approximate floating-point equality.
//

#define FOUNDATION_EXPECT_FEQ_IMPL(expected, expr, stop)                                \
    do {                                                                                \
        using namespace foundation;                                                     \
        case_result.signal_assertion_execution();                                       \
        if (!feq((expr), (expected)))                                                   \
            FOUNDATION_ASSERTION_FAILURE("==", expected, expr, stop);                   \
    } while (0)

#define EXPECT_FEQ(expected, expr)                                                      \
    FOUNDATION_EXPECT_FEQ_IMPL(expected, expr, false)

#define ASSERT_FEQ(expected, expr)                                                      \
    FOUNDATION_EXPECT_FEQ_IMPL(expected, expr, true)


//
// Expect approximate floating-point equality of C arrays of objects.
//

#define FOUNDATION_EXPECT_ARRAY_FEQ_IMPL(expected, expr, stop)                          \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
                                                                                        \
        const size_t expected_count__ = countof(expected);                              \
        const size_t expr_count__ = countof(expr);                                      \
                                                                                        \
        bool are_equal__ = expected_count__ == expr_count__;                            \
        if (are_equal__)                                                                \
        {                                                                               \
            for (size_t i__ = 0; i__ < expected_count__; ++i__)                         \
            {                                                                           \
                if (!feq((expr)[i__], (expected)[i__]))                                 \
                {                                                                       \
                    are_equal__ = false;                                                \
                    break;                                                              \
                }                                                                       \
            }                                                                           \
        }                                                                               \
                                                                                        \
        if (!are_equal__)                                                               \
        {                                                                               \
            case_result.signal_assertion_failure();                                     \
                                                                                        \
            foundation::TestListenerHelper::write(                                      \
                test_listener,                                                          \
                current_test_suite__(),                                                 \
                get_name(),                                                             \
                __FILE__,                                                               \
                __LINE__,                                                               \
                foundation::TestMessage::AssertionFailure,                              \
                "expected: %s == %s\n"                                                  \
                "received: %s == %s",                                                   \
                #expr, foundation::to_string((expected), countof(expected)).c_str(),    \
                #expr, foundation::to_string((expr), countof(expr)).c_str());           \
                                                                                        \
            if (stop)                                                                   \
                throw foundation::ExceptionAssertionFailure();                          \
        }                                                                               \
    } while (0)

#define EXPECT_ARRAY_FEQ(expected, expr)                                                \
    FOUNDATION_EXPECT_ARRAY_FEQ_IMPL(expected, expr, false)

#define ASSERT_ARRAY_FEQ(expected, expr)                                                \
    FOUNDATION_EXPECT_ARRAY_FEQ_IMPL(expected, expr, true)


//
// Expect approximate floating-point equality of sequences of objects.
//

#define FOUNDATION_EXPECT_SEQUENCE_FEQ_IMPL(count, expected, expr, stop)                \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
                                                                                        \
        bool are_equal__ = true;                                                        \
        for (size_t i__ = 0; i__ < count; ++i__)                                        \
        {                                                                               \
            if (!feq((expr)[i__], (expected)[i__]))                                     \
            {                                                                           \
                are_equal__ = false;                                                    \
                break;                                                                  \
            }                                                                           \
        }                                                                               \
                                                                                        \
        if (!are_equal__)                                                               \
        {                                                                               \
            case_result.signal_assertion_failure();                                     \
                                                                                        \
            foundation::TestListenerHelper::write(                                      \
                test_listener,                                                          \
                current_test_suite__(),                                                 \
                get_name(),                                                             \
                __FILE__,                                                               \
                __LINE__,                                                               \
                foundation::TestMessage::AssertionFailure,                              \
                "expected: %s == %s\n"                                                  \
                "received: %s == %s",                                                   \
                #expr, foundation::to_string((expected), count).c_str(),                \
                #expr, foundation::to_string((expr), count).c_str());                   \
                                                                                        \
            if (stop)                                                                   \
                throw foundation::ExceptionAssertionFailure();                          \
        }                                                                               \
    } while (0)

#define EXPECT_SEQUENCE_FEQ(count, expected, expr)                                      \
    FOUNDATION_EXPECT_SEQUENCE_FEQ_IMPL(count, expected, expr, false)

#define ASSERT_SEQUENCE_FEQ(count, expected, expr)                                      \
    FOUNDATION_EXPECT_SEQUENCE_FEQ_IMPL(count, expected, expr, true)


//
// Expect approximate floating-point equality with custom epsilon value.
//

#define FOUNDATION_EXPECT_FEQ_EPS_IMPL(expected, expr, eps, stop)                       \
    do {                                                                                \
        using namespace foundation;                                                     \
        case_result.signal_assertion_execution();                                       \
        if (!feq((expr), (expected), (eps)))                                            \
            FOUNDATION_ASSERTION_FAILURE("==", expected, expr, stop);                   \
    } while (0)

#define EXPECT_FEQ_EPS(expected, expr, eps)                                             \
    FOUNDATION_EXPECT_FEQ_EPS_IMPL(expected, expr, eps, false)

#define ASSERT_FEQ_EPS(expected, expr, eps)                                             \
    FOUNDATION_EXPECT_FEQ_EPS_IMPL(expected, expr, eps, true)


//
// Expect approximate floating-point equality of C arrays of objects with custom epsilon value.
//

#define FOUNDATION_EXPECT_ARRAY_FEQ_EPS_IMPL(expected, expr, eps, stop)                 \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
                                                                                        \
        const size_t expected_count__ = countof(expected);                              \
        const size_t expr_count__ = countof(expr);                                      \
                                                                                        \
        bool are_equal__ = expected_count__ == expr_count__;                            \
        if (are_equal__)                                                                \
        {                                                                               \
            for (size_t i__ = 0; i__ < expected_count__; ++i__)                         \
            {                                                                           \
                if (!feq((expr)[i__], (expected)[i__], (eps)))                          \
                {                                                                       \
                    are_equal__ = false;                                                \
                    break;                                                              \
                }                                                                       \
            }                                                                           \
        }                                                                               \
                                                                                        \
        if (!are_equal__)                                                               \
        {                                                                               \
            case_result.signal_assertion_failure();                                     \
                                                                                        \
            foundation::TestListenerHelper::write(                                      \
                test_listener,                                                          \
                current_test_suite__(),                                                 \
                get_name(),                                                             \
                __FILE__,                                                               \
                __LINE__,                                                               \
                foundation::TestMessage::AssertionFailure,                              \
                "expected: %s == %s\n"                                                  \
                "received: %s == %s",                                                   \
                #expr, foundation::to_string((expected), countof(expected)).c_str(),    \
                #expr, foundation::to_string((expr), countof(expr)).c_str());           \
                                                                                        \
            if (stop)                                                                   \
                throw foundation::ExceptionAssertionFailure();                          \
        }                                                                               \
    } while (0)

#define EXPECT_ARRAY_FEQ_EPS(expected, expr, eps)                                       \
    FOUNDATION_EXPECT_ARRAY_FEQ_EPS_IMPL(expected, expr, eps, false)

#define ASSERT_ARRAY_FEQ_EPS(expected, expr, eps)                                       \
    FOUNDATION_EXPECT_ARRAY_FEQ_EPS_IMPL(expected, expr, eps, true)


//
// Expect approximate floating-point equality of sequences of objects with custom epsilon value.
//

#define FOUNDATION_EXPECT_SEQUENCE_FEQ_EPS_IMPL(count, expected, expr, eps, stop)       \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
                                                                                        \
        bool are_equal__ = true;                                                        \
        for (size_t i__ = 0; i__ < count; ++i__)                                        \
        {                                                                               \
            if (!feq((expr)[i__], (expected)[i__], (eps)))                              \
            {                                                                           \
                are_equal__ = false;                                                    \
                break;                                                                  \
            }                                                                           \
        }                                                                               \
                                                                                        \
        if (!are_equal__)                                                               \
        {                                                                               \
            case_result.signal_assertion_failure();                                     \
                                                                                        \
            foundation::TestListenerHelper::write(                                      \
                test_listener,                                                          \
                current_test_suite__(),                                                 \
                get_name(),                                                             \
                __FILE__,                                                               \
                __LINE__,                                                               \
                foundation::TestMessage::AssertionFailure,                              \
                "expected: %s == %s\n"                                                  \
                "received: %s == %s",                                                   \
                #expr, foundation::to_string((expected), count).c_str(),                \
                #expr, foundation::to_string((expr), count).c_str());                   \
                                                                                        \
            if (stop)                                                                   \
                throw foundation::ExceptionAssertionFailure();                          \
        }                                                                               \
    } while (0)

#define EXPECT_SEQUENCE_FEQ_EPS(count, expected, expr, eps)                             \
    FOUNDATION_EXPECT_SEQUENCE_FEQ_EPS_IMPL(count, expected, expr, eps, false)

#define ASSERT_SEQUENCE_FEQ_EPS(count, expected, expr, eps)                             \
    FOUNDATION_EXPECT_SEQUENCE_FEQ_EPS_IMPL(count, expected, expr, eps, true)


//
// Expect approximate floating-point inequality.
//

#define FOUNDATION_EXPECT_FNEQ_IMPL(expected, expr, stop)                               \
    do {                                                                                \
        using namespace foundation;                                                     \
        case_result.signal_assertion_execution();                                       \
        if (feq((expr), (expected)))                                                    \
            FOUNDATION_ASSERTION_FAILURE("!=", expected, expr, stop);                   \
    } while (0)

#define EXPECT_FNEQ(expected, expr)                                                     \
    FOUNDATION_EXPECT_FNEQ_IMPL(expected, expr, false)

#define ASSERT_FNEQ(expected, expr)                                                     \
    FOUNDATION_EXPECT_FNEQ_IMPL(expected, expr, true)


//
// Expect approximate floating-point inequality with custom epsilon value.
//

#define FOUNDATION_EXPECT_FNEQ_EPS_IMPL(expected, expr, eps, stop)                      \
    do {                                                                                \
        using namespace foundation;                                                     \
        case_result.signal_assertion_execution();                                       \
        if (feq((expr), (expected), (eps)))                                             \
            FOUNDATION_ASSERTION_FAILURE("!=", expected, expr, stop);                   \
    } while (0)

#define EXPECT_FNEQ_EPS(expected, expr, eps)                                            \
    FOUNDATION_EXPECT_FNEQ_EPS_IMPL(expected, expr, eps, false)

#define ASSERT_FNEQ_EPS(expected, expr, eps)                                            \
    FOUNDATION_EXPECT_FNEQ_EPS_IMPL(expected, expr, eps, true)


//
// Expect a given exception to be thrown.
//

#define FOUNDATION_EXPECT_EXCEPTION_IMPL(exception, expr, stop)                         \
    do {                                                                                \
        case_result.signal_assertion_execution();                                       \
                                                                                        \
        bool caught_exception__ = false;                                                \
        try                                                                             \
        {                                                                               \
            expr;                                                                       \
        }                                                                               \
        catch (const exception&)                                                        \
        {                                                                               \
            caught_exception__ = true;                                                  \
        }                                                                               \
                                                                                        \
        if (!caught_exception__)                                                        \
        {                                                                               \
            case_result.signal_assertion_failure();                                     \
                                                                                        \
            foundation::TestListenerHelper::write(                                      \
                test_listener,                                                          \
                current_test_suite__(),                                                 \
                get_name(),                                                             \
                __FILE__,                                                               \
                __LINE__,                                                               \
                foundation::TestMessage::AssertionFailure,                              \
                "expected: %s throws exception %s\n"                                    \
                "received: no exception",                                               \
                #expr, #exception);                                                     \
                                                                                        \
            if (stop)                                                                   \
                throw foundation::ExceptionAssertionFailure();                          \
        }                                                                               \
    } while (0)

#define EXPECT_EXCEPTION(exception, expr)                                               \
    FOUNDATION_EXPECT_EXCEPTION_IMPL(exception, expr, false)

#define ASSERT_EXCEPTION(exception, expr)                                               \
    FOUNDATION_EXPECT_EXCEPTION_IMPL(exception, expr, true)

}   // namespace foundation
