//
//  assert_test.cpp - a test for boost/assert.hpp
//
//  Copyright (c) 2002 Peter Dimov and Multi Media Ltd.
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <boost/detail/lightweight_test.hpp>

#include <boost/assert.hpp>

void test_default()
{
    int x = 1;

    BOOST_ASSERT(1);
    BOOST_ASSERT(x);
    BOOST_ASSERT(x == 1);
    BOOST_ASSERT(&x);
}

#define BOOST_DISABLE_ASSERTS
#include <boost/assert.hpp>

void test_disabled()
{
    int x = 1;

    BOOST_ASSERT(1);
    BOOST_ASSERT(x);
    BOOST_ASSERT(x == 1);
    BOOST_ASSERT(&x);

    BOOST_ASSERT(0);
    BOOST_ASSERT(!x);
    BOOST_ASSERT(x == 0);

    void * p = 0;

    BOOST_ASSERT(p);

    // supress warnings
    p = &x;
    p = &p;
}

#undef BOOST_DISABLE_ASSERTS

#define BOOST_ENABLE_ASSERT_HANDLER
#include <boost/assert.hpp>
#include <boost/config.hpp>
#include <cstdio>

int handler_invoked = 0;

void boost::assertion_failed(char const * expr, char const * function, char const * file, long line)
{
#if !defined(BOOST_NO_STDC_NAMESPACE)
    using std::printf;
#endif

    printf("Expression: %s\nFunction: %s\nFile: %s\nLine: %ld\n\n", expr, function, file, line);
    ++handler_invoked;
}

struct X
{
    static void f()
    {
        BOOST_ASSERT(0);
    }
};

void test_handler()
{
    int x = 1;

    BOOST_ASSERT(1);
    BOOST_ASSERT(x);
    BOOST_ASSERT(x == 1);
    BOOST_ASSERT(&x);

    BOOST_ASSERT(0);
    BOOST_ASSERT(!x);
    BOOST_ASSERT(x == 0);

    void * p = 0;

    BOOST_ASSERT(p);

    X::f();

    BOOST_ASSERT(handler_invoked == 5);
    BOOST_TEST(handler_invoked == 5);
}

#undef BOOST_ENABLE_ASSERT_HANDLER

int main()
{
    test_default();
    test_disabled();
    test_handler();

    return boost::report_errors();
}
