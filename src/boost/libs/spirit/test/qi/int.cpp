/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    Copyright (c) 2001-2009 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <climits>
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_numeric.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_action.hpp>
#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include "test.hpp"

///////////////////////////////////////////////////////////////////////////////
//
//  *** BEWARE PLATFORM DEPENDENT!!! ***
//  *** The following assumes 32 bit integers and 64 bit long longs.
//  *** Modify these constant strings when appropriate.
//
///////////////////////////////////////////////////////////////////////////////
#ifdef BOOST_HAS_LONG_LONG
// Some compilers have long long, but don't define the
// LONG_LONG_MIN and LONG_LONG_MAX macros in limits.h.  This
// assumes that long long is 64 bits.
#if !defined(LONG_LONG_MIN) && !defined(LONG_LONG_MAX)
# define LONG_LONG_MAX 0x7fffffffffffffffLL
# define LONG_LONG_MIN (-LONG_LONG_MAX - 1)
#endif
#endif // BOOST_HAS_LONG_LONG

    char const* max_int = "2147483647";
    char const* int_overflow = "2147483648";
    char const* min_int = "-2147483648";
    char const* int_underflow = "-2147483649";

#ifdef BOOST_HAS_LONG_LONG
    char const* max_long_long = "9223372036854775807";
    char const* long_long_overflow = "9223372036854775808";
    char const* min_long_long = "-9223372036854775808";
    char const* long_long_underflow = "-9223372036854775809";
#endif

int
main()
{
    using namespace spirit_test;

    ///////////////////////////////////////////////////////////////////////////
    //  signed integer tests
    ///////////////////////////////////////////////////////////////////////////
    {
        using boost::spirit::int_;
        int i;

        BOOST_TEST(test("123456", int_));
        BOOST_TEST(test_attr("123456", int_, i));
        BOOST_TEST(i == 123456);

        BOOST_TEST(test("+123456", int_));
        BOOST_TEST(test_attr("+123456", int_, i));
        BOOST_TEST(i == 123456);

        BOOST_TEST(test("-123456", int_));
        BOOST_TEST(test_attr("-123456", int_, i));
        BOOST_TEST(i == -123456);

        BOOST_TEST(test(max_int, int_));
        BOOST_TEST(test_attr(max_int, int_, i));
        BOOST_TEST(i == INT_MAX);

        BOOST_TEST(test(min_int, int_));
        BOOST_TEST(test_attr(min_int, int_, i));
        BOOST_TEST(i == INT_MIN);

        BOOST_TEST(!test(int_overflow, int_));
        BOOST_TEST(!test_attr(int_overflow, int_, i));
        BOOST_TEST(!test(int_underflow, int_));
        BOOST_TEST(!test_attr(int_underflow, int_, i));

        BOOST_TEST(!test("-", int_));
        BOOST_TEST(!test_attr("-", int_, i));

        BOOST_TEST(!test("+", int_));
        BOOST_TEST(!test_attr("+", int_, i));

        // Bug report from Steve Nutt
        BOOST_TEST(test_attr("5368709120", int_, i, false));
        BOOST_TEST(i == 536870912);

        // with leading zeros
        BOOST_TEST(test("0000000000123456", int_));
        BOOST_TEST(test_attr("0000000000123456", int_, i));
        BOOST_TEST(i == 123456);
    }

    ///////////////////////////////////////////////////////////////////////////
    //  long long tests
    ///////////////////////////////////////////////////////////////////////////
#ifdef BOOST_HAS_LONG_LONG
    {
        using boost::spirit::long_long;
        boost::long_long_type ll;

        BOOST_TEST(test("1234567890123456789", long_long));
        BOOST_TEST(test_attr("1234567890123456789", long_long, ll));
        BOOST_TEST(ll == 1234567890123456789LL);

        BOOST_TEST(test("-1234567890123456789", long_long));
        BOOST_TEST(test_attr("-1234567890123456789", long_long, ll));
        BOOST_TEST(ll == -1234567890123456789LL);

        BOOST_TEST(test(max_long_long, long_long));
        BOOST_TEST(test_attr(max_long_long, long_long, ll));
        BOOST_TEST(ll == LONG_LONG_MAX);

        BOOST_TEST(test(min_long_long, long_long));
        BOOST_TEST(test_attr(min_long_long, long_long, ll));
        BOOST_TEST(ll == LONG_LONG_MIN);

        BOOST_TEST(!test(long_long_overflow, long_long));
        BOOST_TEST(!test_attr(long_long_overflow, long_long, ll));
        BOOST_TEST(!test(long_long_underflow, long_long));
        BOOST_TEST(!test_attr(long_long_underflow, long_long, ll));
    }
#endif

    ///////////////////////////////////////////////////////////////////////////
    //  int_spec<unused_type> tests
    ///////////////////////////////////////////////////////////////////////////
    {
        using boost::spirit::qi::int_spec;
        using boost::spirit::unused_type;
        int_spec<unused_type> any_int;

        BOOST_TEST(test("123456", any_int));
        BOOST_TEST(test("-123456", any_int));
        BOOST_TEST(test("-1234567890123456789", any_int));
    }

    ///////////////////////////////////////////////////////////////////////////
    //  action tests
    ///////////////////////////////////////////////////////////////////////////
    {
        using namespace boost::phoenix;
        using boost::spirit::arg_names::_1;
        using boost::spirit::ascii::space;
        using boost::spirit::int_;
        int n, m;

        BOOST_TEST(test("123", int_[ref(n) = _1]));
        BOOST_TEST(n == 123);
        BOOST_TEST(test_attr("789", int_[ref(n) = _1], m));
        BOOST_TEST(n == 789 && m == 789);
        BOOST_TEST(test("   456", int_[ref(n) = _1], space));
        BOOST_TEST(n == 456);
    }

    return boost::report_errors();
}
