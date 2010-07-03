//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//#define KARMA_FAIL_COMPILATION

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/for_each.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/bool.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>

#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_numeric.hpp>
#include <boost/spirit/include/karma_generate.hpp>
#include <boost/spirit/include/karma_directive.hpp>
#include <boost/spirit/include/karma_action.hpp>

#include <limits>
#include "test.hpp"

using namespace spirit_test;

///////////////////////////////////////////////////////////////////////////////
struct test_minmax
{
    template <typename T>
    void operator()(T) const
    {
        using namespace boost::spirit;
        using namespace boost::phoenix;
        using namespace boost::spirit::arg_names;
        
        T minval = (std::numeric_limits<T>::min)();
        T maxval = (std::numeric_limits<T>::max)();
        
        std::string expected_minval = boost::lexical_cast<std::string>(minval); 
        std::string expected_maxval = boost::lexical_cast<std::string>(maxval);
        
        // create a correct generator type from the given integer type
        typedef typename
            boost::mpl::if_<
                boost::mpl::bool_<std::numeric_limits<T>::is_signed>,
                karma::int_spec<T>,
                karma::uint_spec<T>
            >::type
        int_spec_type;
        
        int_spec_type const gen = int_spec_type();

        BOOST_TEST(test(expected_maxval, gen, maxval));
        BOOST_TEST(test(expected_minval, gen, minval));
        BOOST_TEST(test(expected_maxval, gen(maxval)));
        BOOST_TEST(test(expected_minval, gen(minval)));

        BOOST_TEST(test_delimited(expected_maxval + " ", gen, maxval, char(' ')));
        BOOST_TEST(test_delimited(expected_minval + " ", gen, minval, char(' ')));
        BOOST_TEST(test_delimited(expected_maxval + " ", gen(maxval), char(' ')));
        BOOST_TEST(test_delimited(expected_minval + " ", gen(minval), char(' ')));

    // action tests
        BOOST_TEST(test(expected_maxval, gen[_1 = val(maxval)]));
        BOOST_TEST(test(expected_minval, gen[_1 = val(minval)]));
    }
};

///////////////////////////////////////////////////////////////////////////////
int
main()
{
    using namespace boost::spirit;
    
    {
        using namespace boost::spirit::ascii;
        
        ///////////////////////////////////////////////////////////////////////
        // this is currently ambiguous with character literals
//         BOOST_TEST(test("0", 0));
//         BOOST_TEST(test("123", 123));
//         BOOST_TEST(test("-123", -123));

        BOOST_TEST(test("0", int_, 0));
        BOOST_TEST(test("123", int_, 123));
        BOOST_TEST(test("-123", int_, -123));

        BOOST_TEST(test_delimited("0 ", int_, 0, char_(' ')));
        BOOST_TEST(test_delimited("123 ", int_, 123, char_(' ')));
        BOOST_TEST(test_delimited("-123 ", int_, -123, char_(' ')));

        BOOST_TEST(test("0", lower[int_], 0));
        BOOST_TEST(test("123", lower[int_], 123));
        BOOST_TEST(test("-123", lower[int_], -123));

        BOOST_TEST(test_delimited("0 ", lower[int_], 0, char_(' ')));
        BOOST_TEST(test_delimited("123 ", lower[int_], 123, char_(' ')));
        BOOST_TEST(test_delimited("-123 ", lower[int_], -123, char_(' ')));

        BOOST_TEST(test("0", upper[int_], 0));
        BOOST_TEST(test("123", upper[int_], 123));
        BOOST_TEST(test("-123", upper[int_], -123));

        BOOST_TEST(test_delimited("0 ", upper[int_], 0, char_(' ')));
        BOOST_TEST(test_delimited("123 ", upper[int_], 123, char_(' ')));
        BOOST_TEST(test_delimited("-123 ", upper[int_], -123, char_(' ')));

        ///////////////////////////////////////////////////////////////////////
        BOOST_TEST(test("0", int_(0)));
        BOOST_TEST(test("123", int_(123)));
        BOOST_TEST(test("-123", int_(-123)));

        BOOST_TEST(test_delimited("0 ", int_(0), char_(' ')));
        BOOST_TEST(test_delimited("123 ", int_(123), char_(' ')));
        BOOST_TEST(test_delimited("-123 ", int_(-123), char_(' ')));

        BOOST_TEST(test("0", lower[int_(0)]));
        BOOST_TEST(test("123", lower[int_(123)]));
        BOOST_TEST(test("-123", lower[int_(-123)]));

        BOOST_TEST(test_delimited("0 ", lower[int_(0)], char_(' ')));
        BOOST_TEST(test_delimited("123 ", lower[int_(123)], char_(' ')));
        BOOST_TEST(test_delimited("-123 ", lower[int_(-123)], char_(' ')));

        BOOST_TEST(test("0", upper[int_(0)]));
        BOOST_TEST(test("123", upper[int_(123)]));
        BOOST_TEST(test("-123", upper[int_(-123)]));

        BOOST_TEST(test_delimited("0 ", upper[int_(0)], char_(' ')));
        BOOST_TEST(test_delimited("123 ", upper[int_(123)], char_(' ')));
        BOOST_TEST(test_delimited("-123 ", upper[int_(-123)], char_(' ')));
    }

    {
        using namespace boost::spirit::ascii;
        
        karma::int_spec<int, 10, true> const signed_int =
            karma::int_spec<int, 10, true>();

        ///////////////////////////////////////////////////////////////////////
        BOOST_TEST(test(" 0", signed_int, 0));
        BOOST_TEST(test("+123", signed_int, 123));
        BOOST_TEST(test("-123", signed_int, -123));

        BOOST_TEST(test_delimited(" 0 ", signed_int, 0, char_(' ')));
        BOOST_TEST(test_delimited("+123 ", signed_int, 123, char_(' ')));
        BOOST_TEST(test_delimited("-123 ", signed_int, -123, char_(' ')));

        BOOST_TEST(test(" 0", lower[signed_int], 0));
        BOOST_TEST(test("+123", lower[signed_int], 123));
        BOOST_TEST(test("-123", lower[signed_int], -123));

        BOOST_TEST(test_delimited(" 0 ", lower[signed_int], 0, char_(' ')));
        BOOST_TEST(test_delimited("+123 ", lower[signed_int], 123, char_(' ')));
        BOOST_TEST(test_delimited("-123 ", lower[signed_int], -123, char_(' ')));

        BOOST_TEST(test(" 0", upper[signed_int], 0));
        BOOST_TEST(test("+123", upper[signed_int], 123));
        BOOST_TEST(test("-123", upper[signed_int], -123));

        BOOST_TEST(test_delimited(" 0 ", upper[signed_int], 0, char_(' ')));
        BOOST_TEST(test_delimited("+123 ", upper[signed_int], 123, char_(' ')));
        BOOST_TEST(test_delimited("-123 ", upper[signed_int], -123, char_(' ')));

        ///////////////////////////////////////////////////////////////////////
        BOOST_TEST(test(" 0", signed_int(0)));
        BOOST_TEST(test("+123", signed_int(123)));
        BOOST_TEST(test("-123", signed_int(-123)));

        BOOST_TEST(test_delimited(" 0 ", signed_int(0), char_(' ')));
        BOOST_TEST(test_delimited("+123 ", signed_int(123), char_(' ')));
        BOOST_TEST(test_delimited("-123 ", signed_int(-123), char_(' ')));

        BOOST_TEST(test(" 0", lower[signed_int(0)]));
        BOOST_TEST(test("+123", lower[signed_int(123)]));
        BOOST_TEST(test("-123", lower[signed_int(-123)]));

        BOOST_TEST(test_delimited(" 0 ", lower[signed_int(0)], char_(' ')));
        BOOST_TEST(test_delimited("+123 ", lower[signed_int(123)], char_(' ')));
        BOOST_TEST(test_delimited("-123 ", lower[signed_int(-123)], char_(' ')));

        BOOST_TEST(test(" 0", upper[signed_int(0)]));
        BOOST_TEST(test("+123", upper[signed_int(123)]));
        BOOST_TEST(test("-123", upper[signed_int(-123)]));

        BOOST_TEST(test_delimited(" 0 ", upper[signed_int(0)], char_(' ')));
        BOOST_TEST(test_delimited("+123 ", upper[signed_int(123)], char_(' ')));
        BOOST_TEST(test_delimited("-123 ", upper[signed_int(-123)], char_(' ')));
    }

    {
        ///////////////////////////////////////////////////////////////////////
        using boost::spirit::uint_;
        using namespace boost::spirit::ascii;
        
        BOOST_TEST(test("1234", uint_, 1234));
        BOOST_TEST(test("ff", hex, 0xff));
        BOOST_TEST(test("1234", oct, 01234));
        BOOST_TEST(test("11110000", bin, 0xf0));
        
        BOOST_TEST(test_delimited("1234 ", uint_, 1234, char_(' ')));
        BOOST_TEST(test_delimited("ff ", hex, 0xff, char_(' ')));
        BOOST_TEST(test_delimited("1234 ", oct, 01234, char_(' ')));
        BOOST_TEST(test_delimited("11110000 ", bin, 0xf0, char_(' ')));
        
        BOOST_TEST(test("1234", lower[uint_], 1234));
        BOOST_TEST(test("ff", lower[hex], 0xff));
        BOOST_TEST(test("1234", lower[oct], 01234));
        BOOST_TEST(test("11110000", lower[bin], 0xf0));
        
        BOOST_TEST(test_delimited("1234 ", lower[uint_], 1234, char_(' ')));
        BOOST_TEST(test_delimited("ff ", lower[hex], 0xff, char_(' ')));
        BOOST_TEST(test_delimited("1234 ", lower[oct], 01234, char_(' ')));
        BOOST_TEST(test_delimited("11110000 ", lower[bin], 0xf0, char_(' ')));
        
        BOOST_TEST(test("1234", upper[uint_], 1234));
        BOOST_TEST(test("FF", upper[hex], 0xff));
        BOOST_TEST(test("1234", upper[oct], 01234));
        BOOST_TEST(test("11110000", upper[bin], 0xf0));
        
        BOOST_TEST(test_delimited("1234 ", upper[uint_], 1234, char_(' ')));
        BOOST_TEST(test_delimited("FF ", upper[hex], 0xff, char_(' ')));
        BOOST_TEST(test_delimited("1234 ", upper[oct], 01234, char_(' ')));
        BOOST_TEST(test_delimited("11110000 ", upper[bin], 0xf0, char_(' ')));
        
        // no generator transformation should occur for uint_'s
        BOOST_TEST(test("1234", upper[upper[uint_]], 1234));
        BOOST_TEST(test("1234", upper[lower[uint_]], 1234));
        BOOST_TEST(test("1234", lower[upper[uint_]], 1234));
        BOOST_TEST(test("1234", lower[lower[uint_]], 1234));
        
        BOOST_TEST(test_delimited("1234 ", upper[upper[uint_]], 1234, char_(' ')));
        BOOST_TEST(test_delimited("1234 ", upper[lower[uint_]], 1234, char_(' ')));
        BOOST_TEST(test_delimited("1234 ", lower[upper[uint_]], 1234, char_(' ')));
        BOOST_TEST(test_delimited("1234 ", lower[lower[uint_]], 1234, char_(' ')));
        
        BOOST_TEST(test("FF", upper[upper[hex]], 0xff));
        BOOST_TEST(test("FF", upper[lower[hex]], 0xff));
        BOOST_TEST(test("ff", lower[upper[hex]], 0xff));
        BOOST_TEST(test("ff", lower[lower[hex]], 0xff));

        BOOST_TEST(test_delimited("FF ", upper[upper[hex]], 0xff, char_(' ')));
        BOOST_TEST(test_delimited("FF ", upper[lower[hex]], 0xff, char_(' ')));
        BOOST_TEST(test_delimited("ff ", lower[upper[hex]], 0xff, char_(' ')));
        BOOST_TEST(test_delimited("ff ", lower[lower[hex]], 0xff, char_(' ')));

        ///////////////////////////////////////////////////////////////////////
        BOOST_TEST(test("1234", uint_(1234)));
        BOOST_TEST(test("ff", hex(0xff)));
        BOOST_TEST(test("1234", oct(01234)));
        BOOST_TEST(test("11110000", bin(0xf0)));
        
        BOOST_TEST(test_delimited("1234 ", uint_(1234), char_(' ')));
        BOOST_TEST(test_delimited("ff ", hex(0xff), char_(' ')));
        BOOST_TEST(test_delimited("1234 ", oct(01234), char_(' ')));
        BOOST_TEST(test_delimited("11110000 ", bin(0xf0), char_(' ')));

        BOOST_TEST(test("1234", lower[uint_(1234)]));
        BOOST_TEST(test("ff", lower[hex(0xff)]));
        BOOST_TEST(test("1234", lower[oct(01234)]));
        BOOST_TEST(test("11110000", lower[bin(0xf0)]));
        
        BOOST_TEST(test_delimited("1234 ", lower[uint_(1234)], char_(' ')));
        BOOST_TEST(test_delimited("ff ", lower[hex(0xff)], char_(' ')));
        BOOST_TEST(test_delimited("1234 ", lower[oct(01234)], char_(' ')));
        BOOST_TEST(test_delimited("11110000 ", lower[bin(0xf0)], char_(' ')));
        
        BOOST_TEST(test("1234", upper[uint_(1234)]));
        BOOST_TEST(test("FF", upper[hex(0xff)]));
        BOOST_TEST(test("1234", upper[oct(01234)]));
        BOOST_TEST(test("11110000", upper[bin(0xf0)]));
        
        BOOST_TEST(test_delimited("1234 ", upper[uint_(1234)], char_(' ')));
        BOOST_TEST(test_delimited("FF ", upper[hex(0xff)], char_(' ')));
        BOOST_TEST(test_delimited("1234 ", upper[oct(01234)], char_(' ')));
        BOOST_TEST(test_delimited("11110000 ", upper[bin(0xf0)], char_(' ')));
        
        BOOST_TEST(test("FF", upper[upper[hex(0xff)]]));
        BOOST_TEST(test("FF", upper[lower[hex(0xff)]]));
        BOOST_TEST(test("ff", lower[upper[hex(0xff)]]));
        BOOST_TEST(test("ff", lower[lower[hex(0xff)]]));

        BOOST_TEST(test_delimited("FF ", upper[upper[hex(0xff)]], char_(' ')));
        BOOST_TEST(test_delimited("FF ", upper[lower[hex(0xff)]], char_(' ')));
        BOOST_TEST(test_delimited("ff ", lower[upper[hex(0xff)]], char_(' ')));
        BOOST_TEST(test_delimited("ff ", lower[lower[hex(0xff)]], char_(' ')));
    }

// test boundary values
    typedef boost::mpl::vector<
#ifdef BOOST_HAS_LONG_LONG
        boost::long_long_type, boost::ulong_long_type,
#endif
        short, unsigned short, 
        int, unsigned int, 
        long, unsigned long
    > integer_types;
    boost::mpl::for_each<integer_types>(test_minmax());
    
    return boost::report_errors();
}

