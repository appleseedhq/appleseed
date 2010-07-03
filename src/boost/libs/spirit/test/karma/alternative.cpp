//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// #define KARMA_TEST_COMPILE_FAIL

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>

#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_string.hpp>
#include <boost/spirit/include/karma_numeric.hpp>
#include <boost/spirit/include/karma_generate.hpp>
#include <boost/spirit/include/karma_operator.hpp>
#include <boost/spirit/include/karma_directive.hpp>

#include "test.hpp"

using namespace spirit_test;

///////////////////////////////////////////////////////////////////////////////
int
main()
{
    using namespace boost;
    using namespace boost::spirit;
    
    {
        {
            BOOST_TEST(test("x", char_('x') | char_('i')));
            BOOST_TEST(test("xi", char_('x') << char_('i') | char_('i')));

            variant<int, char> v (10);
            BOOST_TEST(test("10", char_ | int_, v));
            BOOST_TEST(test("a", char_('a') | char_ | int_, v));

            v = 'c';
            BOOST_TEST(test("c", char_ | int_, v));
            BOOST_TEST(test("a", char_('a') | char_ | int_, v));
        }

        {
            // test if alternatives with all components having unused 
            // parameter return attributes them self
            fusion::vector<char, char> v('a', 'b');
            BOOST_TEST(test("axb", char_ << (char_('x') | char_('i')) << char_, v));
            BOOST_TEST(test("axib", 
                char_ << (char_('x') << char_('i') | char_('i')) << char_, v));
        }

        {
            BOOST_TEST(test_delimited("x ", char_('x') | char_('i'), char_(' ')));
            BOOST_TEST(test_delimited("x i ", 
                char_('x') << char_('i') | char_('i'), char_(' ')));

            variant<int, char> v (10);
            BOOST_TEST(test_delimited("10 ", char_ | int_, v, char_(' ')));

            v = 'c';
            BOOST_TEST(test_delimited("c ", char_ | int_, v, char_(' ')));
        }

        {
            // if nothing matches, the first explicit alternative will be chosen
            variant<double, char const*> v (10.0);
            BOOST_TEST(test("11", char_ | int_(11), v));
            BOOST_TEST(test("10.0", double_ | int_(11), v));
            BOOST_TEST(!test("", char_ | int_, v));

            v = "c";
            BOOST_TEST(test("11", char_ | int_(11), v));
            BOOST_TEST(test("11", double_ | int_(11), v));
            BOOST_TEST(!test("", char_ | int_, v));
        }

        {
            // if nothing matches, the first explicit alternative will be chosen
            variant<double, char const*> v (10.0);
            BOOST_TEST(test_delimited("11 ", char_ | int_(11), v, char_(' ')));

            v = "c";
            BOOST_TEST(test_delimited("11 ", char_ | int_(11), v, char_(' ')));
        }
    }
    
    return boost::report_errors();
}

