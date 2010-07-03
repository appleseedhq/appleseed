//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// #define KARMA_TEST_COMPILE_FAIL

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>

#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_string.hpp>
#include <boost/spirit/include/karma_numeric.hpp>
#include <boost/spirit/include/karma_generate.hpp>
#include <boost/spirit/include/karma_operator.hpp>
#include <boost/spirit/include/karma_directive.hpp>
#include <boost/spirit/include/karma_action.hpp>
#include <boost/fusion/include/vector.hpp>
#include <boost/spirit/include/support_unused.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>

#include "test.hpp"

using namespace spirit_test;

///////////////////////////////////////////////////////////////////////////////
int
main()
{
    using namespace boost::spirit;
    namespace fusion = boost::fusion;
    
    {
        {
            BOOST_TEST(test("xi", char_('x') << char_('i')));
            BOOST_TEST(!test("xi", char_('x') << char_('o')));
        }

        {
            BOOST_TEST(test_delimited("x i ", char_('x') << 'i', char(' ')));
            BOOST_TEST(!test_delimited("x i ", 
                char_('x') << char_('o'), char(' ')));
        }

        {
            BOOST_TEST(test_delimited("Hello , World ", 
                lit("Hello") << ',' << "World", char(' ')));
        }
        
        {
            fusion::vector<char, char, std::string> p ('a', 'b', "cdefg");
            BOOST_TEST(test("abcdefg", char_ << char_ << lit, p));
            BOOST_TEST(test_delimited("a b cdefg ", 
                char_ << char_ << lit, p, char(' ')));
        }

        {
            fusion::vector<char, int, char> p ('a', 12, 'c');
            BOOST_TEST(test("a12c", char_ << int_ << char_, p));
            BOOST_TEST(test_delimited("a 12 c ", 
                char_ << int_ << char_, p, char(' ')));
        }

        {
            // if all elements of a sequence have unused parameters, the whole 
            // sequence has an unused parameter as well
            fusion::vector<char, char> p ('a', 'e');
            BOOST_TEST(test("abcde", 
                char_ << (char_('b') << 'c' << 'd') << char_, p));
            BOOST_TEST(test_delimited("a b c d e ", 
                char_ << (char_('b') << 'c' << 'd') << char_, p, char(' ')));
        }

        {
            // literal generators do not need a parameter
            fusion::vector<char, char> p('a', 'c');
            BOOST_TEST(test("abc", char_ << 'b' << char_, p));
            BOOST_TEST(test_delimited("a b c ", 
                char_ << 'b' << char_, p, char(' ')));
        }
        
        {
            using namespace boost::spirit::ascii;
            
            BOOST_TEST(test("aa", lower[char_('A') << 'a']));
            BOOST_TEST(test_delimited("BEGIN END ", 
                upper[lit("begin") << "end"], char(' ')));
            BOOST_TEST(!test_delimited("BEGIN END ", 
                upper[lit("begin") << "nend"], char(' ')));

            BOOST_TEST(test("Aa        ", left_align[char_('A') << 'a']));
            BOOST_TEST(test("    Aa    ", center[char_('A') << 'a']));
            BOOST_TEST(test("        Aa", right_align[char_('A') << 'a']));
        }

        // action tests
        {
            using namespace boost::phoenix;
            using namespace boost::spirit::arg_names;
            using namespace boost::spirit::ascii;

            BOOST_TEST(test("abcdefg", 
                (char_ << char_ << lit)[_1 = 'a', _2 = 'b', _3 = "cdefg"]));
            BOOST_TEST(test_delimited("a b cdefg ", 
                (char_ << char_ << lit)[_1 = 'a', _2 = 'b', _3 = "cdefg"], 
                char(' ')));

            BOOST_TEST(test_delimited("a 12 c ", 
                (char_ << int_(12) << char_)[_1 = 'a', _2 = 'c'], char(' ')));

            char c = 'c';
            BOOST_TEST(test("abc", 
                (char_[_1 = 'a'] << 'b' << char_)[_1 = 'x', _2 = ref(c)]));
            BOOST_TEST(test_delimited("a b c ", 
                (char_[_1 = 'a'] << 'b' << char_)[_2 = ref(c)], char(' ')));

            BOOST_TEST(test("aa", lower[char_ << 'A'][_1 = 'A']));
            BOOST_TEST(test("AA", upper[char_ << 'a'][_1 = 'a']));

            BOOST_TEST(test("Aa        ", left_align[char_ << 'a'][_1 = 'A']));
            BOOST_TEST(test("    Aa    ", center[char_ << 'a'][_1 = 'A']));
            BOOST_TEST(test("        Aa", right_align[char_ << 'a'][_1 = 'A']));
        }
    }
    
    return boost::report_errors();
}

