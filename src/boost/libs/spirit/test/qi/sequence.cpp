/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_operator.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_string.hpp>
#include <boost/spirit/include/qi_numeric.hpp>
#include <boost/spirit/include/qi_directive.hpp>
#include <boost/spirit/include/qi_action.hpp>
#include <boost/spirit/include/support_argument.hpp>
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/at.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>

#include <string>
#include <iostream>
#include "test.hpp"

int
main()
{
    using namespace boost::spirit;
    using namespace boost::spirit::ascii;
    using boost::fusion::vector;
    using boost::fusion::at_c;
    using spirit_test::test;
    using spirit_test::test_attr;

    {
        BOOST_TEST((test("aa", char_ >> char_)));
        BOOST_TEST((test("aaa", char_ >> char_ >> char_('a'))));
        BOOST_TEST((test("xi", char_('x') >> char_('i'))));
        BOOST_TEST((!test("xi", char_('x') >> char_('o'))));
        BOOST_TEST((test("xin", char_('x') >> char_('i') >> char_('n'))));

    }

    {
        BOOST_TEST((test(" a a ", char_ >> char_, space)));
        BOOST_TEST((test(" x i ", char_('x') >> char_('i'), space)));
        BOOST_TEST((!test(" x i ", char_('x') >> char_('o'), space)));
    }

    {
        BOOST_TEST((test(" Hello, World", lit("Hello") >> ',' >> "World", space)));
    }

    {
        vector<char, char> attr;
        BOOST_TEST((test_attr("abcdefg", char_ >> char_ >> "cdefg", attr)));
        BOOST_TEST((at_c<0>(attr) == 'a'));
        BOOST_TEST((at_c<1>(attr) == 'b'));
    }

    {
        vector<char, char, char> attr;
        BOOST_TEST((test_attr(" a\n  b\n  c\n ", char_ >> char_ >> char_, attr, space)));
        BOOST_TEST((at_c<0>(attr) == 'a'));
        BOOST_TEST((at_c<1>(attr) == 'b'));
        BOOST_TEST((at_c<2>(attr) == 'c'));
    }

    {
        // unused_type means we don't care about the attribute
        vector<char, char> attr;
        BOOST_TEST((test_attr("abc", char_ >> 'b' >> char_, attr)));
        BOOST_TEST((at_c<0>(attr) == 'a'));
        BOOST_TEST((at_c<1>(attr) == 'c'));
    }

    {
        // omit[] means we don't receive the attribute
        vector<char> attr;
        BOOST_TEST((test_attr("abc", omit[char_] >> omit['b'] >> char_, attr)));
        BOOST_TEST((at_c<0>(attr) == 'c'));
    }

    {
        // If all elements except 1 is omitted, the attribute is
        // a single-element sequence. For this case alone, we allow
        // naked attributes (unwrapped in a fusion sequence).
        char attr;
        BOOST_TEST((test_attr("abc", omit[char_] >> 'b' >> char_, attr)));
        BOOST_TEST((attr == 'c'));
    }

    {
        // omit[] means we don't receive the attribute
        vector<> attr;
        BOOST_TEST((test_attr("abc", omit[char_] >> omit['b'] >> omit[char_], attr)));
    }

    {
        // omit[] means we don't receive the attribute
        // this test is merely a compile test, because using a unused as the
        // explicit attribute doesn't make any sense
        unused_type attr;
        BOOST_TEST((test_attr("abc", omit[char_ >> 'b' >> char_], attr)));
    }

    {
        // omit[] means we don't receive the attribute, if all elements of a
        // sequence have unused attributes, the whole sequence has an unused
        // attribute as well
        vector<char, char> attr;
        BOOST_TEST((test_attr("abcde",
            char_ >> (omit[char_] >> omit['c'] >> omit[char_]) >> char_, attr)));
        BOOST_TEST((at_c<0>(attr) == 'a'));
        BOOST_TEST((at_c<1>(attr) == 'e'));
    }

    {
        // "hello" has an unused_type. unused attrubutes are not part of the sequence
        vector<char, char> attr;
        BOOST_TEST((test_attr("a hello c", char_ >> "hello" >> char_, attr, space)));
        BOOST_TEST((at_c<0>(attr) == 'a'));
        BOOST_TEST((at_c<1>(attr) == 'c'));
    }

    {
        // omit[] means we don't receive the attribute
        vector<char> attr;
        BOOST_TEST((test_attr("a hello c", char_ >> "hello" >> omit[char_], attr, space)));
        BOOST_TEST((at_c<0>(attr) == 'a'));
    }

    {
        // if only one node in a sequence is left (all the others are omitted),
        // then we should also allow "naked" attributes (unwraped in a tuple)
        int attr;
        BOOST_TEST((test_attr("a 123 c", omit['a'] >> int_ >> omit['c'], attr, space)));
        BOOST_TEST((attr == 123));
    }

    {
        // unused means we don't care about the attribute
        BOOST_TEST((test_attr("abc", char_ >> 'b' >> char_, unused)));
    }

    {
        BOOST_TEST((test("aA", no_case[char_('a') >> 'a'])));
        BOOST_TEST((test("BEGIN END", no_case[lit("begin") >> "end"], space)));
        BOOST_TEST((!test("BEGIN END", no_case[lit("begin") >> "nend"], space)));
    }

    {
#ifdef SPIRIT_TEST_COMPILE_FAIL // $$$
        char_ >> char_ = char_ >> char_; // disallow this!
#endif
    }

    {   // test action
        using namespace boost::phoenix;
        using namespace boost::spirit::arg_names;
        char c = 0;
        int n = 0;

        BOOST_TEST(test("x123\"a string\"", (char_ >> int_ >> "\"a string\"")
            [ref(c) = _1, ref(n) = _2]));
        BOOST_TEST(c == 'x');
        BOOST_TEST(n == 123);
    }

    {   // test action with omitted attribute
        using namespace boost::phoenix;
        using namespace boost::spirit::arg_names;
        char c = 0;

        BOOST_TEST(test("x123\"a string\"", (char_ >> omit[int_] >> "\"a string\"")
            [ref(c) = _1]));
        BOOST_TEST(c == 'x');
    }

    {   // test action
        using namespace boost::phoenix;
        using namespace boost::spirit::arg_names;
        char c = 0;
        int n = 0;

        BOOST_TEST(test("x 123 \"a string\"", (char_ >> int_ >> "\"a string\"")
            [ref(c) = _1, ref(n) = _2], space));
        BOOST_TEST(c == 'x');
        BOOST_TEST(n == 123);
    }

    {   // test action with omitted attribute
        using namespace boost::phoenix;
        using namespace boost::spirit::arg_names;
        int n = 0;

        BOOST_TEST(test("x 123 \"a string\"",
            (omit[char_] >> int_ >> "\"a string\"")[ref(n) = _1], space));
        BOOST_TEST(n == 123);
    }

    return boost::report_errors();
}

