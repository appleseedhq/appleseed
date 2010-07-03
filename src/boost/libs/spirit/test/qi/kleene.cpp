/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <string>
#include <vector>

#include <boost/detail/lightweight_test.hpp>
#include <boost/utility/enable_if.hpp>

#include <boost/spirit/include/qi_operator.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_string.hpp>
#include <boost/spirit/include/qi_numeric.hpp>
#include <boost/spirit/include/qi_directive.hpp>
#include <boost/spirit/include/qi_action.hpp>
#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <string>
#include <iostream>
#include "test.hpp"

using namespace spirit_test;

int
main()
{
    using namespace boost::spirit;
    using namespace boost::spirit::ascii;

    {
        BOOST_TEST(test("aaaaaaaa", *char_));
        BOOST_TEST(test("a", *char_));
        BOOST_TEST(test("", *char_));
        BOOST_TEST(test("aaaaaaaa", *alpha));
        BOOST_TEST(!test("aaaaaaaa", *upper));
    }

    {
        BOOST_TEST(test(" a a aaa aa", *char_, space));
        BOOST_TEST(test("12345 678 9 ", *digit, space));
    }

    {
        BOOST_TEST(test("aBcdeFGH", no_case[*char_]));
        BOOST_TEST(test("a B cde FGH  ", no_case[*char_], space));
    }

    {
        using boost::spirit::uint;
        BOOST_TEST(test("12345 678 955 987", *uint, space));
        BOOST_TEST(test("12345, 678, 955, 987", uint >> *(',' >> uint), space));
    }

    {
        std::vector<char> v;
        BOOST_TEST(test_attr("bbbb", *char_, v) && 4 == v.size() &&
            v[0] == 'b' && v[1] == 'b' && v[2] == 'b' &&  v[3] == 'b');

        v.clear();
        BOOST_TEST(test_attr("b b b b ", *char_, v, space) && 4 == v.size() &&
            v[0] == 'b' && v[1] == 'b' && v[2] == 'b' && v[3] == 'b');

        v.clear();
        BOOST_TEST(test_attr("bbbb", *omit[char_('b')], v) && 0 == v.size());

        v.clear();
        BOOST_TEST(test_attr("bbbb", omit[*char_('b')], v) && 0 == v.size());

        v.clear();
        BOOST_TEST(test_attr("b b b b ", *omit[char_('b')], v, space) && 0 == v.size());

        v.clear();
        BOOST_TEST(test_attr("b b b b ", omit[*char_('b')], v, space) && 0 == v.size());
    }

    {
        std::vector<int> v;
        BOOST_TEST(test_attr("123 456 789 10", *int_, v, space) && 4 == v.size() &&
            v[0] == 123 && v[1] == 456 && v[2] == 789 &&  v[3] == 10);
    }

    {
        std::vector<int> v;
        BOOST_TEST(test_attr("123 456 789", *int_, v, space) && 3 == v.size() &&
            v[0] == 123 && v[1] == 456 && v[2] == 789);
    }

    { // actions
        namespace phx = boost::phoenix;
        using boost::spirit::arg_names::_1;

        std::vector<char> v;
        BOOST_TEST(test("bbbb", (*char_)[phx::ref(v) = _1]) && 4 == v.size() &&
            v[0] == 'b' && v[1] == 'b' && v[2] == 'b' &&  v[3] == 'b');
    }

    { // more actions
        namespace phx = boost::phoenix;
        using boost::spirit::arg_names::_1;

        std::vector<int> v;
        BOOST_TEST(test("123 456 789", (*int_)[phx::ref(v) = _1], space) && 3 == v.size() &&
            v[0] == 123 && v[1] == 456 && v[2] == 789);
    }

    return boost::report_errors();
}

