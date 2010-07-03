/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    Copyright (c) 2001-2009 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_operator.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_string.hpp>
#include <boost/spirit/include/qi_numeric.hpp>
#include <boost/spirit/include/qi_action.hpp>
#include <boost/spirit/include/qi_directive.hpp>
#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/variant.hpp>

#include <string>
#include <iostream>
#include "test.hpp"

using namespace spirit_test;

int
main()
{
    using namespace boost::spirit;

    {
        BOOST_TEST((test("a", char_ | char_)));
        BOOST_TEST((test("x", char_('x') | char_('i'))));
        BOOST_TEST((test("i", char_('x') | char_('i'))));
        BOOST_TEST((!test("z", char_('x') | char_('o'))));
        BOOST_TEST((test("rock", lit("rock") | lit("roll"))));
        BOOST_TEST((test("roll", lit("rock") | lit("roll"))));
        BOOST_TEST((test("rock", lit("rock") | int_)));
        BOOST_TEST((test("12345", lit("rock") | int_)));
    }

    {
        boost::variant<unused_type, int, char> v;

        BOOST_TEST((test_attr("12345", lit("rock") | int_ | char_, v)));
        BOOST_TEST(boost::get<int>(v) == 12345);

        BOOST_TEST((test_attr("rock", lit("rock") | int_ | char_, v)));
//        BOOST_TEST(boost::get<std::string>(&v) == 0);
        BOOST_TEST(boost::get<int>(&v) == 0);
        BOOST_TEST(boost::get<char>(&v) == 0);

        BOOST_TEST((test_attr("x", lit("rock") | int_ | char_, v)));
        BOOST_TEST(boost::get<char>(v) == 'x');
    }

    {   // test action
        using namespace boost::phoenix;
        using namespace boost::spirit::arg_names;
        namespace phx = boost::phoenix;

        boost::variant<unused_type, int, char> v;

        BOOST_TEST((test("12345", (lit("rock") | int_ | char_)[phx::ref(v) = _1])));
        BOOST_TEST(boost::get<int>(v) == 12345);
    }

    {
        boost::variant<unused_type> v;
        BOOST_TEST((test("rock", lit("rock") | char_('x'))));
    }

    {
        // test if alternatives with all components having unused
        // attributes return attributes them self
        using namespace boost::fusion;

        vector<char, char> v;
        BOOST_TEST((test_attr("abc",
            char_ >> (omit[char_] | omit[char_]) >> char_, v)));
        BOOST_TEST((at_c<0>(v) == 'a'));
        BOOST_TEST((at_c<1>(v) == 'c'));
    }

    return boost::report_errors();
}

