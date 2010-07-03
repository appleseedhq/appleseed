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
    using spirit_test::test;
    using boost::spirit::qi::expectation_failure;

    {
        try
        {
            BOOST_TEST((test("aa", char_ > char_)));
            BOOST_TEST((test("aaa", char_ > char_ > char_('a'))));
            BOOST_TEST((test("xi", char_('x') > char_('i'))));
            BOOST_TEST((!test("xi", char_('y') > char_('o')))); // should not throw!
            BOOST_TEST((test("xin", char_('x') > char_('i') > char_('n'))));
            BOOST_TEST((!test("xi", char_('x') > char_('o'))));
        }
        catch (expectation_failure<char const*> const& x)
        {
            std::cout << "expected: " << x.what << std::endl;
            std::cout << "got: \"" << x.first << '"' << std::endl;

            BOOST_TEST(x.what == "'o'");
            BOOST_TEST(std::string(x.first, x.last) == "i");
        }
    }

    {
        try
        {
            BOOST_TEST((test(" a a ", char_ > char_, space)));
            BOOST_TEST((test(" x i ", char_('x') > char_('i'), space)));
            BOOST_TEST((!test(" x i ", char_('x') > char_('o'), space)));
        }
        catch (expectation_failure<char const*> const& x)
        {
            std::cout << "expected: " << x.what << std::endl;
            std::cout << "got: \"" << x.first << '"' << std::endl;

            BOOST_TEST(x.what == "'o'");
            BOOST_TEST(std::string(x.first, x.last) == "i ");
        }
    }

    {
        try
        {
            BOOST_TEST((test("aA", no_case[char_('a') > 'a'])));
            BOOST_TEST((test("BEGIN END", no_case[lit("begin") > "end"], space)));
            BOOST_TEST((!test("BEGIN END", no_case[lit("begin") > "nend"], space)));
        }
        catch (expectation_failure<char const*> const& x)
        {
            std::cout << "expected: " << x.what << std::endl;
            std::cout << "got: \"" << x.first << '"' << std::endl;

            BOOST_TEST(x.what == "case-insensitive \"nend\"");
            BOOST_TEST(std::string(x.first, x.last) == "END");
        }
    }

    return boost::report_errors();
}

