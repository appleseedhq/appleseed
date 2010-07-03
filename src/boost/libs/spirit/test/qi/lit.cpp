/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    http://spirit.sourceforge.net/

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_string.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_action.hpp>
#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <iostream>
#include "test.hpp"

int
main()
{
    using spirit_test::test;
    using spirit_test::test_attr;
    using namespace boost::spirit;

    {
        BOOST_TEST((test("kimpo", "kimpo")));
        BOOST_TEST((test("kimpo", lit("kimpo"))));
        BOOST_TEST((test("x", lit("x"))));
        BOOST_TEST((test("x", lit('x'))));
        BOOST_TEST((test(L"x", lit(L'x'))));
    }

    {
        BOOST_TEST((test(L"kimpo", L"kimpo")));
        BOOST_TEST((test(L"kimpo", wlit(L"kimpo"))));
        BOOST_TEST((test(L"x", wlit(L"x"))));
        BOOST_TEST((test(L"x", wlit(L'x'))));
        BOOST_TEST((test(L"x", wlit(L'x'))));
    }

    {
        std::basic_string<char> s("kimpo");
        BOOST_TEST((test("kimpo", lit(s))));

        std::basic_string<wchar_t> ws(L"kimpo");
        BOOST_TEST((test(L"kimpo", lit(ws))));
    }

    {
        using namespace boost::spirit::ascii;
        BOOST_TEST((test("    kimpo", lit("kimpo"), space)));
        BOOST_TEST((test(L"    kimpo", lit(L"kimpo"), space)));
        BOOST_TEST((test("    x", lit("x"), space)));
        BOOST_TEST((test("    x", lit('x'), space)));
        BOOST_TEST((test(L"    x", lit(L'x'), space)));
    }

    {
        using namespace boost::spirit::ascii;
        BOOST_TEST((test("    kimpo", wlit("kimpo"), space)));
        BOOST_TEST((test(L"    kimpo", wlit(L"kimpo"), space)));
        BOOST_TEST((test("    x", wlit("x"), space)));
        BOOST_TEST((test("    x", wlit('x'), space)));
        BOOST_TEST((test(L"    x", wlit(L'x'), space)));
    }

    {   // lazy strings

        using namespace boost::phoenix;
        std::basic_string<char> s("kimpo");
        BOOST_TEST((test("kimpo", lit(val(s)))));

        std::basic_string<wchar_t> ws(L"kimpo");
        BOOST_TEST((test(L"kimpo", lit(ref(ws)))));
    }

    return boost::report_errors();
}
