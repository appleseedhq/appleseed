/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    Copyright (c) 2001-2008 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_auxiliary.hpp>
#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <iostream>
#include "test.hpp"

int
main()
{
    using spirit_test::test;
    using namespace boost::spirit::qi;
    using namespace boost::spirit::ascii;
    using boost::spirit::char_;
    using boost::spirit::wchar;
    using boost::spirit::eol;
    using boost::spirit::eoi;
    
    {   // eol
        BOOST_TEST(test("\r", eol));
        BOOST_TEST(test("\r\n", eol));
        BOOST_TEST(test("\n", eol));
        BOOST_TEST(!test("\b", eol));

        BOOST_TEST(!test("\r", ~eol, false));
        BOOST_TEST(!test("\r\n", ~eol, false));
        BOOST_TEST(!test("\n", ~eol, false));
        BOOST_TEST(test("\b", ~eol, false));

        BOOST_TEST(test("\r", ~~eol));
        BOOST_TEST(test("\r\n", ~~eol));
        BOOST_TEST(test("\n", ~~eol));
        BOOST_TEST(!test("\b", ~~eol));

        BOOST_TEST(test("   \r", eol, char_(' ')));
        BOOST_TEST(test("   \r\n", eol, char_(' ')));
        BOOST_TEST(test("   \n", eol, char_(' ')));
        BOOST_TEST(!test("   \b", eol, char_(' ')));

        BOOST_TEST(test(L"\r", eol));
        BOOST_TEST(test(L"\r\n", eol));
        BOOST_TEST(test(L"\n", eol));
        BOOST_TEST(!test(L"\b", eol));

        BOOST_TEST(test(L"   \r", eol, wchar(L' ')));
        BOOST_TEST(test(L"   \r\n", eol, wchar(L' ')));
        BOOST_TEST(test(L"   \n", eol, wchar(L' ')));
        BOOST_TEST(!test(L"   \b", eol, wchar(L' ')));
    }
    
    {   // eoi
        BOOST_TEST(test("", eoi));
        BOOST_TEST(!test("a", eoi));
        BOOST_TEST(test("a", ~eoi, false));
        BOOST_TEST(!test("", ~eoi));
        BOOST_TEST(test("", ~~eoi));
        BOOST_TEST(!test("a", ~~eoi));

        BOOST_TEST(test("   ", eoi, space));
        BOOST_TEST(!test("   a", eoi, space));
        BOOST_TEST(test("   a", ~eoi, space, false));
        BOOST_TEST(!test("   ", ~eoi, space));
        BOOST_TEST(test("   ", ~~eoi, space));
        BOOST_TEST(!test("   a", ~~eoi, space));
    }
    
    return boost::report_errors();
}
