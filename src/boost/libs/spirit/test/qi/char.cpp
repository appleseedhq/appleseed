/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    Copyright (c) 2001-2009 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_string.hpp>
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
    
    {
        BOOST_TEST(test("x", 'x'));
        BOOST_TEST(test(L"x", L'x'));

        BOOST_TEST(test("x", char_));
        BOOST_TEST(test("x", char_('x')));
        BOOST_TEST(!test("x", char_('y')));
        BOOST_TEST(test(L"x", char_(L'x')));
        BOOST_TEST(!test(L"x", char_(L'y')));
        BOOST_TEST(test("x", char_('a', 'z')));
        BOOST_TEST(!test("x", char_('0', '9')));

        BOOST_TEST(!test("x", ~char_));
        BOOST_TEST(!test("x", ~char_('x')));
        BOOST_TEST(test(" ", ~char_('x')));
        BOOST_TEST(test("X", ~char_('x')));
        BOOST_TEST(!test("x", ~char_('b', 'y')));
        BOOST_TEST(test("a", ~char_('b', 'y')));
        BOOST_TEST(test("z", ~char_('b', 'y')));

        BOOST_TEST(test("x", ~~char_));
        BOOST_TEST(test("x", ~~char_('x')));
        BOOST_TEST(!test(" ", ~~char_('x')));
        BOOST_TEST(!test("X", ~~char_('x')));
        BOOST_TEST(test("x", ~~char_('b', 'y')));
        BOOST_TEST(!test("a", ~~char_('b', 'y')));
        BOOST_TEST(!test("z", ~~char_('b', 'y')));
    }

    {
        BOOST_TEST(test("   x", 'x', space));
        BOOST_TEST(test(L"   x", L'x', space));

        BOOST_TEST(test("   x", char_, space));
        BOOST_TEST(test("   x", char_('x'), space));
        BOOST_TEST(!test("   x", char_('y'), space));
        BOOST_TEST(test(L"   x", char_(L'x'), space));
        BOOST_TEST(!test(L"   x", char_(L'y'), space));
        BOOST_TEST(test("   x", char_('a', 'z'), space));
        BOOST_TEST(!test("   x", char_('0', '9'), space));

    }

    {
        BOOST_TEST(test(L"x", wchar));
        BOOST_TEST(test(L"x", wchar(L'x')));
        BOOST_TEST(!test(L"x", wchar(L'y')));
        BOOST_TEST(test(L"x", wchar('a', 'z')));
        BOOST_TEST(!test(L"x", wchar('0', '9')));

        BOOST_TEST(!test(L"x", ~wchar));
        BOOST_TEST(!test(L"x", ~wchar('x')));
        BOOST_TEST(test(L" ", ~wchar('x')));
        BOOST_TEST(test(L"X", ~wchar('x')));
        BOOST_TEST(!test(L"x", ~wchar('b', 'y')));
        BOOST_TEST(test(L"a", ~wchar('b', 'y')));
        BOOST_TEST(test(L"z", ~wchar('b', 'y')));

        BOOST_TEST(test(L"x", ~~wchar));
        BOOST_TEST(test(L"x", ~~wchar('x')));
        BOOST_TEST(!test(L" ", ~~wchar('x')));
        BOOST_TEST(!test(L"X", ~~wchar('x')));
        BOOST_TEST(test(L"x", ~~wchar('b', 'y')));
        BOOST_TEST(!test(L"a", ~~wchar('b', 'y')));
        BOOST_TEST(!test(L"z", ~~wchar('b', 'y')));
    }


    {   // single char strings as argument to char_
        BOOST_TEST(test("x", char_("x"), space));
        BOOST_TEST(test("x", wchar(L"x"), space));
    }

    {
        // chsets
        BOOST_TEST(test("x", char_("a-z")));
        BOOST_TEST(!test("1", char_("a-z")));
        BOOST_TEST(test("1", char_("a-z0-9")));

        BOOST_TEST(test("x", char_(L"a-z")));
        BOOST_TEST(!test("1", char_(L"a-z")));
        BOOST_TEST(test("1", char_(L"a-z0-9")));
    }

    {   // lazy chars

        using namespace boost::phoenix;
        BOOST_TEST((test("x", char_(val('x')))));
        BOOST_TEST((test("h", char_(val('a'), val('n')))));
    }
    
    return boost::report_errors();
}
