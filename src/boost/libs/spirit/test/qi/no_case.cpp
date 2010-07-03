/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    http://spirit.sourceforge.net/

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_string.hpp>
#include <boost/spirit/include/qi_directive.hpp>

#include <iostream>
#include "test.hpp"

int
main()
{
    using spirit_test::test;
    using namespace boost::spirit;

    {
        using namespace boost::spirit::ascii;
        BOOST_TEST(test("x", no_case[char_]));
        BOOST_TEST(test("X", no_case[char_('x')]));
        BOOST_TEST(test("X", no_case[char_('X')]));
        BOOST_TEST(test("x", no_case[char_('X')]));
        BOOST_TEST(test("x", no_case[char_('x')]));
        BOOST_TEST(!test("z", no_case[char_('X')]));
        BOOST_TEST(!test("z", no_case[char_('x')]));
        BOOST_TEST(test("x", no_case[char_('a', 'z')]));
        BOOST_TEST(test("X", no_case[char_('a', 'z')]));
        BOOST_TEST(!test("a", no_case[char_('b', 'z')]));
        BOOST_TEST(!test("z", no_case[char_('a', 'y')]));
    }

    {
        using namespace boost::spirit::ascii;
        BOOST_TEST(test("Bochi Bochi", no_case[lit("bochi bochi")]));
        BOOST_TEST(test("BOCHI BOCHI", no_case[lit("bochi bochi")]));
        BOOST_TEST(!test("Vavoo", no_case[lit("bochi bochi")]));
    }

    {
        // should work!
        using namespace boost::spirit::ascii;
        BOOST_TEST(test("x", no_case[no_case[char_]]));
        BOOST_TEST(test("x", no_case[no_case[char_('x')]]));
        BOOST_TEST(test("yabadabadoo", no_case[no_case[lit("Yabadabadoo")]]));
    }

    {
        using namespace boost::spirit::ascii;
        BOOST_TEST(test("X", no_case[alnum]));
        BOOST_TEST(test("6", no_case[alnum]));
        BOOST_TEST(!test(":", no_case[alnum]));

        BOOST_TEST(test("X", no_case[lower]));
        BOOST_TEST(test("x", no_case[lower]));
        BOOST_TEST(test("X", no_case[upper]));
        BOOST_TEST(test("x", no_case[upper]));
        BOOST_TEST(!test(":", no_case[lower]));
        BOOST_TEST(!test(":", no_case[upper]));
    }

    {
        using namespace boost::spirit::iso8859_1;
        BOOST_TEST(test("X", no_case[alnum]));
        BOOST_TEST(test("6", no_case[alnum]));
        BOOST_TEST(!test(":", no_case[alnum]));

        BOOST_TEST(test("X", no_case[lower]));
        BOOST_TEST(test("x", no_case[lower]));
        BOOST_TEST(test("X", no_case[upper]));
        BOOST_TEST(test("x", no_case[upper]));
        BOOST_TEST(!test(":", no_case[lower]));
        BOOST_TEST(!test(":", no_case[upper]));
    }

    {
        using namespace boost::spirit::standard;
        BOOST_TEST(test("X", no_case[alnum]));
        BOOST_TEST(test("6", no_case[alnum]));
        BOOST_TEST(!test(":", no_case[alnum]));

        BOOST_TEST(test("X", no_case[lower]));
        BOOST_TEST(test("x", no_case[lower]));
        BOOST_TEST(test("X", no_case[upper]));
        BOOST_TEST(test("x", no_case[upper]));
        BOOST_TEST(!test(":", no_case[lower]));
        BOOST_TEST(!test(":", no_case[upper]));
    }

    {
        using namespace boost::spirit::standard;
        // chsets
        BOOST_TEST(test("x", no_case[char_("a-z")]));
        BOOST_TEST(test("X", no_case[char_("a-z")]));
        BOOST_TEST(test(L"X", no_case[wchar(L"a-z")]));
        BOOST_TEST(test(L"X", no_case[wchar(L"X")]));
    }

    return boost::report_errors();
}
