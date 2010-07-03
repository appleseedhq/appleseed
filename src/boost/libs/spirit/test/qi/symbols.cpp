/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_string.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_action.hpp>
#include <boost/spirit/include/qi_directive.hpp>
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
    using namespace boost::spirit::qi;

    { // basics
        symbols<char, int> sym;

        sym.add
            ("Joel")
            ("Ruby")
            ("Tenji")
            ("Tutit")
            ("Kim")
            ("Joey")
        ;

        BOOST_TEST((test("Joel", sym)));
        BOOST_TEST((test("Ruby", sym)));
        BOOST_TEST((test("Tenji", sym)));
        BOOST_TEST((test("Tutit", sym)));
        BOOST_TEST((test("Kim", sym)));
        BOOST_TEST((test("Joey", sym)));
        BOOST_TEST((!test("XXX", sym)));

        sym.remove
            ("Joel")
            ("Ruby")
        ;

        BOOST_TEST((!test("Joel", sym)));
        BOOST_TEST((!test("Ruby", sym)));
    }

    { // comma syntax
        symbols<char, int> sym;
        sym += "Joel", "Ruby", "Tenji", "Tutit", "Kim", "Joey";

        BOOST_TEST((test("Joel", sym)));
        BOOST_TEST((test("Ruby", sym)));
        BOOST_TEST((test("Tenji", sym)));
        BOOST_TEST((test("Tutit", sym)));
        BOOST_TEST((test("Kim", sym)));
        BOOST_TEST((test("Joey", sym)));
        BOOST_TEST((!test("XXX", sym)));

        sym -= "Joel", "Ruby";

        BOOST_TEST((!test("Joel", sym)));
        BOOST_TEST((!test("Ruby", sym)));
    }

    { // no-case handling
        using namespace boost::spirit::ascii;

        symbols<char, int> sym;
        // NOTE: make sure all entries are in lower-case!!!
        sym = "joel", "ruby", "tenji", "tutit", "kim", "joey";

        BOOST_TEST((test("joel", no_case[sym])));
        BOOST_TEST((test("ruby", no_case[sym])));
        BOOST_TEST((test("tenji", no_case[sym])));
        BOOST_TEST((test("tutit", no_case[sym])));
        BOOST_TEST((test("kim", no_case[sym])));
        BOOST_TEST((test("joey", no_case[sym])));

        BOOST_TEST((test("JOEL", no_case[sym])));
        BOOST_TEST((test("RUBY", no_case[sym])));
        BOOST_TEST((test("TENJI", no_case[sym])));
        BOOST_TEST((test("TUTIT", no_case[sym])));
        BOOST_TEST((test("KIM", no_case[sym])));
        BOOST_TEST((test("JOEY", no_case[sym])));
    }

    { // attributes
        symbols<char, int> sym;

        sym.add
            ("Joel", 1)
            ("Ruby", 2)
            ("Tenji", 3)
            ("Tutit", 4)
            ("Kim", 5)
            ("Joey", 6)
        ;

        int i;
        BOOST_TEST((test_attr("Joel", sym, i)));
        BOOST_TEST(i == 1);
        BOOST_TEST((test_attr("Ruby", sym, i)));
        BOOST_TEST(i == 2);
        BOOST_TEST((test_attr("Tenji", sym, i)));
        BOOST_TEST(i == 3);
        BOOST_TEST((test_attr("Tutit", sym, i)));
        BOOST_TEST(i == 4);
        BOOST_TEST((test_attr("Kim", sym, i)));
        BOOST_TEST(i == 5);
        BOOST_TEST((test_attr("Joey", sym, i)));
        BOOST_TEST(i == 6);
        BOOST_TEST((!test_attr("XXX", sym, i)));
    }

    { // actions
        using namespace boost::phoenix;
        using boost::spirit::arg_names::_1;

        symbols<char, int> sym;
        sym.add
            ("Joel", 1)
            ("Ruby", 2)
            ("Tenji", 3)
            ("Tutit", 4)
            ("Kim", 5)
            ("Joey", 6)
        ;

        int i;
        BOOST_TEST((test("Joel", sym[ref(i) = _1])));
        BOOST_TEST(i == 1);
        BOOST_TEST((test("Ruby", sym[ref(i) = _1])));
        BOOST_TEST(i == 2);
        BOOST_TEST((test("Tenji", sym[ref(i) = _1])));
        BOOST_TEST(i == 3);
        BOOST_TEST((test("Tutit", sym[ref(i) = _1])));
        BOOST_TEST(i == 4);
        BOOST_TEST((test("Kim", sym[ref(i) = _1])));
        BOOST_TEST(i == 5);
        BOOST_TEST((test("Joey", sym[ref(i) = _1])));
        BOOST_TEST(i == 6);
        BOOST_TEST((!test("XXX", sym[ref(i) = _1])));
    }

    { // construction from symbol array
        char const* syms[] = {"Joel","Ruby","Tenji","Tutit","Kim","Joey"};
        symbols<char, int> sym(syms);

        BOOST_TEST((test("Joel", sym)));
        BOOST_TEST((test("Ruby", sym)));
        BOOST_TEST((test("Tenji", sym)));
        BOOST_TEST((test("Tutit", sym)));
        BOOST_TEST((test("Kim", sym)));
        BOOST_TEST((test("Joey", sym)));
        BOOST_TEST((!test("XXX", sym)));
    }

    { // construction from 2 arrays

        char const* syms[] = {"Joel","Ruby","Tenji","Tutit","Kim","Joey"};
        int data[] = {1,2,3,4,5,6};
        symbols<char, int> sym(syms, data);

        int i;
        BOOST_TEST((test_attr("Joel", sym, i)));
        BOOST_TEST(i == 1);
        BOOST_TEST((test_attr("Ruby", sym, i)));
        BOOST_TEST(i == 2);
        BOOST_TEST((test_attr("Tenji", sym, i)));
        BOOST_TEST(i == 3);
        BOOST_TEST((test_attr("Tutit", sym, i)));
        BOOST_TEST(i == 4);
        BOOST_TEST((test_attr("Kim", sym, i)));
        BOOST_TEST(i == 5);
        BOOST_TEST((test_attr("Joey", sym, i)));
        BOOST_TEST(i == 6);
        BOOST_TEST((!test_attr("XXX", sym, i)));
    }

    return boost::report_errors();
}
