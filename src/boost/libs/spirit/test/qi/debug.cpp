/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#define BOOST_SPIRIT_DEBUG

#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_operator.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_string.hpp>
#include <boost/spirit/include/qi_numeric.hpp>
#include <boost/spirit/include/qi_nonterminal.hpp>
#include <boost/spirit/include/qi_action.hpp>
#include <boost/spirit/include/qi_debug.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>

#include <string>
#include <iostream>
#include "test.hpp"

using namespace spirit_test;

int
main()
{
    using namespace boost::spirit;
    using namespace boost::spirit::qi;
    using namespace boost::spirit::ascii;

    { // basic tests
        rule<char const*> a, b, c, start;

        a = 'a';
        b = 'b';
        c = 'c';
        BOOST_SPIRIT_DEBUG_NODE(a);
        BOOST_SPIRIT_DEBUG_NODE(b);
        BOOST_SPIRIT_DEBUG_NODE(c);

        start = *(a | b | c);
        BOOST_SPIRIT_DEBUG_NODE(start);
        BOOST_TEST(test("abcabcacb", start));

        start = (a | b) >> (start | b);
        BOOST_SPIRIT_DEBUG_NODE(start);
        BOOST_TEST(test("aaaabababaaabbb", start));
        BOOST_TEST(test("aaaabababaaabba", start, false));
    }

    { // basic tests w/ skipper

        rule<char const*, space_type> a, b, c, start;

        a = 'a';
        b = 'b';
        c = 'c';
        BOOST_SPIRIT_DEBUG_NODE(a);
        BOOST_SPIRIT_DEBUG_NODE(b);
        BOOST_SPIRIT_DEBUG_NODE(c);

        start = *(a | b | c);
        BOOST_TEST(test(" a b c a b c a c b", start, space));

        // allow no skipping too:
        BOOST_SPIRIT_DEBUG_NODE(start);
        BOOST_TEST(test("abcabcacb", start));

        start = (a | b) >> (start | b);
        BOOST_SPIRIT_DEBUG_NODE(start);
        BOOST_TEST(test(" a a a a b a b a b a a a b b b ", start, space));
        BOOST_TEST(test(" a a a a b a b a b a a a b b a ", start, space, false));
    }

//     { // alias tests
// 
//         rule<char const*> a, b, c, d, start;
// 
//         a = 'a';
//         b = 'b';
//         c = 'c';
//         d = start.alias(); // d will always track start
// 
//         start = *(a | b | c);
//         BOOST_TEST(test("abcabcacb", d));
// 
//         start = (a | b) >> (start | b);
//         BOOST_TEST(test("aaaabababaaabbb", d));
//     }
// 
//     { // copy tests
// 
//         rule<char const*> a, b, c, start;
// 
//         a = 'a';
//         b = 'b';
//         c = 'c';
// 
//         // The FF is the dynamic equivalent of start = *(a | b | c);
//         start = a;
//         start = start.copy() | b;
//         start = start.copy() | c;
//         start = *(start.copy());
// 
//         BOOST_TEST(test("abcabcacb", start));
// 
//         // The FF is the dynamic equivalent of start = (a | b) >> (start | b);
//         start = b;
//         start = a | start.copy();
//         start = start.copy() >> (start | b);
// 
//         BOOST_TEST(test("aaaabababaaabbb", start));
//         BOOST_TEST(test("aaaabababaaabba", start, false));
//     }
// 
//     { // context tests
// 
//         using namespace boost::phoenix;
//         using namespace boost::spirit::ascii;
//         using boost::spirit::arg_names::_1;
//         using boost::spirit::arg_names::_val;
// 
//         char ch;
//         rule<char const*, char()> a;
//         a = alpha[_val = _1];
// 
//         BOOST_TEST(test("x", a[ref(ch) = _1]));
//         BOOST_TEST(ch == 'x');
// 
//         BOOST_TEST(test_attr("z", a, ch)); // attribute is given.
//         BOOST_TEST(ch == 'z');
//     }
// 
//     { // context (w/arg) tests
// 
//         using namespace boost::phoenix;
//         using namespace boost::spirit::ascii;
//         using boost::spirit::arg_names::_1;
//         using boost::spirit::arg_names::_r1;
//         using boost::spirit::arg_names::_r2;
//         using boost::spirit::arg_names::_val;
// 
//         char ch;
//         rule<char const*, char(int)> a; // 1 arg
//         a = alpha[_val = _1 + _r1];
// 
//         BOOST_TEST(test("x", a(val(1))[ref(ch) = _1]));
//         BOOST_TEST(ch == 'x' + 1);
// 
//         BOOST_TEST(test_attr("a", a(1), ch)); // allow scalars as rule args too.
//         BOOST_TEST(ch == 'a' + 1);
// 
//         rule<char const*, char(int, int)> b; // 2 args
//         b = alpha[_val = _1 + _r1 + _r2];
//         BOOST_TEST(test_attr("a", b(1, 2), ch));
//         BOOST_TEST(ch == 'a' + 1 + 2);
//     }
// 
//     { // context (w/locals) tests
//         using namespace boost::phoenix;
//         using namespace boost::spirit::ascii;
//         using boost::spirit::arg_names::_1;
//         using boost::spirit::arg_names::_a;
//         using boost::spirit::char_;
//         using boost::spirit::locals;
// 
//         rule<char const*, locals<char> > a; // 1 local
//         a = alpha[_a = _1] >> char_(_a);
//         BOOST_TEST(test("aa", a));
//         BOOST_TEST(!test("ax", a));
//     }
// 
//     { // context (w/args and locals) tests
//         using namespace boost::phoenix;
//         using namespace boost::spirit::ascii;
//         using boost::spirit::arg_names::_1;
//         using boost::spirit::arg_names::_r1;
//         using boost::spirit::arg_names::_a;
//         using boost::spirit::char_;
//         using boost::spirit::locals;
// 
//         rule<char const*, void(int), locals<char> > a; // 1 arg + 1 local
//         a = alpha[_a = _1 + _r1] >> char_(_a);
//         BOOST_TEST(test("ab", a(val(1))));
//         BOOST_TEST(test("xy", a(val(1))));
//         BOOST_TEST(!test("ax", a(val(1))));
//     }
// 
//     { // bug: test that injected attributes are ok
//         using namespace boost::phoenix;
//         using namespace boost::spirit::ascii;
//         using boost::spirit::arg_names::_1;
//         using boost::spirit::arg_names::_r1;
//         using boost::spirit::arg_names::_val;
//         using boost::spirit::char_;
// 
//         rule<char const*, char(int) > r;
// 
//         // problem code:
//         r = char_(_r1)[_val = _1];
//     }

    { // error handling

        using namespace boost::phoenix;
        using namespace boost::spirit::ascii;
        using boost::phoenix::val;
        using boost::spirit::int_;
        using boost::spirit::arg_names::_4; // what
        using boost::spirit::arg_names::_3; // error pos
        using boost::spirit::arg_names::_2; // end
        using boost::spirit::qi::fail;

        rule<char const*> r;
        r = char_('(') > int_ > ',' > int_ > ')';

        on_error<fail>
        (
            r, std::cout
                << val("Error! Expecting: ")
                << _4
                << val(" Here: \"")
                << construct<std::string>(_3, _2)
                << val("\"")
                << std::endl
        );

        BOOST_SPIRIT_DEBUG_NODE(r);
        BOOST_TEST(test("(123,456)", r));
        BOOST_TEST(!test("(abc,def)", r));
        BOOST_TEST(!test("(123,456]", r));
        BOOST_TEST(!test("(123;456)", r));
        BOOST_TEST(!test("[123,456]", r));
    }

    return boost::report_errors();
}

