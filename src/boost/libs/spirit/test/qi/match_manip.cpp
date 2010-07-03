//  Copyright (c) 2001-2009 Hartmut Kaiser
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_stream.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>

#include <string>
#include <sstream>
#include <vector>
#include <list>

#include <boost/static_assert.hpp>
#include <boost/detail/lightweight_test.hpp>

///////////////////////////////////////////////////////////////////////////////
template <typename Char, typename Expr>
bool test(Char const *toparse, Expr const& xpr)
{
    namespace spirit = boost::spirit;
    typedef
        spirit::traits::is_component<spirit::qi::domain, Expr>
    is_component;

    // report invalid expression error as early as possible
    BOOST_MPL_ASSERT_MSG(is_component::value,
        xpr_is_not_convertible_to_a_parser, ());

    typedef
        typename spirit::result_of::as_component<spirit::qi::domain, Expr>::type
    component;
    typedef typename component::director director;

    component c = spirit::as_component(spirit::qi::domain(), xpr);

    std::istringstream istrm(toparse);
    istrm >> c;
    return istrm.good() || istrm.eof();
}

template <typename Char, typename Expr, typename Attribute, typename Skipper>
bool test(Char const *toparse,
    boost::spirit::qi::detail::match_manip<Expr, Attribute, Skipper> const& mm)
{
    std::istringstream istrm(toparse);
    istrm >> mm;
    return istrm.good() || istrm.eof();
}

///////////////////////////////////////////////////////////////////////////////
bool is_list_ok(std::list<char> const& l)
{
    std::list<char>::const_iterator cit = l.begin();
    if (cit == l.end() || *cit != 'a')
        return false;
    if (++cit == l.end() || *cit != 'b')
        return false;

    return ++cit != l.end() && *cit == 'c';
}

///////////////////////////////////////////////////////////////////////////////
int
main()
{
    using namespace boost::spirit;
    using namespace boost::spirit::ascii;
    using namespace boost::spirit::arg_names;
    using namespace boost::spirit::qi;

    namespace fusion = boost::fusion;
    using namespace boost::phoenix;

    {
        char c = '\0';
        BOOST_TEST(test( "a",
            char_[ref(c) = _1]
        ) && c == 'a');

        c = '\0';
        BOOST_TEST(test( "a",
            match(char_[ref(c) = _1])
        ) && c == 'a');

        c = '\0';
        BOOST_TEST(test( " a",
            phrase_match(char_[ref(c) = _1], space)
        ) && c == 'a');

        c = '\0';
        BOOST_TEST(test( "a",
            match(char_, c)
        ) && c == 'a');

        c = '\0';
        BOOST_TEST(test( " a",
            phrase_match(char_, c, space)
        ) && c == 'a');
    }

    {
        ///////////////////////////////////////////////////////////////////////
        typedef typed_stream<char> char_stream_type;
        char_stream_type const char_stream = char_stream_type();

        typedef typed_stream<int> int_stream_type;
        int_stream_type const int_stream = int_stream_type();

        ///////////////////////////////////////////////////////////////////////
        char c = '\0';
        BOOST_TEST(test( "a",
            char_stream[ref(c) = _1]
        ) && c == 'a');

        c = '\0';
        BOOST_TEST(test( "a",
            match(char_stream[ref(c) = _1])
        ) && c == 'a');

        c = '\0';
        BOOST_TEST(test( " a",
            phrase_match(char_stream[ref(c) = _1], space)
        ) && c == 'a');

        int i = 0;
        BOOST_TEST(test( "42",
            int_stream[ref(i) = _1]
        ) && i == 42);

        i = 0;
        BOOST_TEST(test( "42",
            match(int_stream[ref(i) = _1])
        ) && i == 42);

        i = 0;
        BOOST_TEST(test( " 42",
            phrase_match(int_stream[ref(i) = _1], space)
        ) && i == 42);

        ///////////////////////////////////////////////////////////////////////
        c = '\0';
        BOOST_TEST(test( "a",
            match(stream, c)
        ) && c == 'a');

        c = '\0';
        BOOST_TEST(test( " a",
            phrase_match(stream, c, space)
        ) && c == 'a');

        i = 0;
        BOOST_TEST(test( "42",
            match(stream, i)
        ) && i == 42);

        i = 0;
        BOOST_TEST(test( " 42",
            phrase_match(stream, i, space)
        ) && i == 42);
    }

    {
        char a = '\0', b = '\0';
        BOOST_TEST(test( "ab",
            char_[ref(a) = _1] >> char_[ref(b) = _1]
        ) && a == 'a' && b == 'b');

        a = '\0', b = '\0';
        BOOST_TEST(test( "ab",
            match(char_[ref(a) = _1] >> char_[ref(b) = _1])
        ) && a == 'a' && b == 'b');

        a = '\0', b = '\0';
        BOOST_TEST(test( " a b",
            phrase_match(char_[ref(a) = _1] >> char_[ref(b) = _1], space)
        ) && a == 'a' && b == 'b');

        fusion::vector<char, char> t;
        BOOST_TEST(test( "ab",
            match(char_ >> char_, t)
        ) && fusion::at_c<0>(t) == 'a' && fusion::at_c<1>(t) == 'b');

        t = fusion::vector<char, char>();
        BOOST_TEST(test( " a b",
            phrase_match(char_ >> char_, t, space)
        ) && fusion::at_c<0>(t) == 'a' && fusion::at_c<1>(t) == 'b');
    }

    {
        char a = '\0', b = '\0', c = '\0';
        BOOST_TEST(test( "abc",
            char_[ref(a) = _1] >> char_[ref(b) = _1] >> char_[ref(c) = _1]
        ) && a == 'a' && b == 'b' && c == 'c');

        BOOST_TEST(test( "abc",
            match(char_('a') >> char_('b') >> char_('c'))
        ));

        BOOST_TEST(test( " a b c",
            phrase_match(char_('a') >> char_('b') >> char_('c'), space)
        ));

        BOOST_TEST(!test( "abc",
            match(char_('a') >> char_('b') >> char_('d'))
        ));

        BOOST_TEST(!test( " a b c",
            phrase_match(char_('a') >> char_('b') >> char_('d'), space)
        ));

        fusion::vector<char, char, char> t;
        BOOST_TEST(test( "abc",
            match(char_ >> char_ >> char_, t)
        ) && fusion::at_c<0>(t) == 'a' && fusion::at_c<1>(t) == 'b' && fusion::at_c<2>(t) == 'c');

        t = fusion::vector<char, char, char>();
        BOOST_TEST(test( " a b c",
            phrase_match(char_ >> char_ >> char_, t, space)
        ) && fusion::at_c<0>(t) == 'a' && fusion::at_c<1>(t) == 'b' && fusion::at_c<2>(t) == 'c');
    }

    {
        char a = '\0';
        int i = 0;
        BOOST_TEST(test( "a2",
            (char_ >> int_)[ref(a) = _1, ref(i) = _2]
        ) && a == 'a' && i == 2);

        fusion::vector<char, int> t;
        BOOST_TEST(test( "a2",
            match(char_ >> int_, t)
        ) && fusion::at_c<0>(t) == 'a' && fusion::at_c<1>(t) == 2);

        t = fusion::vector<char, int>();
        BOOST_TEST(test( " a 2",
            phrase_match(char_ >> int_, t, space)
        ) && fusion::at_c<0>(t) == 'a' && fusion::at_c<1>(t) == 2);

        BOOST_TEST(!test( "a2",
            match(char_ >> alpha, t)
        ));
        BOOST_TEST(!test( " a 2",
            phrase_match(char_ >> alpha, t, space)
        ));
    }

    {
        // output all elements of a vector
        std::vector<char> v;
        BOOST_TEST(test( "abc",
            (*char_)[ref(v) = _1]
        ) && 3 == v.size() && v[0] == 'a' && v[1] == 'b' && v[2] == 'c');

        v.clear();
        BOOST_TEST(test( "abc",
            match(*char_, v)
        ) && 3 == v.size() && v[0] == 'a' && v[1] == 'b' && v[2] == 'c');

        v.clear();
        BOOST_TEST(test( " a b c",
            phrase_match(*char_, v, space)
        ) && 3 == v.size() && v[0] == 'a' && v[1] == 'b' && v[2] == 'c');

        // parse a comma separated list of vector elements
        v.clear();
        BOOST_TEST(test( "a,b,c",
            match(char_ % ',', v)
        ) && 3 == v.size() && v[0] == 'a' && v[1] == 'b' && v[2] == 'c');

        v.clear();
        BOOST_TEST(test( " a , b , c",
            phrase_match(char_ % ',', v, space)
        ) && 3 == v.size() && v[0] == 'a' && v[1] == 'b' && v[2] == 'c');

        // output all elements of a list
        std::list<char> l;
        BOOST_TEST(test( "abc",
            match(*char_, l)
        ) && 3 == l.size() && is_list_ok(l));

        l.clear();
        BOOST_TEST(test( " a b c",
            phrase_match(*char_, l, space)
        ) && 3 == l.size() && is_list_ok(l));
    }

    return boost::report_errors();
}

