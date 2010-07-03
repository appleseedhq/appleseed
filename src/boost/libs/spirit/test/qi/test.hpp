/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_TEST_FEB_01_2007_0605PM)
#define BOOST_SPIRIT_TEST_FEB_01_2007_0605PM

#include <boost/spirit/include/qi_parse.hpp>
#include <boost/spirit/include/qi_what.hpp>

namespace spirit_test
{
    template <typename Char, typename Parser>
    bool test(Char const* in, Parser const& p, bool full_match = true)
    {
        // we don't care about the result of the "what" function.
        // we only care that all parsers have it:
        boost::spirit::qi::what(p);

        Char const* last = in;
        while (*last)
            last++;
        return boost::spirit::qi::parse(in, last, p)
            && (!full_match || (in == last));
    }

    template <typename Char, typename Parser, typename Skipper>
    bool test(Char const* in, Parser const& p
        , Skipper const& s, bool full_match = true)
    {
        // we don't care about the result of the "what" function.
        // we only care that all parsers have it:
        boost::spirit::qi::what(p);

        Char const* last = in;
        while (*last)
            last++;
        return boost::spirit::qi::phrase_parse(in, last, p, s)
            && (!full_match || (in == last));
    }

    template <typename Char, typename Parser, typename Attr>
    bool test_attr(Char const* in, Parser const& p
        , Attr& attr, bool full_match = true)
    {
        // we don't care about the result of the "what" function.
        // we only care that all parsers have it:
        boost::spirit::qi::what(p);

        Char const* last = in;
        while (*last)
            last++;
        return boost::spirit::qi::parse(in, last, p, attr)
            && (!full_match || (in == last));
    }

    template <typename Char, typename Parser, typename Attr, typename Skipper>
    bool test_attr(Char const* in, Parser const& p
        , Attr& attr, Skipper const& s, bool full_match = true)
    {
        // we don't care about the result of the "what" function.
        // we only care that all parsers have it:
        boost::spirit::qi::what(p);

        Char const* last = in;
        while (*last)
            last++;
        return boost::spirit::qi::phrase_parse(in, last, p, attr, s)
            && (!full_match || (in == last));
    }
}

#endif
