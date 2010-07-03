/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    Copyright (c) 2001-2009 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_CHAR_CLASS_NOV_10_2006_0907AM)
#define BOOST_SPIRIT_CHAR_CLASS_NOV_10_2006_0907AM

#include <string>

#include <boost/spirit/home/support/char_class/standard.hpp>
#include <boost/spirit/home/support/char_class/standard_wide.hpp>
#include <boost/spirit/home/support/char_class/ascii.hpp>
#include <boost/spirit/home/support/char_class/iso8859_1.hpp>

#include <boost/config.hpp>
#if defined(BOOST_MSVC)
# pragma warning(push)
# pragma warning(disable: 4800) // 'int' : forcing value to bool 'true' or 'false' warning
#endif

namespace boost { namespace spirit { namespace char_class
{
    namespace tag
    {
        // classification
        struct alnum {};
        struct alpha {};
        struct digit {};
        struct xdigit {};
        struct cntrl {};
        struct graph {};
        struct lower {};
        struct print {};
        struct punct {};
        struct space {};
        struct blank {};
        struct upper {};
    }

    // This composite tag type encodes both the character
    // set and the specific char classification.
    template <typename CharSet, typename CharClass>
    struct key
    {
        typedef CharSet char_set;
        typedef CharClass char_class;
    };

    // This identity tag types encode the character set.
    struct no_case_base_tag {};
    struct lower_case_base_tag {};
    struct upper_case_base_tag {};

    template <typename CharSet>
    struct no_case_tag : no_case_base_tag
    {
        typedef CharSet char_set;
    };

    template <typename CharSet>
    struct lower_case_tag : lower_case_base_tag
    {
        typedef CharSet char_set;
        typedef tag::lower char_class;
    };

    template <typename CharSet>
    struct upper_case_tag : upper_case_base_tag
    {
        typedef CharSet char_set;
        typedef tag::upper char_class;
    };

    // Test characters for classification
    template <typename CharSet>
    struct classify
    {
        typedef typename CharSet::char_type char_type;

        template <typename Char>
        static bool
        is(tag::alnum, Char ch)
        {
            return CharSet::isalnum(char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::alpha, Char ch)
        {
            return CharSet::isalpha(char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::digit, Char ch)
        {
            return CharSet::isdigit(char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::xdigit, Char ch)
        {
            return CharSet::isxdigit(char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::cntrl, Char ch)
        {
            return CharSet::iscntrl(char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::graph, Char ch)
        {
            return CharSet::isgraph(char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::lower, Char ch)
        {
            return CharSet::islower(char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::print, Char ch)
        {
            return CharSet::isprint(char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::punct, Char ch)
        {
            return CharSet::ispunct(char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::space, Char ch)
        {
            return CharSet::isspace(char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::blank, Char ch)
        {
            return CharSet::isblank BOOST_PREVENT_MACRO_SUBSTITUTION (char_type(ch));
        }

        template <typename Char>
        static bool
        is(tag::upper, Char ch)
        {
            return CharSet::isupper(char_type(ch));
        }
    };

    // Convert characters
    template <typename CharSet>
    struct convert
    {
        typedef typename CharSet::char_type char_type;

        template <typename Char>
        static Char
        to(tag::lower, Char ch)
        {
            return CharSet::tolower(char_type(ch));
        }

        template <typename Char>
        static Char
        to(tag::upper, Char ch)
        {
            return CharSet::toupper(char_type(ch));
        }
    };

    // Info on character classification
    template <typename CharSet>
    struct what
    {
        static char const* is(tag::alnum)
        {
            return "alnum";
        }

        static char const* is(tag::alpha)
        {
            return "alpha";
        }

        static char const* is(tag::digit)
        {
            return "digit";
        }

        static char const* is(tag::xdigit)
        {
            return "xdigit";
        }

        static char const* is(tag::cntrl)
        {
            return "cntrl";
        }

        static char const* is(tag::graph)
        {
            return "graph";
        }

        static char const* is(tag::lower)
        {
            return "lower";
        }

        static char const* is(tag::print)
        {
            return "print";
        }

        static char const* is(tag::punct)
        {
            return "punct";
        }

        static char const* is(tag::space)
        {
            return "space";
        }

        static char const* is(tag::blank)
        {
            return "blank";
        }

        static char const* is(tag::upper)
        {
            return "upper";
        }
    };
}}}

#if defined(BOOST_MSVC)
# pragma warning(pop)
#endif

#endif


