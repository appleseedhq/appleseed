//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(BOOST_SPIRIT_KARMA_TEST_FEB_23_2007_1221PM)
#define BOOST_SPIRIT_KARMA_TEST_FEB_23_2007_1221PM

#include <cstring>
#include <string>
#include <iterator>
#include <iostream>
#include <typeinfo>

#include <boost/spirit/include/karma_generate.hpp>
#include <boost/spirit/include/karma_what.hpp>

namespace spirit_test
{
    ///////////////////////////////////////////////////////////////////////////
    struct display_type
    {
        template<typename T>
        void operator()(T const &) const
        {
            std::cout << typeid(T).name() << std::endl;
        }

        template<typename T>
        static void print() 
        {
            std::cout << typeid(T).name() << std::endl;
        }
    };

    display_type const display = {};

    ///////////////////////////////////////////////////////////////////////////
    template <typename Char>
    struct output_iterator
    {
        typedef std::basic_string<Char> string_type;
        typedef std::back_insert_iterator<string_type> type;
    };

    ///////////////////////////////////////////////////////////////////////////
    template <typename Char, typename Generator>
    inline bool test(Char const *expected, Generator const& g)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<Char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate(std::back_inserter(generated), g);

        return result && generated == expected;
    }

    template <typename Char, typename Generator>
    inline bool test(std::basic_string<Char> const& expected, Generator const& g)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<Char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate(std::back_inserter(generated), g);

        return result && generated == expected;
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Char, typename Generator, typename Parameter>
    inline bool test(Char const *expected, Generator const& g, 
        Parameter const &parameter)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<Char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate(std::back_inserter(generated), g, parameter);

        return result && generated == expected;
    }

    template <typename Char, typename Generator, typename Parameter>
    inline bool test(std::basic_string<Char> const& expected, Generator const& g, 
        Parameter const &parameter)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<Char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate(std::back_inserter(generated), g, parameter);

        return result && generated == expected;
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Char, typename Generator, typename Delimiter>
    inline bool test_delimited(Char const *expected, Generator const& g, 
        Delimiter const& d)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<Char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate_delimited(std::back_inserter(generated), g, d);

        return result && generated == expected;
    }

    template <typename Char, typename Generator, typename Delimiter>
    inline bool test_delimited(std::basic_string<Char> const& expected, 
        Generator const& g, Delimiter const& d)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<Char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate_delimited(std::back_inserter(generated), g, d);

        return result && generated == expected;
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Char, typename Generator, typename Parameter,
        typename Delimiter>
    inline bool test_delimited(Char const *expected, Generator const& g, 
        Parameter const &parameter, Delimiter const& d)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<Char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate_delimited(std::back_inserter(generated), 
            g, parameter, d);

        return result && generated == expected;
    }

    template <typename Char, typename Generator, typename Parameter,
        typename Delimiter>
    inline bool test_delimited(std::basic_string<Char> const& expected, 
        Generator const& g, Parameter const &parameter, Delimiter const& d)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<Char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate_delimited(std::back_inserter(generated), 
            g, parameter, d);

        return result && generated == expected;
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Generator>
    inline bool 
    binary_test(char const *expected, std::size_t size, 
        Generator const& g)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate(std::back_inserter(generated), g);

        return result && !std::memcmp(generated.c_str(), expected, size);
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Generator, typename Parameter>
    inline bool 
    binary_test(char const *expected, std::size_t size, 
        Generator const& g, Parameter const &parameter)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate(std::back_inserter(generated), g, parameter);

        return result && !std::memcmp(generated.c_str(), expected, size);
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Generator, typename Delimiter>
    inline bool 
    binary_test_delimited(char const *expected, std::size_t size, 
        Generator const& g, Delimiter const& d)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate_delimited(std::back_inserter(generated), 
            g, d);

        return result && !std::memcmp(generated.c_str(), expected, size);
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Generator, typename Parameter, typename Delimiter>
    inline bool 
    binary_test_delimited(char const *expected, std::size_t size, 
        Generator const& g, Parameter const &parameter, Delimiter const& d)
    {
        namespace karma = boost::spirit::karma;
        typedef std::basic_string<char> string_type;

        // we don't care about the result of the "what" function.
        // we only care that all generators have it:
        karma::what(g);

        string_type generated;
        bool result = karma::generate_delimited(std::back_inserter(generated), 
            g, parameter, d);

        return result && !std::memcmp(generated.c_str(), expected, size);
    }

}   // namespace spirit_test

#endif // !BOOST_SPIRIT_KARMA_TEST_FEB_23_2007_1221PM
