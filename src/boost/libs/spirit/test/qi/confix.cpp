/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    Copyright (c) 2001-2008 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_string.hpp>
#include <boost/spirit/include/qi_auxiliary.hpp>
#include <boost/spirit/include/qi_numeric.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_parse.hpp>
#include <boost/spirit/include/support_argument.hpp>

#include <iostream>
#include "test.hpp"

namespace html
{
    ///////////////////////////////////////////////////////////////////////////////
    //  define a HTML tag helper generator
    template <typename Char, typename Traits, typename Allocator>
    inline boost::spirit::confix_spec<std::basic_string<Char, Traits, Allocator> >
    tag (std::basic_string<Char, Traits, Allocator> const& tagname)
    {
        typedef std::basic_string<Char, Traits, Allocator> string_type;
        return boost::spirit::confix_spec<string_type>(
            string_type("<") + tagname + ">", string_type("</") + tagname + ">");
    }

    inline boost::spirit::confix_spec<std::string>
    tag (char const* tagname)
    {
        return tag(std::string(tagname));
    }

    typedef boost::spirit::confix_spec<std::string> html_tag_type;

    html_tag_type const ol = tag("ol");
}

int
main()
{
    using spirit_test::test;
    using namespace boost::spirit;
    using namespace boost::spirit::ascii;

    {
        BOOST_TEST(test("<tag>10</tag>", confix("<tag>", "</tag>")[int_]));
        BOOST_TEST(test(L"<tag>10</tag>", confix(L"<tag>", L"</tag>")[int_]));
        BOOST_TEST(test("//10\n", confix(lit("//"), eol)[int_]));
        BOOST_TEST(test(L"//10\n", confix(wlit("//"), eol)[int_]));
        BOOST_TEST(test("<ol>10</ol>", html::ol[int_]));
        BOOST_TEST(test(L"<ol>10</ol>", html::ol[int_]));
    }

    {
        BOOST_TEST(test("   <tag> 10   </tag>", confix("<tag>", "</tag>")[int_], 
            ascii::space));
        BOOST_TEST(test(L" <tag>   10 </tag>", confix(L"<tag>", L"</tag>")[int_], 
            ascii::space));
        BOOST_TEST(test("// 10  \n", confix(lit("//"), eol)[int_], ascii::space));
        BOOST_TEST(test(L"// 10  \n", confix(wlit("//"), eol)[int_], ascii::space));
        BOOST_TEST(test("   <ol> 10   </ol>", html::ol[int_], ascii::space));
        BOOST_TEST(test(L" <ol> 10   </ol>", html::ol[int_], ascii::space));
    }

    return boost::report_errors();
}
