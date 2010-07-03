//  Copyright (c) 2001-2008 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/karma_auxiliary.hpp>
#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_string.hpp>
#include <boost/spirit/include/karma_generate.hpp>

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

///////////////////////////////////////////////////////////////////////////////
int
main()
{
    using namespace spirit_test;
    using namespace boost::spirit;
    using namespace boost::spirit::ascii;

    {
        BOOST_TEST((test("<tag>a</tag>", 
            confix("<tag>", "</tag>")[char_('a')])));
        BOOST_TEST((test("// some C++ comment\n", 
            confix(lit("//"), eol)[" some C++ comment"])));
        BOOST_TEST((test("<ol>some text</ol>", html::ol["some text"])));
    }

    {
        BOOST_TEST((test(L"<tag>a</tag>", 
            confix(L"<tag>", L"</tag>")[wchar(L'a')])));
        BOOST_TEST((test(L"// some C++ comment\n", 
            confix(wlit(L"//"), eol)[L" some C++ comment"])));
        BOOST_TEST((test(L"<ol>some text</ol>", html::ol[L"some text"])));
    }

    {
        BOOST_TEST((test_delimited("<tag> a </tag> ", 
            confix("<tag>", "</tag>")[char_('a')], space)));
        BOOST_TEST((test_delimited("// some C++ comment \n ", 
            confix(lit("//"), eol)["some C++ comment"], space)));
        BOOST_TEST((test_delimited("<ol> some text </ol> ", 
            html::ol["some text"], space)));
    }

    {
        BOOST_TEST((test_delimited(L"<tag> a </tag> ", 
            confix(L"<tag>", L"</tag>")[wchar(L'a')], space)));
        BOOST_TEST((test_delimited(L"// some C++ comment \n ", 
            confix(wlit(L"//"), eol)[L"some C++ comment"], space)));
        BOOST_TEST((test_delimited(L"<ol> some text </ol> ", 
            html::ol[L"some text"], space)));
    }

    return boost::report_errors();
}
