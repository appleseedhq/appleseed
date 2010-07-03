/*=============================================================================
    Copyright (c) 2006 Joel de Guzman
    http://spirit.sourceforge.net/

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_QUICKBOOK_CODE_SNIPPET_HPP)
#define BOOST_SPIRIT_QUICKBOOK_CODE_SNIPPET_HPP

#include <boost/spirit/include/classic_core.hpp>
#include <boost/spirit/include/classic_actor.hpp>
#include "./detail/template_stack.hpp"

namespace quickbook
{
    struct cpp_code_snippet_grammar
        : grammar<cpp_code_snippet_grammar>
    {
        cpp_code_snippet_grammar(std::vector<template_symbol>& storage, std::string const& doc_id)
            : storage(storage)
            , doc_id(doc_id)
        {}

        template <typename Scanner>
        struct definition
        {
            definition(cpp_code_snippet_grammar const& self)
            {
                typedef cpp_code_snippet_grammar self_type;
                start_ =
                    +(
                            snippet                 [boost::bind(&self_type::compile, &self, _1, _2)]
                        |   anychar_p
                    )
                    ;

                identifier =
                    (alpha_p | '_') >> *(alnum_p | '_')
                    ;

                snippet =
                    "//[" >> *space_p
                    >> identifier                   [assign_a(self.id)]
                    >> (*(code_elements - "//]"))
                    >> "//]"
                    ;

                code_elements =
                        escaped_comment
                    |   ignore
                    |   line_callout
                    |   inline_callout
                    |   (anychar_p - "//]")         [boost::bind(&self_type::pass_thru, &self, _1, _2)]
                    ;

                inline_callout =
                    "/*<"
                    >> (*(anychar_p - ">*/"))       [boost::bind(&self_type::inline_callout, &self, _1, _2)]
                    >> ">*/"
                    ;

                line_callout =
                    "/*<<"
                    >> (*(anychar_p - ">>*/"))      [boost::bind(&self_type::line_callout, &self, _1, _2)]
                    >> ">>*/"
                    >> *space_p
                    ;

                ignore =
                        *blank_p >> "//<-"
                        >> (*(anychar_p - "//->"))
                        >> "//->" >> *blank_p >> eol_p
                    |   "/*<-*/"
                        >> (*(anychar_p - "/*->*/"))
                        >> "/*->*/"
                    |   "/*<-"
                        >> (*(anychar_p - "->*/"))
                        >> "->*/"
                    ;

                escaped_comment =
                        *space_p >> "//`"
                        >> ((*(anychar_p - eol_p))
                            >> eol_p)               [boost::bind(&self_type::escaped_comment, &self, _1, _2)]
                    |   *space_p >> "/*`"
                        >> (*(anychar_p - "*/"))    [boost::bind(&self_type::escaped_comment, &self, _1, _2)]
                        >> "*/"
                    ;
            }

            rule<Scanner>
                start_, snippet, identifier, code_elements, escaped_comment,
                inline_callout, line_callout, ignore;

            rule<Scanner> const&
            start() const { return start_; }
        };

        void pass_thru(iterator first, iterator last) const;
        void escaped_comment(iterator first, iterator last) const;
        void compile(iterator first, iterator last) const;
        void callout(iterator first, iterator last, char const* role) const;
        void inline_callout(iterator first, iterator last) const;
        void line_callout(iterator first, iterator last) const;

        mutable std::string code;
        mutable std::string snippet;
        mutable std::string id;
        mutable std::vector<std::string> callouts;
        std::vector<template_symbol>& storage;
        std::string doc_id;
    };
}

#endif // BOOST_SPIRIT_QUICKBOOK_CODE_SNIPPET_HPP

