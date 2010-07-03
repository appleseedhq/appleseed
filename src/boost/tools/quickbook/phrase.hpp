/*=============================================================================
    Copyright (c) 2002 2004 2006 Joel de Guzman
    Copyright (c) 2004 Eric Niebler
    http://spirit.sourceforge.net/

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_QUICKBOOK_PHRASE_HPP)
#define BOOST_SPIRIT_QUICKBOOK_PHRASE_HPP

#include "detail/utils.hpp"
#include <boost/spirit/include/classic_core.hpp>
#include <boost/spirit/include/classic_confix.hpp>
#include <boost/spirit/include/classic_chset.hpp>
#include <boost/spirit/include/classic_assign_actor.hpp>
#include <boost/spirit/include/classic_if.hpp>

namespace quickbook
{
    using namespace boost::spirit::classic;

    template <typename Rule, typename Action>
    inline void
    simple_markup(
        Rule& simple
      , char mark
      , Action const& action
      , Rule const& close
    )
    {
        simple =
            mark >>
            (
                (
                    graph_p                     // A single char. e.g. *c*
                    >> eps_p(mark
                        >> (space_p | punct_p | end_p))
                                                // space_p, punct_p or end_p
                )                               // must follow mark
            |
                (   graph_p >>                  // graph_p must follow mark
                    *(anychar_p -
                        (   (graph_p >> mark)   // Make sure that we don't go
                        |   close               // past a single block
                        )
                    ) >> graph_p                // graph_p must precede mark
                    >> eps_p(mark
                        >> (space_p | punct_p | end_p))
                                                // space_p, punct_p or end_p
                )                               // must follow mark
            )                                   [action]
            >> mark
            ;
    }

    template <typename Actions>
    struct phrase_grammar : grammar<phrase_grammar<Actions> >
    {
        phrase_grammar(Actions& actions, bool& no_eols)
            : no_eols(no_eols), actions(actions) {}

        template <typename Scanner>
        struct definition
        {
            definition(phrase_grammar const& self)
            {
                using detail::var;
                Actions& actions = self.actions;

                space =
                    *(space_p | comment)
                    ;

                blank =
                    *(blank_p | comment)
                    ;

                eol = blank >> eol_p
                    ;

                phrase_end =
                    ']' |
                    if_p(var(self.no_eols))
                    [
                        eol >> eol                      // Make sure that we don't go
                    ]                                   // past a single block, except
                    ;                                   // when preformatted.

                hard_space =
                    (eps_p - (alnum_p | '_')) >> space  // must not be preceded by
                    ;                                   // alpha-numeric or underscore

                comment =
                    "[/" >> *(dummy_block | (anychar_p - ']')) >> ']'
                    ;

                dummy_block =
                    '[' >> *(dummy_block | (anychar_p - ']')) >> ']'
                    ;

                common =
                        macro
                    |   phrase_markup
                    |   code_block
                    |   inline_code
                    |   simple_format
                    |   escape
                    |   comment
                    ;

                macro =
                    eps_p(actions.macro                 // must not be followed by
                        >> (eps_p - (alpha_p | '_')))   // alpha or underscore
                    >> actions.macro                    [actions.do_macro]
                    ;

                template_args =
                    template_arg                        [push_back_a(actions.template_info)]
                    >> *(
                            ".." >> template_arg        [push_back_a(actions.template_info)]
                        )
                    ;

                static const bool true_ = true;
                static const bool false_ = false;

                template_ =
                    (
                        ch_p('`')                       [assign_a(actions.template_escape,true_)]
                        |
                        eps_p                           [assign_a(actions.template_escape,false_)]
                    )
                    >>
                    ( (
                        (eps_p(punct_p)
                            >> actions.templates.scope
                        )                               [push_back_a(actions.template_info)]
                        >> !template_args
                    ) | (
                        (actions.templates.scope
                            >> eps_p
                        )                               [push_back_a(actions.template_info)]
                        >> !(hard_space
                            >> template_args)
                    ) )
                    ;

                brackets =
                    '[' >> +template_arg >> ']'
                    ;

                template_arg =
                    +(brackets | (anychar_p - (str_p("..") | ']')))
                    ;

                inline_code =
                    '`' >>
                    (
                       *(anychar_p -
                            (   '`'
                            |   (eol >> eol)            // Make sure that we don't go
                            )                           // past a single block
                        ) >> eps_p('`')
                    )                                   [actions.inline_code]
                    >>  '`'
                    ;

                code_block =
                        (
                            "```" >>
                            (
                               *(anychar_p - "```")
                                    >> eps_p("```")
                            )                           [actions.code_block]
                            >>  "```"
                        )
                    |   (
                            "``" >>
                            (
                               *(anychar_p - "``")
                                    >> eps_p("``")
                            )                           [actions.code_block]
                            >>  "``"
                        )
                    ;

                simple_format =
                        simple_bold
                    |   simple_italic
                    |   simple_underline
                    |   simple_teletype
                    ;

                simple_markup(simple_bold,
                    '*', actions.simple_bold, phrase_end);
                simple_markup(simple_italic,
                    '/', actions.simple_italic, phrase_end);
                simple_markup(simple_underline,
                    '_', actions.simple_underline, phrase_end);
                simple_markup(simple_teletype,
                    '=', actions.simple_teletype, phrase_end);

                phrase =
                   *(   common
                    |   comment
                    |   (anychar_p - phrase_end)        [actions.plain_char]
                    )
                    ;

                phrase_markup =
                        '['
                    >>  (   cond_phrase
                        |   image
                        |   url
                        |   link
                        |   anchor
                        |   source_mode
                        |   funcref
                        |   classref
                        |   memberref
                        |   enumref
                        |   macroref
                        |   headerref
                        |   conceptref
                        |   globalref
                        |   bold
                        |   italic
                        |   underline
                        |   teletype
                        |   strikethrough
                        |   quote
                        |   replaceable
                        |   footnote
                        |   template_                   [actions.do_template]
                        |   str_p("br")                 [actions.break_]
                        )
                    >>  ']'
                    ;

                escape =
                        str_p("\\n")                    [actions.break_]
                    |   "\\ "                           // ignore an escaped char
                    |   '\\' >> punct_p                 [actions.raw_char]
                    |   (
                            ("'''" >> !eol)             [actions.escape_pre]
                        >>  *(anychar_p - "'''")        [actions.raw_char]
                        >>  str_p("'''")                [actions.escape_post]
                        )
                    ;

                macro_identifier =
                    +(anychar_p - (space_p | ']'))
                    ;

                cond_phrase =
                        '?' >> blank
                    >>  macro_identifier                [actions.cond_phrase_pre]
                    >>  (!phrase)                       [actions.cond_phrase_post]
                    ;

                image =
                        '$' >> blank
                    >> (*(anychar_p -
                            phrase_end))                [actions.image]
                    ;

                url =
                        '@'
                    >>  (*(anychar_p -
                            (']' | hard_space)))        [actions.url_pre]
                    >>  (   eps_p(']')
                        |   (hard_space >> phrase)
                        )                               [actions.url_post]
                    ;

                link =
                        "link" >> hard_space
                    >>  (*(anychar_p -
                            (']' | hard_space)))        [actions.link_pre]
                    >>  (   eps_p(']')
                        |   (hard_space >> phrase)
                        )                               [actions.link_post]
                    ;

                anchor =
                        '#'
                    >>  blank
                    >>  (   *(anychar_p -
                                phrase_end)
                        )                               [actions.anchor]
                    ;

                funcref =
                    "funcref" >> hard_space
                    >>  (*(anychar_p -
                            (']' | hard_space)))        [actions.funcref_pre]
                    >>  (   eps_p(']')
                        |   (hard_space >> phrase)
                        )                               [actions.funcref_post]
                    ;

                classref =
                    "classref" >> hard_space
                    >>  (*(anychar_p -
                            (']' | hard_space)))        [actions.classref_pre]
                    >>  (   eps_p(']')
                        |   (hard_space >> phrase)
                        )                               [actions.classref_post]
                    ;

                memberref =
                    "memberref" >> hard_space
                    >>  (*(anychar_p -
                            (']' | hard_space)))        [actions.memberref_pre]
                    >>  (   eps_p(']')
                        |   (hard_space >> phrase)
                        )                               [actions.memberref_post]
                    ;

                enumref =
                    "enumref" >> hard_space
                    >>  (*(anychar_p -
                            (']' | hard_space)))        [actions.enumref_pre]
                    >>  (   eps_p(']')
                        |   (hard_space >> phrase)
                        )                               [actions.enumref_post]
                    ;

                macroref =
                    "macroref" >> hard_space
                    >>  (*(anychar_p -
                            (']' | hard_space)))        [actions.macroref_pre]
                    >>  (   eps_p(']')
                        |   (hard_space >> phrase)
                        )                               [actions.macroref_post]
                    ;

                headerref =
                    "headerref" >> hard_space
                    >>  (*(anychar_p -
                            (']' | hard_space)))        [actions.headerref_pre]
                    >>  (   eps_p(']')
                        |   (hard_space >> phrase)
                        )                               [actions.headerref_post]
                    ;

                conceptref =
                    "conceptref" >> hard_space
                    >>  (*(anychar_p -
                            (']' | hard_space)))        [actions.conceptref_pre]
                    >>  (   eps_p(']')
                        |   (hard_space >> phrase)
                        )                               [actions.conceptref_post]
                    ;

                globalref =
                    "globalref" >> hard_space
                    >>  (*(anychar_p -
                            (']' | hard_space)))        [actions.globalref_pre]
                    >>  (   eps_p(']')
                        |   (hard_space >> phrase)
                        )                               [actions.globalref_post]
                    ;

                bold =
                        ch_p('*')                       [actions.bold_pre]
                    >>  blank >> phrase                 [actions.bold_post]
                    ;

                italic =
                        ch_p('\'')                      [actions.italic_pre]
                    >>  blank >> phrase                 [actions.italic_post]
                    ;

                underline =
                        ch_p('_')                       [actions.underline_pre]
                    >>  blank >> phrase                 [actions.underline_post]
                    ;

                teletype =
                        ch_p('^')                       [actions.teletype_pre]
                    >>  blank >> phrase                 [actions.teletype_post]
                    ;

                strikethrough =
                        ch_p('-')                       [actions.strikethrough_pre]
                    >>  blank >> phrase                 [actions.strikethrough_post]
                    ;

                quote =
                        ch_p('"')                       [actions.quote_pre]
                    >>  blank >> phrase                 [actions.quote_post]
                    ;

                replaceable =
                        ch_p('~')                       [actions.replaceable_pre]
                    >>  blank >> phrase                 [actions.replaceable_post]
                    ;

                source_mode =
                    (
                        str_p("c++")
                    |   "python"
                    )                                   [assign_a(actions.source_mode)]
                    ;

                footnote =
                        str_p("footnote")               [actions.footnote_pre]
                    >>  blank >> phrase                 [actions.footnote_post]
                    ;
            }

            rule<Scanner>   space, blank, comment, phrase, phrase_markup, image,
                            phrase_end, bold, italic, underline, teletype,
                            strikethrough, escape, url, common, funcref, classref,
                            memberref, enumref, macroref, headerref, conceptref, globalref,
                            anchor, link, hard_space, eol, inline_code, simple_format,
                            simple_bold, simple_italic, simple_underline,
                            simple_teletype, source_mode, template_, template_arg,
                            quote, code_block, footnote, replaceable, macro,
                            brackets, template_args, dummy_block, cond_phrase,
                            macro_identifier
                            ;

            rule<Scanner> const&
            start() const { return common; }
        };

        bool& no_eols;
        Actions& actions;
    };

    template <typename Actions>
    struct simple_phrase_grammar
    : public grammar<simple_phrase_grammar<Actions> >
    {
        simple_phrase_grammar(Actions& actions)
            : actions(actions) {}

        template <typename Scanner>
        struct definition
        {
            definition(simple_phrase_grammar const& self)
                : unused(false), common(self.actions, unused)
            {
                Actions& actions = self.actions;

                phrase =
                   *(   common
                    |   comment
                    |   (anychar_p - ']')           [actions.plain_char]
                    )
                    ;

                comment =
                    "[/" >> *(dummy_block | (anychar_p - ']')) >> ']'
                    ;

                dummy_block =
                    '[' >> *(dummy_block | (anychar_p - ']')) >> ']'
                    ;
            }

            bool unused;
            rule<Scanner> phrase, comment, dummy_block;
            phrase_grammar<Actions> common;

            rule<Scanner> const&
            start() const { return phrase; }
        };

        Actions& actions;
    };
}

#endif // BOOST_SPIRIT_QUICKBOOK_PHRASE_HPP

