/*=============================================================================
    Copyright (c) 2002 2004  2006Joel de Guzman
    Copyright (c) 2004 Eric Niebler
    http://spirit.sourceforge.net/

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_QUICKBOOK_BLOCK_HPP)
#define BOOST_SPIRIT_QUICKBOOK_BLOCK_HPP

#include "./detail/utils.hpp"
#include "./phrase.hpp"
#include <boost/spirit/include/classic_core.hpp>
#include <boost/spirit/include/classic_confix.hpp>
#include <boost/spirit/include/classic_chset.hpp>
#include <boost/spirit/include/classic_assign_actor.hpp>
#include <boost/spirit/include/classic_if.hpp>
#include <boost/spirit/include/classic_symbols.hpp>

namespace quickbook
{
    using namespace boost::spirit::classic;

    template <typename Actions, bool skip_initial_spaces = false>
    struct block_grammar : grammar<block_grammar<Actions> >
    {
        block_grammar(Actions& actions_)
            : actions(actions_) {}

        template <typename Scanner>
        struct definition
        {
            definition(block_grammar const& self)
                : no_eols(true)
                , common(self.actions, no_eols)
            {
                using detail::var;
                Actions& actions = self.actions;

                if (skip_initial_spaces)
                {
                    start_ =
                        *(space_p | comment) >> blocks >> blank
                        ;
                }
                else
                {
                    start_ =
                        blocks >> blank
                        ;
                }

                blocks =
                   +(   block_markup
                    |   code
                    |   list                            [actions.list]
                    |   hr                              [actions.hr]
                    |   comment >> *eol
                    |   paragraph                       [actions.paragraph]
                    |   eol
                    )
                    ;

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
                    if_p(var(no_eols))
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

                hr =
                    str_p("----")
                    >> *(anychar_p - eol)
                    >> +eol
                    ;

                block_markup =
                        '[' >> space
                    >>  (   begin_section
                        |   end_section
                        |   headings
                        |   blurb
                        |   blockquote
                        |   admonition
                        |   preformatted
                        |   def_macro
                        |   table
                        |   variablelist
                        |   xinclude
                        |   include
                        |   import
                        |   template_
                        )
                    >>  (   (space >> ']' >> +eol)
                        |   eps_p                       [actions.error]
                        )
                    ;

                begin_section =
                       "section"
                    >> hard_space
                    >>  (':' >> (*(alnum_p | '_'))      [assign_a(actions.section_id)]
                        | eps_p                         [assign_a(actions.section_id)]
                        )
                    >> phrase                           [actions.begin_section]
                    ;

                end_section =
                    str_p("endsect")                    [actions.end_section]
                    ;

                headings =
                    h1 | h2 | h3 | h4 | h5 | h6 | h
                    ;

                h = "heading" >> hard_space >> phrase   [actions.h];
                h1 = "h1" >> hard_space >> phrase       [actions.h1];
                h2 = "h2" >> hard_space >> phrase       [actions.h2];
                h3 = "h3" >> hard_space >> phrase       [actions.h3];
                h4 = "h4" >> hard_space >> phrase       [actions.h4];
                h5 = "h5" >> hard_space >> phrase       [actions.h5];
                h6 = "h6" >> hard_space >> phrase       [actions.h6];

                static const bool true_ = true;
                static const bool false_ = false;

                inside_paragraph =
                    phrase                              [actions.inside_paragraph]
                    >> *(
                        eol >> eol >> phrase            [actions.inside_paragraph]
                    )
                    ;

                blurb =
                    "blurb" >> hard_space
                    >> inside_paragraph                 [actions.blurb]
                    >> eps_p
                    ;

                blockquote =
                    ':' >> blank >>
                    inside_paragraph                    [actions.blockquote]
                    ;

                admonition =
                    "warning" >> blank >>
                    inside_paragraph                    [actions.warning]
                    |
                    "caution" >> blank >>
                    inside_paragraph                    [actions.caution]
                    |
                    "important" >> blank >>
                    inside_paragraph                    [actions.important]
                    |
                    "note" >> blank >>
                    inside_paragraph                    [actions.note]
                    |
                    "tip" >> blank >>
                    inside_paragraph                    [actions.tip]
                    ;

                preformatted =
                    "pre" >> hard_space                 [assign_a(no_eols, false_)]
                    >> !eol >> phrase                   [actions.preformatted]
                    >> eps_p                            [assign_a(no_eols, true_)]
                    ;

                macro_identifier =
                    +(anychar_p - (space_p | ']'))
                    ;

                def_macro =
                    "def" >> hard_space
                    >> macro_identifier                 [actions.macro_identifier]
                    >> blank >> phrase                  [actions.macro_definition]
                    ;

                identifier =
                    (alpha_p | '_') >> *(alnum_p | '_')
                    ;

                template_id =
                    identifier | (punct_p - (ch_p('[') | ']'))
                    ;

                template_ =
                    "template"
                    >> hard_space >> template_id        [push_back_a(actions.template_info)]
                    >>
                    !(
                        space >> '['
                        >> *(
                                space >> template_id    [push_back_a(actions.template_info)]
                            )
                        >> space >> ']'
                    )
                    >> template_body                    [actions.template_body]
                    ;

                template_body =
                   *(('[' >> template_body >> ']') | (anychar_p - ']'))
                    >> space >> eps_p(']')
                    ;

                variablelist =
                    "variablelist"
                    >>  (eps_p(*blank_p >> eol_p) | hard_space)
                    >>  (*(anychar_p - eol))            [assign_a(actions.table_title)]
                    >>  +eol
                    >>  *varlistentry
                    >>  eps_p                           [actions.variablelist]
                    ;

                varlistentry =
                    space
                    >>  ch_p('[')                       [actions.start_varlistentry]
                    >>
                    (
                        (
                            varlistterm
                            >> +varlistitem
                            >>  ch_p(']')               [actions.end_varlistentry]
                            >>  space
                        )
                        | eps_p                         [actions.error]
                    )
                    ;

                varlistterm =
                    space
                    >>  ch_p('[')                       [actions.start_varlistterm]
                    >>
                    (
                        (
                            phrase
                            >>  ch_p(']')               [actions.end_varlistterm]
                            >>  space
                        )
                        | eps_p                         [actions.error]
                    )
                    ;

                varlistitem =
                    space
                    >>  ch_p('[')                       [actions.start_varlistitem]
                    >>
                    (
                        (
                            inside_paragraph
                            >>  ch_p(']')               [actions.end_varlistitem]
                            >>  space
                        )
                        | eps_p                         [actions.error]
                    )
                    ;

                table =
                    "table"
                    >>  (eps_p(*blank_p >> eol_p) | hard_space)
                    >>  (*(anychar_p - eol))            [assign_a(actions.table_title)]
                    >>  +eol
                    >>  *table_row
                    >>  eps_p                           [actions.table]
                    ;

                table_row =
                    space
                    >>  ch_p('[')                       [actions.start_row]
                    >>
                    (
                        (
                            *table_cell
                            >>  ch_p(']')               [actions.end_row]
                            >>  space
                        )
                        | eps_p                         [actions.error]
                    )
                    ;

                table_cell =
                    space
                    >>  ch_p('[')                       [actions.start_cell]
                    >>
                    (
                        (
                            inside_paragraph
                            >>  ch_p(']')               [actions.end_cell]
                            >>  space
                        )
                        | eps_p                         [actions.error]
                    )
                    ;

                xinclude =
                       "xinclude"
                    >> hard_space
                    >> (*(anychar_p -
                            phrase_end))                [actions.xinclude]
                    ;

                import =
                       "import"
                    >> hard_space
                    >> (*(anychar_p -
                            phrase_end))                [actions.import]
                    ;

                include =
                       "include"
                    >> hard_space
                    >>
                   !(
                        ':'
                        >> (*((alnum_p | '_') - space_p))[assign_a(actions.include_doc_id)]
                        >> space
                    )
                    >> (*(anychar_p -
                            phrase_end))                [actions.include]
                    ;

                code =
                    (
                        code_line
                        >> *(*eol >> code_line)
                    )                                   [actions.code]
                    >> +eol
                    ;

                code_line =
                    ((ch_p(' ') | '\t'))
                    >> *(anychar_p - eol) >> eol
                    ;

                list =
                    eps_p(ch_p('*') | '#') >>
                   +(
                        (*blank_p
                        >> (ch_p('*') | '#'))           [actions.list_format]
                        >> *blank_p
                        >> list_item
                    )                                   [actions.list_item]
                    ;

                list_item =
                   *(   common
                    |   (anychar_p -
                            (   eol_p >> *blank_p >> eps_p(ch_p('*') | '#')
                            |   (eol >> eol)
                            )
                        )                               [actions.plain_char]
                    )
                    >> +eol
                    ;

                paragraph_end_markups =
                    "section", "endsect", "h1", "h2", "h3", "h4", "h5", "h6",
                    "blurb", ":", "pre", "def", "table", "include", "xinclude",
                    "variablelist", "import", "template", "warning", "caution",
                    "important", "note", "tip", ":"
                    ;

                paragraph_end =
                    '[' >> space >> paragraph_end_markups >> hard_space | eol >> eol
                    ;

                paragraph =
                   *(   common
                    |   (anychar_p -                    // Make sure we don't go past
                            paragraph_end               // a single block.
                        )                               [actions.plain_char]
                    )
                    >> (eps_p('[') | +eol)
                    ;

                phrase =
                   *(   common
                    |   comment
                    |   (anychar_p -
                            phrase_end)                 [actions.plain_char]
                    )
                    ;
            }

            bool no_eols;

            rule<Scanner>   start_, blocks, block_markup, code, code_line,
                            paragraph, space, blank, comment, headings, h, h1, h2,
                            h3, h4, h5, h6, hr, blurb, blockquote, admonition,
                            phrase, list, phrase_end, ordered_list, def_macro,
                            macro_identifier, table, table_row, variablelist,
                            varlistentry, varlistterm, varlistitem, table_cell,
                            preformatted, list_item, begin_section, end_section,
                            xinclude, include, hard_space, eol, paragraph_end,
                            template_, template_id, template_formal_arg,
                            template_body, identifier, dummy_block, import,
                            inside_paragraph;

            symbols<>       paragraph_end_markups;

            phrase_grammar<Actions> common;

            rule<Scanner> const&
            start() const { return start_; }
        };

        Actions&   actions;
    };
}

#endif // BOOST_SPIRIT_QUICKBOOK_BLOCK_HPP


