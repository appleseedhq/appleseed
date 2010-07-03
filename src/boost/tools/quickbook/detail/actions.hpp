/*=============================================================================
    Copyright (c) 2002 2004 2006 Joel de Guzman
    Copyright (c) 2004 Eric Niebler
    http://spirit.sourceforge.net/

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_QUICKBOOK_ACTIONS_HPP)
#define BOOST_SPIRIT_QUICKBOOK_ACTIONS_HPP

#include <time.h>
#include <map>
#include <string>
#include <vector>
#include <stack>
#include <algorithm>
#include <boost/spirit/include/classic_iterator.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/foreach.hpp>
#include <boost/tuple/tuple.hpp>
#include "../syntax_highlight.hpp"
#include "./collector.hpp"
#include "./template_stack.hpp"
#include "./utils.hpp"

#ifdef BOOST_MSVC
// disable copy/assignment could not be generated, unreferenced formal params
#pragma warning (push)
#pragma warning(disable : 4511 4512 4100)
#endif

namespace quickbook
{
    namespace fs = boost::filesystem;
    typedef position_iterator<std::string::const_iterator> iterator;
    typedef symbols<std::string> string_symbols;

    struct actions;
    extern tm* current_time; // the current time
    extern tm* current_gm_time; // the current UTC time
    extern bool debug_mode;
    extern unsigned qbk_major_version;
    extern unsigned qbk_minor_version;
    extern unsigned qbk_version_n; // qbk_major_version * 100 + qbk_minor_version
    extern std::vector<std::string> include_path;

    // forward declarations
    struct actions;
    int parse(char const* filein_, actions& actor, bool ignore_docinfo = false);

    struct error_action
    {
        // Prints an error message to std::cerr

        error_action(
            int& error_count)
        : error_count(error_count) {}

        void operator()(iterator first, iterator /*last*/) const;

        int& error_count;
    };

    struct phrase_action
    {
        //  blurb, blockquote, preformatted, list_item,
        //  unordered_list, ordered_list

        phrase_action(
            collector& out,
            collector& phrase,
            std::string const& pre,
            std::string const& post)
        : out(out)
        , phrase(phrase)
        , pre(pre)
        , post(post) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        collector& phrase;
        std::string pre;
        std::string post;
    };

    struct header_action
    {
        //  Handles paragraph, h1, h2, h3, h4, h5, h6,

        header_action(
            collector& out,
            collector& phrase,
            std::string const& library_id,
            std::string const& section_id,
            std::string const& qualified_section_id,
            std::string const& pre,
            std::string const& post)
        : out(out)
        , phrase(phrase)
        , library_id(library_id)
        , section_id(section_id)
        , qualified_section_id(qualified_section_id)
        , pre(pre)
        , post(post) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        collector& phrase;
        std::string const& library_id;
        std::string const& section_id;
        std::string const& qualified_section_id;
        std::string pre;
        std::string post;
    };

    struct generic_header_action
    {
        //  Handles h

        generic_header_action(
            collector& out,
            collector& phrase,
            std::string const& library_id,
            std::string const& section_id,
            std::string const& qualified_section_id,
            int const& section_level)
        : out(out)
        , phrase(phrase)
        , library_id(library_id)
        , section_id(section_id)
        , qualified_section_id(qualified_section_id)
        , section_level(section_level) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        collector& phrase;
        std::string const& library_id;
        std::string const& section_id;
        std::string const& qualified_section_id;
        int const& section_level;
    };

    struct simple_phrase_action
    {
        //  Handles simple text formats

        simple_phrase_action(
            collector& out
          , std::string const& pre
          , std::string const& post
          , string_symbols const& macro)
        : out(out)
        , pre(pre)
        , post(post)
        , macro(macro) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        std::string pre;
        std::string post;
        string_symbols const& macro;
    };

    struct cond_phrase_action_pre
    {
        //  Handles conditional phrases

        cond_phrase_action_pre(
            collector& out
          , std::vector<bool>& conditions
          , string_symbols const& macro)
        : out(out)
        , conditions(conditions)
        , macro(macro) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        std::vector<bool>& conditions;
        string_symbols const& macro;
    };

    struct cond_phrase_action_post
    {
        //  Handles conditional phrases

        cond_phrase_action_post(
            collector& out
          , std::vector<bool>& conditions
          , string_symbols const& macro)
        : out(out)
        , conditions(conditions)
        , macro(macro) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        std::vector<bool>& conditions;
        string_symbols const& macro;
    };

    struct list_action
    {
        //  Handles lists

        typedef std::pair<char, int> mark_type;
        list_action(
            collector& out
          , collector& list_buffer
          , int& list_indent
          , std::stack<mark_type>& list_marks)
        : out(out)
        , list_buffer(list_buffer)
        , list_indent(list_indent)
        , list_marks(list_marks) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        collector& list_buffer;
        int& list_indent;
        std::stack<mark_type>& list_marks;
    };

    struct list_format_action
    {
        //  Handles list formatting and hierarchy

        typedef std::pair<char, int> mark_type;
        list_format_action(
            collector& out
          , int& list_indent
          , std::stack<mark_type>& list_marks
          , int& error_count)
        : out(out)
        , list_indent(list_indent)
        , list_marks(list_marks)
        , error_count(error_count) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        int& list_indent;
        std::stack<mark_type>& list_marks;
        int& error_count;
    };

    struct span
    {
        // Decorates c++ code fragments

        span(char const* name, collector& out)
        : name(name), out(out) {}

        void operator()(iterator first, iterator last) const;

        char const* name;
        collector& out;
    };

    struct unexpected_char
    {
        // Handles unexpected chars in c++ syntax

        unexpected_char(collector& out)
        : out(out) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
    };

    struct anchor_action
    {
        // Handles anchors

        anchor_action(collector& out)
            : out(out) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
    };

    namespace
    {
        char const* quickbook_get_date = "__quickbook_get_date__";
        char const* quickbook_get_time = "__quickbook_get_time__";
    }

    struct do_macro_action
    {
        // Handles macro substitutions

        do_macro_action(collector& phrase)
            : phrase(phrase) {}

        void operator()(std::string const& str) const;
        collector& phrase;
    };

    struct space
    {
        // Prints a space

        space(collector& out)
            : out(out) {}

        void operator()(iterator first, iterator last) const;
        void operator()(char ch) const;

        collector& out;
    };

    struct pre_escape_back
    {
        // Escapes back from code to quickbook (Pre)

        pre_escape_back(actions& escape_actions, std::string& save)
            : escape_actions(escape_actions), save(save) {}

        void operator()(iterator first, iterator last) const;

        actions& escape_actions;
        std::string& save;
    };

    struct post_escape_back
    {
        // Escapes back from code to quickbook (Post)

        post_escape_back(collector& out, actions& escape_actions, std::string& save)
            : out(out), escape_actions(escape_actions), save(save) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        actions& escape_actions;
        std::string& save;
    };

    typedef cpp_highlight<
        span
      , space
      , string_symbols
      , do_macro_action
      , pre_escape_back
      , post_escape_back
      , actions
      , unexpected_char
      , collector>
    cpp_p_type;

    typedef python_highlight<
        span
      , space
      , string_symbols
      , do_macro_action
      , pre_escape_back
      , post_escape_back
      , actions
      , unexpected_char
      , collector>
    python_p_type;

    struct code_action
    {
        // Does the actual syntax highlighing of code

        code_action(
            collector& out
          , collector& phrase
          , collector& temp
          , std::string const& source_mode
          , string_symbols const& macro
          , actions& escape_actions)
        : out(out)
        , phrase(phrase)
        , temp(temp)
        , source_mode(source_mode)
        , cpp_p(temp, macro, do_macro_action(temp), escape_actions)
        , python_p(temp, macro, do_macro_action(temp), escape_actions)
        {
        }

        void operator()(iterator first, iterator last) const;

        collector& out;
        collector& phrase;
        collector& temp;
        std::string const& source_mode;

        cpp_p_type cpp_p;
        python_p_type python_p;
    };

    struct inline_code_action
    {
        // Does the actual syntax highlighing of code inlined in text

        inline_code_action(
            collector& out
          , collector& temp
          , std::string const& source_mode
          , string_symbols const& macro
          , actions& escape_actions)
        : out(out)
        , source_mode(source_mode)
        , temp(temp)
        , cpp_p(temp, macro, do_macro_action(temp), escape_actions)
        , python_p(temp, macro, do_macro_action(temp), escape_actions)
        {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        std::string const& source_mode;
        collector& temp;

        cpp_p_type cpp_p;
        python_p_type python_p;
    };

    struct raw_char_action
    {
        // Prints a single raw (unprocessed) char.
        // Allows '<', '>'... etc.

        raw_char_action(collector& phrase)
        : phrase(phrase) {}

        void operator()(char ch) const;
        void operator()(iterator first, iterator /*last*/) const;

        collector& phrase;
    };

    struct plain_char_action
    {
        // Prints a single plain char.
        // Converts '<' to "&lt;"... etc See utils.hpp

        plain_char_action(collector& phrase)
        : phrase(phrase) {}

        void operator()(char ch) const;
        void operator()(iterator first, iterator /*last*/) const;

        collector& phrase;
    };

    struct image_action
    {
        // Handles inline images

        image_action(collector& phrase)
        : phrase(phrase) {}

        void operator()(iterator first, iterator last) const;

        collector& phrase;
    };

    struct markup_action
    {
        // A generic markup action

        markup_action(collector& phrase, std::string const& str)
        : phrase(phrase), str(str) {}

        template <typename T>
        void operator()(T const&) const
        {
            phrase << str;
        }

        template <typename T>
        void operator()(T const&, T const&) const
        {
            phrase << str;
        }

        collector& phrase;
        std::string str;
    };

    struct start_varlistitem_action
    {
        start_varlistitem_action(collector& phrase)
        : phrase(phrase) {}

        void operator()(char) const;

        collector& phrase;
    };

    struct end_varlistitem_action
    {
        end_varlistitem_action(collector& phrase, collector& temp_para)
        : phrase(phrase), temp_para(temp_para) {}

        void operator()(char) const;

        collector& phrase;
        collector& temp_para;
    };

    struct break_action
    {
        // Handles line-breaks (DEPRECATED!!!)

        break_action(collector& phrase)
        : phrase(phrase) {}

        void operator()(iterator f, iterator) const;

        collector& phrase;
    };

    struct macro_identifier_action
    {
        // Handles macro identifiers

        macro_identifier_action(quickbook::actions& actions)
        : actions(actions) {}

        void operator()(iterator first, iterator last) const;

        quickbook::actions& actions;
    };

    struct macro_definition_action
    {
        // Handles macro definitions

        macro_definition_action(quickbook::actions& actions)
        : actions(actions) {}

        void operator()(iterator first, iterator last) const;

        quickbook::actions& actions;
    };

    struct template_body_action
    {
        // Handles template definitions

        template_body_action(quickbook::actions& actions)
        : actions(actions) {}

        void operator()(iterator first, iterator last) const;

        quickbook::actions& actions;
    };

    struct do_template_action
    {
        // Handles template substitutions

        do_template_action(quickbook::actions& actions)
        : actions(actions) {}

        void operator()(iterator first, iterator last) const;

        quickbook::actions& actions;
    };

    struct link_action
    {
        // Handles links (URL, XML refentry, function, class, member)

        link_action(collector& phrase, char const* tag)
        : phrase(phrase), tag(tag) {}

        void operator()(iterator first, iterator last) const;

        collector& phrase;
        char const* tag;
    };

    struct variablelist_action
    {
        // Handles variable lists

        variablelist_action(quickbook::actions& actions)
        : actions(actions) {}

        void operator()(iterator, iterator) const;

        quickbook::actions& actions;
    };

    struct table_action
    {
        // Handles tables

        table_action(quickbook::actions& actions)
        : actions(actions) {}

        void operator()(iterator, iterator) const;

        quickbook::actions& actions;
    };

    struct start_row_action
    {
        // Handles table rows

        start_row_action(collector& phrase, unsigned& span, std::string& header)
            : phrase(phrase), span(span), header(header) {}

        void operator()(char) const;
        void operator()(iterator f, iterator) const;

        collector& phrase;
        unsigned& span;
        std::string& header;
    };

    struct start_col_action
    {
        // Handles table columns

        start_col_action(collector& phrase, unsigned& span)
        : phrase(phrase), span(span) {}

        void operator()(char) const;

        collector& phrase;
        unsigned& span;
    };

    struct end_col_action
    {
        end_col_action(collector& phrase, collector& temp_para)
        : phrase(phrase), temp_para(temp_para) {}

        void operator()(char) const;

        collector& phrase;
        collector& temp_para;
    };

    struct begin_section_action
    {
        // Handles begin page

        begin_section_action(
            collector& out
          , collector& phrase
          , std::string& library_id
          , std::string& section_id
          , int& section_level
          , std::string& qualified_section_id)
        : out(out)
        , phrase(phrase)
        , library_id(library_id)
        , section_id(section_id)
        , section_level(section_level)
        , qualified_section_id(qualified_section_id) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        collector& phrase;
        std::string& library_id;
        std::string& section_id;
        int& section_level;
        std::string& qualified_section_id;
    };

    struct end_section_action
    {
        end_section_action(
            collector& out
          , int& section_level
          , std::string& qualified_section_id
          , int& error_count)
        : out(out)
        , section_level(section_level)
        , qualified_section_id(qualified_section_id)
        , error_count(error_count) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        int& section_level;
        std::string& qualified_section_id;
        int& error_count;
   };

    struct xinclude_action
    {
        // Handles XML includes
        xinclude_action(collector& out_, quickbook::actions& actions_)
            : out(out_), actions(actions_) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        quickbook::actions& actions;
    };

    struct include_action
    {
        // Handles QBK includes

        include_action(quickbook::actions& actions_)
            : actions(actions_) {}

        void operator()(iterator first, iterator last) const;

        quickbook::actions& actions;
    };

    struct import_action
    {
        // Handles import of source code files (e.g. *.cpp *.py)
        import_action(collector& out_, quickbook::actions& actions_)
            : out(out_), actions(actions_) {}

        void operator()(iterator first, iterator last) const;

        collector& out;
        quickbook::actions& actions;
    };

    struct xml_author
    {
        // Handles xml author

        xml_author(collector& out)
        : out(out) {}

        void operator()(std::pair<std::string, std::string> const& author) const;

        collector& out;
    };

    struct xml_year
    {
        // Handles xml year

        xml_year(collector& out)
            : out(out) {}

        void operator()(std::string const &year) const;

        collector& out;
    };

    struct xml_copyright
    {
        // Handles xml copyright

        xml_copyright(collector& out)
            : out(out) {}

        void operator()(std::pair<std::vector<std::string>, std::string> const &copyright) const;

        collector& out;
    };

    void pre(collector& out, quickbook::actions& actions, bool ignore_docinfo = false);
    void post(collector& out, quickbook::actions& actions, bool ignore_docinfo = false);

    struct phrase_to_string_action
    {
        phrase_to_string_action(std::string& out, collector& phrase)
            : out(out) , phrase(phrase) {}

        void operator()(iterator first, iterator last) const;

        std::string& out;
        collector& phrase;
    };
}

#ifdef BOOST_MSVC
#pragma warning (pop)
#endif

#endif // BOOST_SPIRIT_QUICKBOOK_ACTIONS_HPP

