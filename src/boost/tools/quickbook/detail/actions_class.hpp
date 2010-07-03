/*=============================================================================
    Copyright (c) 2002 2004 2006 Joel de Guzman
    Copyright (c) 2004 Eric Niebler
    http://spirit.sourceforge.net/

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_ACTIONS_CLASS_HPP)
#define BOOST_SPIRIT_ACTIONS_CLASS_HPP

#include "./actions.hpp"
#include <boost/tuple/tuple.hpp>

namespace quickbook
{
    using namespace boost::spirit::classic;
    namespace fs = boost::filesystem;

    struct actions
    {
        actions(char const* filein_, fs::path const& outdir, string_stream& out_);

    ///////////////////////////////////////////////////////////////////////////
    // State
    ///////////////////////////////////////////////////////////////////////////

        typedef std::vector<std::string> string_list;
        typedef std::vector<std::pair<std::string, std::string> > author_list;
        typedef std::vector<std::pair<string_list, std::string> > copyright_list;
        typedef std::pair<char, int> mark_type;
        static int const max_template_depth = 100;

    // header info
        std::string             doc_type;
        std::string             doc_title;
        std::string             doc_version;
        std::string             doc_id;
        std::string             doc_dirname;
        copyright_list          doc_copyrights;
        std::string             doc_purpose;
        std::string             doc_category;
        author_list             doc_authors;
        std::string             doc_license;
        std::string             doc_last_revision;
        std::string             include_doc_id;
        std::string             doc_license_1_1;
        std::string             doc_purpose_1_1;

    // main output stream
        collector               out;

    // auxilliary streams
        collector               phrase;
        collector               temp;
        collector               temp_para;
        collector               list_buffer;

    // state
        fs::path                filename;
        fs::path                outdir;
        string_symbols          macro;
        int                     section_level;
        std::string             section_id;
        std::string             qualified_section_id;
        std::string             source_mode;

        typedef boost::tuple<
            fs::path
          , fs::path
          , string_symbols
          , int
          , std::string
          , std::string
          , std::string>
        state_tuple;

        std::stack<state_tuple> state_stack;

    // temporary or global state
        std::string             table_title;
        unsigned                table_span;
        std::string             table_header;
        std::string             macro_id;
        std::stack<mark_type>   list_marks;
        int                     list_indent;
        std::vector<bool>       conditions;
        string_list             template_info;
        int                     template_depth;
        bool                    template_escape;
        template_stack          templates;
        int                     error_count;

    // push/pop the states and the streams
        void push();
        void pop();

    ///////////////////////////////////////////////////////////////////////////
    // actions
    ///////////////////////////////////////////////////////////////////////////
        error_action            error;
        phrase_to_string_action extract_doc_license;
        phrase_to_string_action extract_doc_purpose;

        code_action             code;
        code_action             code_block;
        inline_code_action      inline_code;
        phrase_action           paragraph;
        phrase_action           inside_paragraph;
        generic_header_action   h;
        header_action           h1, h2, h3, h4, h5, h6;
        markup_action           hr;
        phrase_action           blurb, blockquote, preformatted;
        phrase_action           warning, caution, important, note, tip;
        plain_char_action       plain_char;
        raw_char_action         raw_char;
        image_action            image;
        cond_phrase_action_pre  cond_phrase_pre;
        cond_phrase_action_post cond_phrase_post;

        list_action             list;
        list_format_action      list_format;
        phrase_action           list_item;

        link_action             funcref_pre;
        markup_action           funcref_post;
        link_action             classref_pre;
        markup_action           classref_post;
        link_action             memberref_pre;
        markup_action           memberref_post;
        link_action             enumref_pre;
        markup_action           enumref_post;
        link_action             macroref_pre;
        markup_action           macroref_post;
        link_action             headerref_pre;
        markup_action           headerref_post;
        link_action             conceptref_pre;
        markup_action           conceptref_post;
        link_action             globalref_pre;
        markup_action           globalref_post;

        markup_action           bold_pre;
        markup_action           bold_post;
        markup_action           italic_pre;
        markup_action           italic_post;
        markup_action           underline_pre;
        markup_action           underline_post;
        markup_action           teletype_pre;
        markup_action           teletype_post;
        markup_action           strikethrough_pre;
        markup_action           strikethrough_post;
        markup_action           quote_pre;
        markup_action           quote_post;
        markup_action           replaceable_pre;
        markup_action           replaceable_post;
        markup_action           footnote_pre;
        markup_action           footnote_post;

        simple_phrase_action    simple_bold;
        simple_phrase_action    simple_italic;
        simple_phrase_action    simple_underline;
        simple_phrase_action    simple_teletype;
        simple_phrase_action    simple_strikethrough;

        variablelist_action     variablelist;
        markup_action           start_varlistentry;
        markup_action           end_varlistentry;
        markup_action           start_varlistterm;
        markup_action           end_varlistterm;
        start_varlistitem_action start_varlistitem;
        end_varlistitem_action  end_varlistitem;

        break_action            break_;
        macro_identifier_action macro_identifier;
        macro_definition_action macro_definition;
        do_macro_action         do_macro;
        template_body_action    template_body;
        do_template_action      do_template;
        link_action             url_pre;
        markup_action           url_post;
        link_action             link_pre;
        markup_action           link_post;
        table_action            table;
        start_row_action        start_row;
        markup_action           end_row;
        start_col_action        start_cell;
        end_col_action          end_cell;
        anchor_action           anchor;

        begin_section_action    begin_section;
        end_section_action      end_section;
        xinclude_action         xinclude;
        include_action          include;
        import_action           import;

        markup_action           escape_pre;
        markup_action           escape_post;
    };
}

#endif // BOOST_SPIRIT_ACTIONS_CLASS_HPP

