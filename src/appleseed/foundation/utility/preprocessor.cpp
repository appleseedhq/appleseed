
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header.
#include "preprocessor.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <map>
#include <string>
#include <vector>

namespace foundation
{

//
// Preprocessor class implementation.
//

namespace
{
    bool is_separator(const char c)
    {
        return !((c >= 'a' && c <= 'z') ||
                 (c >= 'A' && c <= 'Z') ||
                 (c >= '0' && c <= '9') ||
                 (c == '_'));
    }

    bool is_surrounded_by_separators(
        const std::string&               s,
        const std::string::size_type     begin,
        const std::string::size_type     end)
    {
        const bool separator_on_left = begin == 0 || is_separator(s[begin - 1]);
        const bool separator_on_right = end == s.size() || is_separator(s[end]);

        return separator_on_left && separator_on_right;
    }

    bool is_directive(const std::string& line)
    {
        const std::string::size_type i = line.find_first_not_of(Blanks);

        return i != std::string::npos && line[i] == '#';
    }

    void split_line(const std::string& line, std::string& keyword, std::string& arguments)
    {
        std::string::size_type cursor = line.find_first_not_of(Blanks);

        const std::string::size_type keyword_begin = cursor;
        cursor = line.find_first_of(Blanks, cursor);

        keyword =
            cursor == std::string::npos
                ? line.substr(keyword_begin)
                : line.substr(keyword_begin, cursor - keyword_begin);

        arguments.clear();

        if (cursor == std::string::npos)
            return;

        cursor = line.find_first_not_of(Blanks, cursor);

        if (cursor == std::string::npos)
            return;

        const std::string::size_type arguments_begin = cursor;

        cursor = line.find_last_not_of(Blanks);

        arguments = line.substr(arguments_begin, cursor - arguments_begin + 1);
    }

    void split_directive(const std::string& line, std::string& keyword, std::string& arguments)
    {
        const std::string::size_type i = line.find_first_not_of(Blanks);

        assert(i != std::string::npos);
        assert(line[i] == '#');

        split_line(line.substr(i + 1), keyword, arguments);
    }

    TEST_SUITE(Foundation_Utility_Preprocessor_Impl)
    {
        TEST_CASE(IsSeparator_GivenSpace_ReturnsTrue)
        {
            EXPECT_TRUE(is_separator(' '));
        }

        TEST_CASE(IsSeparator_GivenNewLine_ReturnsTrue)
        {
            EXPECT_TRUE(is_separator('\n'));
        }

        TEST_CASE(IsSeparator_GivenLowerCaseLetterA_ReturnsFalse)
        {
            EXPECT_FALSE(is_separator('a'));
        }

        TEST_CASE(IsSeparator_GivenUpperCaseLetterA_ReturnsFalse)
        {
            EXPECT_FALSE(is_separator('A'));
        }

        TEST_CASE(IsSeparator_GivenUnderscore_ReturnsFalse)
        {
            EXPECT_FALSE(is_separator('_'));
        }

        TEST_CASE(IsSurroundedBySeparators_GivenLeadingWord_ReturnsTrue)
        {
            EXPECT_TRUE(is_surrounded_by_separators("This is a sentence.", 0, 4));
        }

        TEST_CASE(IsSurroundedBySeparators_GivenTrailingWord_ReturnsTrue)
        {
            EXPECT_TRUE(is_surrounded_by_separators("This is a sentence", 10, 18));
        }

        TEST_CASE(IsSurroundedBySeparators_GivenMiddleWord_ReturnsTrue)
        {
            EXPECT_TRUE(is_surrounded_by_separators("This is a sentence", 5, 7));
        }

        TEST_CASE(IsSurroundedBySeparators_GivenSegmentOfLeadingWord_ReturnsFalse)
        {
            EXPECT_FALSE(is_surrounded_by_separators("This is a sentence.", 1, 4));
        }

        TEST_CASE(SplitDirective_GivenKeyword_ReturnsKeyword)
        {
            std::string keyword, arguments;
            split_directive("#keyword", keyword, arguments);

            EXPECT_EQ("keyword", keyword);
        }

        TEST_CASE(SplitDirective_GivenSpacesBeforeHashCharacter_ReturnsKeyword)
        {
            std::string keyword, arguments;
            split_directive("   #keyword", keyword, arguments);

            EXPECT_EQ("keyword", keyword);
        }

        TEST_CASE(SplitDirective_GivenSpacesAfterHashCharacter_ReturnsKeyword)
        {
            std::string keyword, arguments;
            split_directive("#   keyword", keyword, arguments);

            EXPECT_EQ("keyword", keyword);
        }

        TEST_CASE(SplitDirective_GivenSpacesBeforeAndAfterHashCharacter_ReturnsKeyword)
        {
            std::string keyword, arguments;
            split_directive("   #   keyword", keyword, arguments);

            EXPECT_EQ("keyword", keyword);
        }

        TEST_CASE(SplitDirective_GivenSpacesAfterKeyword_ReturnsKeyword)
        {
            std::string keyword, arguments;
            split_directive("#keyword   ", keyword, arguments);

            EXPECT_EQ("keyword", keyword);
        }

        TEST_CASE(SplitDirective_GivenNoArgument_ReturnsEmptyArguments)
        {
            std::string keyword, arguments;
            split_directive("#keyword", keyword, arguments);

            EXPECT_EQ("", arguments);
        }

        TEST_CASE(SplitDirective_GivenArguments_ReturnsArguments)
        {
            std::string keyword, arguments;
            split_directive("#keyword arg1 arg2", keyword, arguments);

            EXPECT_EQ("arg1 arg2", arguments);
        }

        TEST_CASE(SplitDirective_GivenSpacesAfterArguments_ReturnsArguments)
        {
            std::string keyword, arguments;
            split_directive("#keyword arg1 arg2   ", keyword, arguments);

            EXPECT_EQ("arg1 arg2", arguments);
        }
    }
}

struct Preprocessor::Impl
{
    struct ExceptionParseError
      : public Exception
    {
        const size_t m_line_number;

        ExceptionParseError(const std::string& message, const size_t line_number)
          : Exception(message.c_str())
          , m_line_number(line_number)
        {
        }
    };

    bool                         m_succeeded;
    std::string                  m_error_message;
    size_t                       m_error_location;

    typedef std::map<std::string, std::string> SymbolTable;

    SymbolTable                  m_symbols;

    std::vector<std::string>     m_input_lines;
    size_t                       m_current_input_line;

    std::string                  m_result;
    size_t                       m_current_output_line;

    void process_text(const std::string& text)
    {
        assert(m_result.empty());

        m_succeeded = true;
        m_error_location = 0;
        m_current_input_line = 0;
        m_current_output_line = 0;

        split(text, "\n", m_input_lines);

        parse();
    }

    void parse()
    {
        while (!is_end_of_input_text())
        {
            std::string line = get_next_input_line();

            if (is_directive(line))
            {
                std::string keyword, arguments;
                split_directive(line, keyword, arguments);
                parse_directive(keyword, arguments);
            }
            else
            {
                process_line(line);
            }
        }
    }

    void parse_directive(const std::string& keyword, const std::string& arguments)
    {
        if (keyword == "define")
        {
            parse_define_directive(arguments);
        }
        else if (keyword == "ifdef")
        {
            parse_ifdef_directive(arguments);
        }
        else
        {
            parse_error(std::string("Unknown directive: #") + keyword);
        }
    }

    void parse_define_directive(const std::string& arguments)
    {
        std::string symbol, value;
        split_line(arguments, symbol, value);

        substitute_symbols(value);

        m_symbols[symbol] = value;
    }

    void parse_ifdef_directive(const std::string& ifdef_arguments)
    {
        const bool condition_value = evaluate_condition(ifdef_arguments);

        while (true)
        {
            if (is_end_of_input_text())
            {
                parse_error("Expected directive: #endif");
                break;
            }

            std::string line = get_next_input_line();

            if (is_directive(line))
            {
                std::string keyword, arguments;
                split_directive(line, keyword, arguments);

                if (keyword == "endif")
                    break;

                if (condition_value)
                    parse_directive(keyword, arguments);
            }
            else
            {
                if (condition_value)
                    process_line(line);
            }
        }
    }

    bool evaluate_condition(const std::string& condition) const
    {
        return m_symbols.find(condition) != m_symbols.end();
    }

    void process_line(std::string& line)
    {
        substitute_symbols(line);
        emit_line(line);
    }

    void substitute_symbols(std::string& line) const
    {
        for (const_each<SymbolTable> i = m_symbols; i; ++i)
            substitute_symbol(line, i->first, i->second);
    }

    void substitute_symbol(
        std::string&         line,
        const std::string&   old_string,
        const std::string&   new_string) const
    {
        std::string::size_type pos = line.find(old_string);

        while (pos != std::string::npos)
        {
            if (is_surrounded_by_separators(line, pos, pos + old_string.size()))
                line.replace(pos, old_string.size(), new_string);

            pos = line.find(old_string, pos + new_string.size());
        }
    }

    void parse_error(const std::string& message)
    {
        throw ExceptionParseError(message, m_current_input_line);
    }

    bool is_end_of_input_text() const
    {
        return m_current_input_line == m_input_lines.size();
    }

    const std::string& get_next_input_line()
    {
        assert(m_current_input_line < m_input_lines.size());

        return m_input_lines[m_current_input_line++];
    }

    void emit_line(const std::string& line)
    {
        if (m_current_output_line > 0)
            m_result += '\n';

        m_result += line;

        ++m_current_output_line;
    }
};

Preprocessor::Preprocessor()
  : impl(new Impl())
{
}

Preprocessor::~Preprocessor()
{
    delete impl;
}

void Preprocessor::define_symbol(const char* name)
{
    assert(name);

    impl->m_symbols[name] = "";
}

void Preprocessor::define_symbol(const char* name, const char* value)
{
    assert(name);
    assert(value);

    impl->m_symbols[name] = value;
}

void Preprocessor::process(const char* text)
{
    assert(text);

    try
    {
        impl->process_text(text);
    }
    catch (const Impl::ExceptionParseError& e)
    {
        impl->m_succeeded = false;
        impl->m_error_message = e.what();
        impl->m_error_location = e.m_line_number;
    }
}

const char* Preprocessor::get_processed_text() const
{
    return impl->m_result.c_str();
}

bool Preprocessor::succeeded() const
{
    return impl->m_succeeded;
}

bool Preprocessor::failed() const
{
    return !succeeded();
}

const char* Preprocessor::get_error_message() const
{
    return failed() ? impl->m_error_message.c_str() : nullptr;
}

size_t Preprocessor::get_error_location() const
{
    return failed() ? impl->m_error_location : ~size_t(0);
}

}   // namespace foundation
