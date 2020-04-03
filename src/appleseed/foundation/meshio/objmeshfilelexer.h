
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

#pragma once

// appleseed.foundation headers.
#include "foundation/meshio/objmeshfilereader.h"
#include "foundation/platform/compiler.h"
#include "foundation/string/string.h"
#include "foundation/utility/bufferedfile.h"

// Standard headers.
#include <cassert>
#include <cctype>
#include <cstddef>
#include <cstdlib>
#include <string>
#include <vector>

namespace foundation
{

//
// A lexical analyzer for the OBJ file format.
//

class OBJMeshFileLexer
{
  public:
    // Available modes for parsing floating-point values.
    enum ParsingMode
    {
        Fast, Precise
    };

    // Constructor.
    explicit OBJMeshFileLexer(const ParsingMode parsing_mode = Precise)
      : m_parsing_mode(parsing_mode)
      , m_eof(false)
      , m_line_number(0)
      , m_line(4096)
      , m_line_size(0)
      , m_line_index(0)
    {
        // Precompute the value of std::isspace(c) for all c.
        for (int i = 0; i < 256; ++i)
            m_is_space[i] = std::isspace(i) != 0;
    }

    // Open an input file.
    // Return true on success, false on error.
    bool open(const std::string& filename)
    {
        m_eof = false;
        m_line_number = 0;
        m_line_size = 0;
        m_line_index = 0;

        m_file.open(
            filename.c_str(),
            BufferedFile::TextType,
            BufferedFile::ReadMode);

        if (!m_file.is_open())
            return false;

        read_next_line();

        return true;
    }

    // Close the input file.
    void close()
    {
        m_file.close();
    }

    // Return the position of the current line in the file.
    size_t get_line_number() const
    {
        assert(m_file.is_open());

        return m_line_number;
    }

    // Return the current character in the line.
    APPLESEED_FORCE_INLINE unsigned char get_char() const
    {
        assert(m_file.is_open());

        return m_line_index == m_line_size ? '\n' : m_line[m_line_index];
    }

    // Advance to the next character in the line.
    APPLESEED_FORCE_INLINE void next_char()
    {
        assert(m_file.is_open());

        if (m_line_index < m_line_size)
            ++m_line_index;
        else
        {
            m_line_index = 0;
            read_next_line();
        }
    }

    // Return true if a given character is a blank character, similarly to std::isspace().
    APPLESEED_FORCE_INLINE bool is_space(const unsigned char c) const
    {
        return m_is_space[c];
    }

    // Return true if the end of the line has been reached.
    APPLESEED_FORCE_INLINE bool is_eol() const
    {
        assert(m_file.is_open());

        return m_line_index == m_line_size;
    }

    // Return true if the end of the file has been reached.
    APPLESEED_FORCE_INLINE bool is_eof() const
    {
        assert(m_file.is_open());

        return m_eof && is_eol();
    }

    // Eat blank characters and comments.
    void eat_blanks()
    {
        assert(m_file.is_open());

        while (true)
        {
            if (is_eof())
                break;

            const unsigned char c = get_char();

            if (c == '\n')
                break;

            if (c == '#')
            {
                m_line_index = m_line_size;
                break;
            }

            if (!is_space(c))
                break;

            next_char();
        }
    }

    // Eat the entire line.
    void eat_line()
    {
        m_line_index = 0;
        read_next_line();
    }

    // Accept a end-of-line character, or generate a parse error.
    void accept_newline()
    {
        assert(m_file.is_open());

        if (!is_eol())
            parse_error();

        next_char();
    }

    // Accept a string of non-blank characters, or generate a parse error.
    void accept_string(const char** begin, size_t* length)
    {
        assert(m_file.is_open());

        if (is_eof())
            parse_error();

        if (is_space(get_char()))
            parse_error();

        const size_t string_begin = m_line_index;
        size_t string_end = m_line_index;

        while (true)
        {
            if (is_eof())
                break;

            const unsigned char c = get_char();

            if (is_space(c))
                break;

            ++string_end;

            next_char();
        }

        *begin = &m_line[string_begin];
        *length = string_end - string_begin;
    }

    // Accept a long integer, or generate a parse error.
    APPLESEED_FORCE_INLINE long accept_long()
    {
        assert(m_file.is_open());

        // Read an integer value at the current position in the line.
        const char* base_ptr = &m_line[0];
        const char* end_ptr;
        const long value =
            fast_strtol_base10(
                base_ptr + m_line_index,
                &end_ptr);

        // Move the cursor to the first character that isn't part of the value.
        m_line_index = end_ptr - base_ptr;

        return value;
    }

    // Accept a double-precision floating point number, or generate a parse error.
    APPLESEED_FORCE_INLINE double accept_double()
    {
        assert(m_file.is_open());

        // Read a floating-point value at the current position in the line.
        char* base_ptr = &m_line[0];
        char* end_ptr;
        const double value =
            m_parsing_mode == Fast
                ? fast_strtod(
                    base_ptr + m_line_index,
                    &end_ptr)
                : std::strtod(
                    base_ptr + m_line_index,
                    &end_ptr);

        // Move the cursor to the first character that isn't part of the value.
        m_line_index = end_ptr - base_ptr;

        return value;
    }

  private:
    const ParsingMode   m_parsing_mode;     // parsing mode for floating-point values
    bool                m_is_space[256];    // precomputed values of std::isspace(c) for all c
    BufferedFile        m_file;
    bool                m_eof;              // has the end of the file been reached?
    size_t              m_line_number;      // position of the current line in the file
    std::vector<char>   m_line;             // current line
    size_t              m_line_size;        // size of the current line (not counting the zero terminator)
    size_t              m_line_index;       // position of the cursor in the current line

    // Close the input file and throw an ExceptionParseError exception.
    void parse_error()
    {
        m_file.close();
        throw OBJMeshFileReader::ExceptionParseError(m_line_number);
    }

    // Read the next line from the input file.
    void read_next_line()
    {
        assert(m_file.is_open());

        m_line_size = 0;

        if (!m_eof)
        {
            ++m_line_number;

            while (m_line_size < m_line.size() - 1)
            {
                // Read one character from the file.
                char c;
                if (m_file.read(&c) < 1)
                {
                    // Reached the end of the file.
                    m_eof = true;
                    break;
                }

                // Stop as soon as the end of the line is reached.
                if (c == '\n')
                    break;

                // Append the character to the line.
                m_line[m_line_size++] = c;
            }
        }

        // Append a null terminator.
        m_line[m_line_size] = 0;
    }
};

}   // namespace foundation
