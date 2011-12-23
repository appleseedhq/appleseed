
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MESH_OBJMESHFILELEXER_H
#define APPLESEED_FOUNDATION_MESH_OBJMESHFILELEXER_H

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/mesh/objmeshfilereader.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/bufferedfile.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cassert>
#include <cctype>
#include <cstddef>
#include <cstdlib>
#include <string>

namespace foundation
{

//
// A lexical analyzer for the OBJ file format.
//

class OBJMeshFileLexer
{
  public:
    // Constructor.
    OBJMeshFileLexer()
      : m_eof(false)
      , m_line_number(0)
      , m_line_index(0)
    {
        // Precompute the value of std::isspace(c) for all c.
        for (size_t i = 0; i < 256; ++i)
            m_is_space[i] = std::isspace(static_cast<int>(i)) != 0;
    }

    // Open an input file.
    // Return true on success, false on error.
    bool open(const std::string& filename)
    {
        m_eof = false;
        m_line_number = 0;
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
    FORCE_INLINE unsigned char get_char() const
    {
        assert(m_file.is_open());

        return m_line_index == m_line.size() ? '\n' : m_line[m_line_index];
    }

    // Advance to the next character in the line.
    FORCE_INLINE void next_char()
    {
        assert(m_file.is_open());

        if (m_line_index < m_line.size())
            ++m_line_index;
        else
        {
            m_line_index = 0;
            read_next_line();
        }
    }

    // Return true if a given character is a blank character, similarly to std::isspace().
    FORCE_INLINE bool is_space(const unsigned char c) const
    {
        return m_is_space[c];
    }

    // Return true if the end of the line has been reached.
    FORCE_INLINE bool is_eol() const
    {
        assert(m_file.is_open());

        return m_line_index == m_line.size();
    }

    // Return true if the end of the file has been reached.
    FORCE_INLINE bool is_eof() const
    {
        assert(m_file.is_open());

        return m_eof && m_line_index == m_line.size();
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
                m_line_index = m_line.size();
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

    // Eat until the end-of-line character (eat the remaining of the line).
    void eat_until_newline()
    {
        assert(m_file.is_open());

        while (true)
        {
            if (is_eof())
                break;

            const unsigned char c = get_char();

            if (c == '\n')
                break;

            next_char();
        }
    }

    // Accept a end-of-line character, or generate a parse error.
    void accept_newline()
    {
        assert(m_file.is_open());

        if (is_eof())
            parse_error();

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
    FORCE_INLINE long accept_long()
    {
        assert(m_file.is_open());

        // Read an integer value at the current position in the line.
        const char* base_ptr = m_line.c_str();
        char* end_ptr;
        const long n =
            std::strtol(
                base_ptr + m_line_index,
                &end_ptr,
                10);        // base

        // Move the cursor to the first character that isn't part of the value.
        m_line_index = end_ptr - base_ptr;

        return n;
    }

    // Accept a double-precision floating point number, or generate a parse error.
    FORCE_INLINE double accept_double()
    {
        assert(m_file.is_open());

        // Read a floating-point value at the current position in the line.
        const char* base_ptr = m_line.c_str();
        char* end_ptr;
        const double d =
            std::strtod(
                base_ptr + m_line_index,
                &end_ptr);

        // Move the cursor to the first character that isn't part of the value.
        m_line_index = end_ptr - base_ptr;

        return d;
    }

  private:
    bool            m_is_space[256];    // precomputed values of std::isspace(c) for all c
    BufferedFile    m_file;
    bool            m_eof;              // has the end of the file been reached?
    std::string     m_line;             // current line
    size_t          m_line_number;      // position of the current line in the file
    size_t          m_line_index;       // position of the cursor in the current line

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

        clear_keep_memory(m_line);

        if (m_eof)
            return;

        ++m_line_number;

        while (true)
        {
            // Read one character from the file.
            unsigned char c;
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
            m_line += c;
        }
    }
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MESH_OBJMESHFILELEXER_H
