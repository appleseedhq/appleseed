
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

#ifndef APPLESEED_FOUNDATION_UTILITY_INDENTER_H
#define APPLESEED_FOUNDATION_UTILITY_INDENTER_H

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>

namespace foundation
{

//
// Indenter.
//

class Indenter
{
  public:
    // Constructor. 'tab_size' is the size of one level of indentation.
    Indenter(
        const size_t    tab_size,
        const char      tab_char = ' ');

    // Add one level of indentation.
    Indenter& operator++();

    // Subtract one level of indentation.
    Indenter& operator--();

    // Return the indentation string.
    const std::string& str() const;
    const char* c_str() const;

  private:
    const size_t        m_tab_size;
    const char          m_tab_char;
    std::string         m_str;
};


//
// Indenter class implementation.
//

// Constructor. 'tab_size' is the size of one level of indentation.
inline Indenter::Indenter(
    const size_t    tab_size,
    const char      tab_char)
  : m_tab_size(tab_size)
  , m_tab_char(tab_char)
{
}

// Add one level of indentation.
inline Indenter& Indenter::operator++()
{
    m_str.resize(m_str.size() + m_tab_size, m_tab_char);
    return *this;
}

// Subtract one level of indentation.
inline Indenter& Indenter::operator--()
{
    assert(m_str.size() >= m_tab_size);
    if (m_str.size() >= m_tab_size)
        m_str.resize(m_str.size() - m_tab_size, m_tab_char);
    else m_str.resize(0);
    return *this;
}

// Return the indentation string.
inline const std::string& Indenter::str() const
{
    return m_str;
}
inline const char* Indenter::c_str() const
{
    return m_str.c_str();
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_INDENTER_H
