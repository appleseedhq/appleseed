
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_UTILITY_XMLELEMENT_H
#define APPLESEED_FOUNDATION_UTILITY_XMLELEMENT_H

// appleseed.foundation headers.
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstdio>
#include <string>
#include <utility>
#include <vector>

// Forward declarations.
namespace foundation    { class Indenter; }

namespace foundation
{

//
// A class representing a XML element.
//

class XMLElement
{
  public:
    // Constructor, opens the element.
    XMLElement(
        const std::string&  name,
        std::FILE*          file,
        Indenter&           indenter);

    // Destructor, closes the element.
    ~XMLElement();

    // Append an attribute to the element.
    template <typename T>
    void add_attribute(
        const std::string&  name,
        const T&            value);

    // Write the element.
    void write(const bool has_content);

  private:
    typedef std::pair<std::string, std::string> Attribute;
    typedef std::vector<Attribute> AttributeVector;

    const std::string       m_name;
    std::FILE*              m_file;
    Indenter&               m_indenter;
    AttributeVector         m_attributes;
    bool                    m_opened;
    bool                    m_closed;
};


//
// XMLElement class implementation.
//

template <typename T>
void XMLElement::add_attribute(
    const std::string&      name,
    const T&                value)
{
    assert(!m_opened);
    m_attributes.push_back(std::make_pair(name, to_string(value)));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_XMLELEMENT_H
