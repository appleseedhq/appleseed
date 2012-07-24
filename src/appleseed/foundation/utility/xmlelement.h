
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
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/indenter.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstdio>
#include <string>
#include <utility>
#include <vector>

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

    // Close the element.
    void close();

  private:
    typedef std::pair<std::string, std::string> Attribute;
    typedef std::vector<Attribute> AttributeVector;

    const std::string       m_name;
    std::FILE*              m_file;
    Indenter&               m_indenter;
    AttributeVector         m_attributes;
    bool                    m_opened;
};


//
// An utility function to write a dictionary to an XML file.
//
// Example: a dictionary containing two scalar values "x" and "y"
// and a child dictionary "nested" containing a scalar value "z"
// will be written as follow:
//
//   <parameter name="x" value="17" />
//   <parameter name="y" value="42" />
//   <parameters name="nested">
//       <parameter name="z" value="66" />
//   </parameters>
//

void write_dictionary(
    const Dictionary&       dictionary,
    std::FILE*              file,
    Indenter&               indenter);


//
// Implementation.
//

inline XMLElement::XMLElement(
    const std::string&      name,
    std::FILE*              file,
    Indenter&               indenter)
  : m_name(name)
  , m_file(file)
  , m_indenter(indenter)
  , m_opened(false)
{
}

inline XMLElement::~XMLElement()
{
    close();
}

template <typename T>
void XMLElement::add_attribute(
    const std::string&      name,
    const T&                value)
{
    assert(!m_opened);

    m_attributes.push_back(std::make_pair(name, to_string(value)));
}

inline void XMLElement::write(const bool has_content)
{
    assert(!m_opened);

    std::fprintf(m_file, "%s<%s", m_indenter.c_str(), m_name.c_str());

    for (const_each<AttributeVector> i = m_attributes; i; ++i)
    {
        const std::string attribute_value = replace_special_xml_characters(i->second);
        std::fprintf(m_file, " %s=\"%s\"", i->first.c_str(), attribute_value.c_str());
    }

    if (has_content)
    {
        std::fprintf(m_file, ">\n");
        ++m_indenter;
        m_opened = true;
    }
    else
    {
        std::fprintf(m_file, " />\n");
    }
}

inline void XMLElement::close()
{
    if (m_opened)
    {
        --m_indenter;
        std::fprintf(m_file, "%s</%s>\n", m_indenter.c_str(), m_name.c_str());
        m_opened = false;
    }
}

inline void write_dictionary(
    const Dictionary&       dictionary,
    std::FILE*              file,
    Indenter&               indenter)
{
    for (const_each<StringDictionary> i = dictionary.strings(); i; ++i)
    {
        XMLElement element("parameter", file, indenter);
        element.add_attribute("name", i->name());
        element.add_attribute("value", i->value<std::string>());
        element.write(false);
    }

    for (const_each<DictionaryDictionary> i = dictionary.dictionaries(); i; ++i)
    {
        XMLElement element("parameters", file, indenter);
        element.add_attribute("name", i->name());
        element.write(true);
        write_dictionary(i->value(), file, indenter);
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_XMLELEMENT_H
