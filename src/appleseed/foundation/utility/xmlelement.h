
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
#include "foundation/containers/dictionary.h"
#include "foundation/platform/compiler.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/indenter.h"

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

    enum ContentType
    {
        HasNoContent,
        HasChildElements,
        HasChildText
    };

    // Write the element.
    void write(const ContentType content_type);

    // Close the element.
    void close();

  private:
    typedef std::pair<std::string, std::string> Attribute;
    typedef std::vector<Attribute> AttributeVector;

    const std::string       m_name;
    std::FILE*              m_file;
    Indenter&               m_indenter;
    AttributeVector         m_attributes;
    bool                    m_is_open;
    ContentType             m_content_type;
};


//
// An utility function to write a dictionary to an XML file.
//
// Example: a dictionary containing two scalar values "u" and "v"
// and a child dictionary "nested" containing a scalar named "x"
// with the value found in the attribute and another named "y"
// with the value found between an opening and a closing tag
// will be written as follow:
//
//   <parameter name="u" value="17" />
//   <parameter name="v" value="42" />
//   <parameters name="nested">
//       <parameter name="x" value="66" />
//       <parameter name="y">15</parameter>
//   </parameters>
//
// The example with the value found between an opening and a closing
// tag can contain special characters such as newlines.
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
  , m_is_open(false)
  , m_content_type(HasNoContent)
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
    assert(!m_is_open);

    m_attributes.push_back(std::make_pair(name, to_string(value)));
}

inline void XMLElement::write(const ContentType content_type)
{
    assert(!m_is_open);

    // Open the tag.
    std::fprintf(m_file, "%s<%s", m_indenter.c_str(), m_name.c_str());

    // Emit the attributes.
    for (const_each<AttributeVector> i = m_attributes; i; ++i)
    {
        const std::string attribute_value = replace_special_xml_characters(i->second);
        std::fprintf(m_file, " %s=\"%s\"", i->first.c_str(), attribute_value.c_str());
    }

    // Close the tag or the whole element.
    switch (content_type)
    {
      case HasNoContent:
        std::fprintf(m_file, " />\n");
        break;

      case HasChildElements:
        std::fprintf(m_file, ">\n");
        ++m_indenter;
        m_is_open = true;
        break;

      case HasChildText:
        std::fprintf(m_file, ">");
        m_is_open = true;
        break;
    }

    m_content_type = content_type;
}

inline void XMLElement::close()
{
    if (m_is_open)
    {
        // Close the element.
        switch (m_content_type)
        {
          case HasNoContent:
            APPLESEED_UNREACHABLE;
            break;

          case HasChildElements:
            --m_indenter;
            std::fprintf(m_file, "%s</%s>\n", m_indenter.c_str(), m_name.c_str());
            break;

          case HasChildText:
            std::fprintf(m_file, "</%s>\n", m_name.c_str());
            break;
        }

        m_is_open = false;
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
        element.add_attribute("name", i->key());

        const std::string value = i->value<std::string>();

        if (value.find('\n') != std::string::npos)
        {
            element.write(XMLElement::HasChildText);
            std::fprintf(file, "%s", value.c_str());
        }
        else
        {
            element.add_attribute("value", value);
            element.write(XMLElement::HasNoContent);
        }
    }

    for (const_each<DictionaryDictionary> i = dictionary.dictionaries(); i; ++i)
    {
        XMLElement element("parameters", file, indenter);
        element.add_attribute("name", i->key());
        element.write(XMLElement::HasChildElements);
        write_dictionary(i->value(), file, indenter);
    }
}

}   // namespace foundation
