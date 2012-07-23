
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

// Interface header.
#include "xmlelement.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/indenter.h"

using namespace std;

namespace foundation
{

XMLElement::XMLElement(
    const string&       name,
    FILE*               file,
    Indenter&           indenter)
  : m_name(name)
  , m_file(file)
  , m_indenter(indenter)
  , m_opened(false)
  , m_closed(false)
{
}

XMLElement::~XMLElement()
{
    assert(m_opened);

    if (!m_closed)
    {
        --m_indenter;
        fprintf(m_file, "%s</%s>\n", m_indenter.c_str(), m_name.c_str());
    }
}

void XMLElement::write(const bool has_content)
{
    assert(!m_opened);

    fprintf(m_file, "%s<%s", m_indenter.c_str(), m_name.c_str());

    for (const_each<AttributeVector> i = m_attributes; i; ++i)
    {
        const string attribute_value = replace_special_xml_characters(i->second);
        fprintf(m_file, " %s=\"%s\"", i->first.c_str(), attribute_value.c_str());
    }

    if (has_content)
    {
        fprintf(m_file, ">\n");
        ++m_indenter;
        m_closed = false;
    }
    else
    {
        fprintf(m_file, " />\n");
        m_closed = true;
    }

    m_opened = true;
}

void write_dictionary(
    const Dictionary&   dictionary,
    FILE*               file,
    Indenter&           indenter)
{
    for (const_each<StringDictionary> i = dictionary.strings(); i; ++i)
    {
        XMLElement element("parameter", file, indenter);
        element.add_attribute("name", i->name());
        element.add_attribute("value", i->value<string>());
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

}   // namespace foundation
