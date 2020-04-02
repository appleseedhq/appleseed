
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
#include "settingsfilereader.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/xercesc.h"

// Xerces-C++ headers.
#include "xercesc/dom/DOM.hpp"
#include "xercesc/parsers/XercesDOMParser.hpp"
#include "xercesc/util/XMLException.hpp"

// Standard headers.
#include <cassert>
#include <memory>
#include <string>

using namespace xercesc;

namespace foundation
{

SettingsFileReader::SettingsFileReader(Logger& logger)
  : m_logger(logger)
{
}

namespace
{
    void build_dictionary(const DOMNode* node, Dictionary& dictionary)
    {
        assert(node);

        const std::basic_string<XMLCh> name_attribute_name = transcode("name");
        const std::basic_string<XMLCh> value_attribute_name = transcode("value");

        node = node->getFirstChild();

        while (node)
        {
            if (node->getNodeType() == DOMNode::ELEMENT_NODE)
            {
                const std::string element_name = transcode(node->getNodeName());

                if (element_name == "parameters")
                {
                    const DOMNamedNodeMap* attributes = node->getAttributes();
                    const DOMNode* name_attribute = attributes->getNamedItem(name_attribute_name.c_str());

                    Dictionary child_dictionary;
                    build_dictionary(node, child_dictionary);

                    dictionary.insert(
                        transcode(name_attribute->getNodeValue()).c_str(),
                        child_dictionary);
                }
                else if (element_name == "parameter")
                {
                    const DOMNamedNodeMap* attributes = node->getAttributes();
                    const DOMNode* name_attribute = attributes->getNamedItem(name_attribute_name.c_str());
                    const DOMNode* value_attribute = attributes->getNamedItem(value_attribute_name.c_str());

                    if (value_attribute)
                    {
                        dictionary.insert(
                            transcode(name_attribute->getNodeValue()).c_str(),
                            transcode(value_attribute->getNodeValue()));
                    }
                    else
                    {
                        dictionary.insert(
                            transcode(name_attribute->getNodeValue()).c_str(),
                            transcode(node->getTextContent()));
                    }
                }
            }

            node = node->getNextSibling();
        }
    }
}

bool SettingsFileReader::read(
    const char*     settings_filename,
    const char*     schema_filename,
    Dictionary&     settings)
{
    // Initialize Xerces-C++.
    XercesCContext xerces_context(m_logger);
    if (!xerces_context.is_initialized())
        return false;

    // Create the DOM parser.
    std::unique_ptr<XercesDOMParser> parser(new XercesDOMParser());
    parser->setValidationScheme(XercesDOMParser::Val_Always);
    parser->setDoNamespaces(true);
    parser->setDoSchema(true);
    parser->setExternalNoNamespaceSchemaLocation(schema_filename);

    // Create the error handler.
    std::unique_ptr<ErrorLogger> error_handler(new ErrorLogger(m_logger, settings_filename));
    parser->setErrorHandler(error_handler.get());

    // Parse the settings file.
    try
    {
        parser->parse(settings_filename);
    }
    catch (const XMLException&)
    {
        return false;
    }
    catch (const DOMException&)
    {
        return false;
    }

    // Bail out in case of warnings or errors.
    if (error_handler->get_warning_count() > 0 ||
        error_handler->get_error_count() > 0 ||
        error_handler->get_fatal_error_count() > 0)
        return false;

    // Bail out if we won't have a document.
    const DOMDocument* document = parser->getDocument();
    if (!document)
        return false;

    // Bail out if we won't have a root node.
    const DOMNode* root_node = document->getFirstChild();
    if (!root_node)
        return false;

    // Build a dictionary out of the XML document.
    build_dictionary(root_node, settings);

    return true;
}

}   // namespace foundation
