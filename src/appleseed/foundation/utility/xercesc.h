
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_UTILITY_XERCESC_H
#define APPLESEED_FOUNDATION_UTILITY_XERCESC_H

// appleseed.foundation headers.
#include "foundation/core/concepts.h"
#include "foundation/platform/thread.h"
#include "foundation/platform/types.h"
#include "foundation/utility/log.h"

// Xerces-C++ headers.
#include "xercesc/framework/MemoryManager.hpp"
#include "xercesc/sax/ErrorHandler.hpp"
#include "xercesc/sax/SAXParseException.hpp"
#include "xercesc/sax2/Attributes.hpp"
#include "xercesc/sax2/DefaultHandler.hpp"
#include "xercesc/util/XMLString.hpp"

// Standard headers.
#include <cassert>
#include <map>
#include <stack>
#include <string>

// Forward declarations.
namespace foundation    { class Logger; }

namespace foundation
{

//
// An utility class to initialize and terminate Xerces-C++ in a thread-safe manner.
//

class XercesCManager
  : public NonCopyable
{
  public:
    // Initialize Xerces-C++.  This method is thread-safe.
    // Returns true if Xerces-C++ is properly initialized.
    static bool initialize(Logger& logger);

    // Terminate Xerces-C++.  This method is thread-safe.
    static void terminate();

  private:
    static boost::mutex s_mutex;
};


//
// An utility class to automatize Xerces-C++ initialization and termination.
//

class XercesCContext
  : public NonCopyable
{
  public:
    // Constructor, initializes Xerces-C++.
    explicit XercesCContext(Logger& logger);

    // Destructor, terminates Xerces-C++.
    ~XercesCContext();

    // Return true if Xerces-C++ was properly initialized.
    bool is_initialized() const;

  private:
    bool m_initialized;
};


//
// Utility functions for string transcoding.
//

std::basic_string<char> transcode(const XMLCh* s);
std::basic_string<XMLCh> transcode(const char* s);
std::basic_string<XMLCh> transcode(const std::string& s);

//
// Element handler interface.
//

template <typename ElementID>
class IElementHandler
  : public NonCopyable
{
  public:
    typedef IElementHandler<ElementID> ElementHandlerType;

    // Destructor.
    virtual ~IElementHandler() {}

    // Receive notification of the start of an element.
    virtual void start_element(
        const xercesc::Attributes&  attrs) = 0;

    // Receive notification of the end of an element.
    virtual void end_element() = 0;

    // Receive notification of character data inside an element.
    virtual void characters(
        const XMLCh* const          chars,
        const unsigned int          length) = 0;

    // Receive notification of the start of a child element.
    virtual void start_child_element(
        const ElementID             element,
        ElementHandlerType*         handler) = 0;

    // Receive notification of the end of a child element.
    virtual void end_child_element(
        const ElementID             element,
        ElementHandlerType*         handler) = 0;

    // Utility function to retrieve the value of an attribute.
    static std::string get_value(
        const xercesc::Attributes&  attrs,
        const std::string&          name,
        const std::string&          default_value = "");
};


//
// Default element handler, implements all the methods of the
// IElementHandler interface but does nothing.
//

template <typename ElementID>
class DefaultElementHandler
  : public IElementHandler<ElementID>
{
  public:
    typedef IElementHandler<ElementID> ElementHandlerType;

    // Receive notification of the start of an element.
    virtual void start_element(
        const xercesc::Attributes&  attrs) {}

    // Receive notification of the end of an element.
    virtual void end_element() {}

    // Receive notification of character data inside an element.
    virtual void characters(
        const XMLCh* const          chars,
        const unsigned int          length) {}

    // Receive notification of the start of a child element.
    virtual void start_child_element(
        const ElementID             element,
        ElementHandlerType*         handler) {}

    // Receive notification of the end of a child element.
    virtual void end_child_element(
        const ElementID             element,
        ElementHandlerType*         handler) {}
};


//
// A generic SAX2 content handler.
//

template <typename ElementID>
class SAX2ContentHandler
  : public xercesc::DefaultHandler
{
  public:
    // Constructor.
    SAX2ContentHandler();

    // Register an element.
    void register_element(
        const std::string&          name,
        const ElementID             id,
        IElementHandler<ElementID>* handler);

    // Receive notification of the start of an element.
    virtual void startElement(
        const XMLCh* const          uri,
        const XMLCh* const          localname,
        const XMLCh* const          qname,
        const xercesc::Attributes&  attrs);

    // Receive notification of the end of an element.
    virtual void endElement(
        const XMLCh* const          uri,
        const XMLCh* const          localname,
        const XMLCh* const          qname);

    // Receive notification of character data inside an element.
    virtual void characters(
        const XMLCh* const          chars,
        const unsigned int          length);

  private:
    typedef IElementHandler<ElementID> ElementHandlerType;

    struct ElementRecord
    {
        ElementID           m_id;
        ElementHandlerType* m_handler;
    };

    typedef std::map<std::string, ElementRecord> ElementMap;
    typedef std::stack<ElementHandlerType*> ElementHandlerStack;

    ElementMap              m_elements;
    ElementHandlerStack     m_eh_stack;
};


//
// An error handler that forwards messages to a logger.
//

class ErrorLogger
  : public xercesc::ErrorHandler
{
  public:
    // Constructor.
    ErrorLogger(
        Logger&             logger,
        const std::string&  input_filename);

    // Reset the error handler object on its reuse.
    virtual void resetErrors();

    // Receive notification of a warning.
    virtual void warning(const xercesc::SAXParseException& e);

    // Receive notification of a recoverable error.
    virtual void error(const xercesc::SAXParseException& e);

    // Receive notification of a non-recoverable error.
    virtual void fatalError(const xercesc::SAXParseException& e);

  private:
    Logger&             m_logger;
    const std::string   m_input_filename;

    void print(
        const LogMessage::Category          category,
        const xercesc::SAXParseException&   e) const;
};


//
// Transcoding functions implementation.
//

inline std::basic_string<char> transcode(const XMLCh* s)
{
    char* temp = xercesc::XMLString::transcode(s);
    std::basic_string<char> result = temp;

    xercesc::XMLString::release(&temp);

    return result;
}

inline std::basic_string<XMLCh> transcode(const char* s)
{
    XMLCh* temp = xercesc::XMLString::transcode(s);
    std::basic_string<XMLCh> result = temp;

    xercesc::XMLString::release(&temp);

    return result;
}

inline std::basic_string<XMLCh> transcode(const std::string& s)
{
    return transcode(s.c_str());
}


//
// IElementHandler class implementation.
//

// Utility function to retrieve the value of an attribute.
template <typename ElementID>
inline std::string IElementHandler<ElementID>::get_value(
    const xercesc::Attributes&  attrs,
    const std::string&          name,
    const std::string&          default_value)
{
    const XMLCh* value = attrs.getValue(transcode(name).c_str());
    return value ? transcode(value) : default_value;
}


//
// SAX2ContentHandler class implementation.
//

// Constructor.
template <typename ElementID>
SAX2ContentHandler<ElementID>::SAX2ContentHandler()
{
    // Push an empty element handler on the stack to avoid
    // special-casing for an empty stack.
    m_eh_stack.push(new DefaultElementHandler<ElementID>());
}

// Register an element.
template <typename ElementID>
void SAX2ContentHandler<ElementID>::register_element(
    const std::string&              name,
    const ElementID                 id,
    IElementHandler<ElementID>*     handler)
{
    ElementRecord record;
    record.m_id = id;
    record.m_handler = handler;
    m_elements[name] = record;
}

// Receive notification of the start of an element.
template <typename ElementID>
void SAX2ContentHandler<ElementID>::startElement(
    const XMLCh* const              uri,
    const XMLCh* const              localname,
    const XMLCh* const              qname,
    const xercesc::Attributes&      attrs)
{
    const typename ElementMap::const_iterator it =
        m_elements.find(transcode(localname));

    if (it != m_elements.end())
    {
        m_eh_stack.top()->start_child_element(
            it->second.m_id,
            it->second.m_handler);
        m_eh_stack.push(it->second.m_handler);
        it->second.m_handler->start_element(attrs);
    }
    else
    {
        // Push an empty element handler on the stack.
        m_eh_stack.push(new DefaultElementHandler<ElementID>());
    }
}

// Receive notification of the end of an element.
template <typename ElementID>
void SAX2ContentHandler<ElementID>::endElement(
    const XMLCh* const              uri,
    const XMLCh* const              localname,
    const XMLCh* const              qname)
{
    const typename ElementMap::const_iterator it =
        m_elements.find(transcode(localname));

    if (it != m_elements.end())
    {
        it->second.m_handler->end_element();
        m_eh_stack.pop();
        m_eh_stack.top()->end_child_element(
            it->second.m_id,
            it->second.m_handler);
    }
    else
    {
        // Pop empty element handler from the stack.
        m_eh_stack.pop();
    }
}

// Receive notification of character data inside an element.
template <typename ElementID>
void SAX2ContentHandler<ElementID>::characters(
    const XMLCh* const              chars,
    const unsigned int              length)
{
    assert(!m_eh_stack.empty());
    m_eh_stack.top()->characters(chars, length);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_XERCESC_H
