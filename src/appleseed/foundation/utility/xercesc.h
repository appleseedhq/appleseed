
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/log/log.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/foreach.h"

// Xerces-C++ headers.
#include "xercesc/sax/ErrorHandler.hpp"
#include "xercesc/sax/SAXParseException.hpp"
#include "xercesc/sax2/Attributes.hpp"
#include "xercesc/sax2/DefaultHandler.hpp"
#include "xercesc/util/XMLString.hpp"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <map>
#include <memory>
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
    // Initialize Xerces-C++. This method is thread-safe.
    // Returns true if Xerces-C++ is properly initialized.
    static bool initialize();

    // Initialize Xerces-C++ and issue an error message if
    // the initialization failed. This method is thread-safe.
    static bool initialize(Logger& logger);

    // Terminate Xerces-C++. This method is thread-safe.
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
    XercesCContext();

    // Constructor, initializes Xerces-C++ and issue an
    // error message if the initialization failed.
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
        const XMLSize_t             length) = 0;

    // Receive notification of the start of a child element.
    virtual void start_child_element(
        const ElementID             element,
        ElementHandlerType*         handler) = 0;

    // Receive notification of the end of a child element.
    virtual void end_child_element(
        const ElementID             element,
        ElementHandlerType*         handler) = 0;
};


//
// A convenient base class for element handlers.
//

template <typename ElementID>
class ElementHandlerBase
  : public IElementHandler<ElementID>
{
  public:
    typedef IElementHandler<ElementID> ElementHandlerType;

    // Receive notification of the start of an element.
    void start_element(
        const xercesc::Attributes&  attrs) override;

    // Receive notification of the end of an element.
    void end_element() override;

    // Receive notification of character data inside an element.
    void characters(
        const XMLCh* const          chars,
        const XMLSize_t             length) override;

    // Receive notification of the start of a child element.
    void start_child_element(
        const ElementID             element,
        ElementHandlerType*         handler) override;

    // Receive notification of the end of a child element.
    void end_child_element(
        const ElementID             element,
        ElementHandlerType*         handler) override;

  protected:
    // Utility function to retrieve the value of an attribute.
    static std::string get_value(
        const xercesc::Attributes&  attrs,
        const std::string&          name,
        const std::string&          default_value = "");
};


//
// Element handler factory interface.
//

template <typename ElementID>
class IElementHandlerFactory
  : public NonCopyable
{
  public:
    typedef IElementHandler<ElementID> ElementHandlerType;

    // Destructor.
    virtual ~IElementHandlerFactory() {}

    // Create a new instance of the element handler.
    virtual std::unique_ptr<ElementHandlerType> create() = 0;
};


//
// A generic SAX2 content handler.
//

template <typename ElementID>
class SAX2ContentHandler
  : public xercesc::DefaultHandler
{
  public:
    typedef IElementHandlerFactory<ElementID> ElementHandlerFactoryType;

    // Constructor.
    SAX2ContentHandler();

    // Destructor.
    ~SAX2ContentHandler() override;

    // Register a factory for a given element.
    void register_factory(
        const std::string&                          name,
        const ElementID                             id,
        std::unique_ptr<ElementHandlerFactoryType>  handler_factory);

    // Receive notification of the start of an element.
    void startElement(
        const XMLCh* const                          uri,
        const XMLCh* const                          localname,
        const XMLCh* const                          qname,
        const xercesc::Attributes&                  attrs) override;

    // Receive notification of the end of an element.
    void endElement(
        const XMLCh* const                          uri,
        const XMLCh* const                          localname,
        const XMLCh* const                          qname) override;

    // Receive notification of character data inside an element.
    void characters(
        const XMLCh* const                          chars,
        const XMLSize_t                             length) override;

  private:
    typedef IElementHandler<ElementID> ElementHandlerType;

    struct FactoryInfo
    {
        ElementID                   m_id;
        ElementHandlerFactoryType*  m_handler_factory;
    };

    typedef std::map<std::string, FactoryInfo> FactoryInfoMap;
    typedef std::stack<ElementHandlerType*> ElementHandlerStack;

    FactoryInfoMap                  m_factory_info;
    ElementHandlerStack             m_handler_stack;
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
        const std::string&  input_filepath);

    // Reset the error handler object on its reuse.
    void resetErrors() override;

    // Receive notification of a warning.
    void warning(const xercesc::SAXParseException& e) override;

    // Receive notification of a recoverable error.
    void error(const xercesc::SAXParseException& e) override;

    // Receive notification of a non-recoverable error.
    void fatalError(const xercesc::SAXParseException& e) override;

    // Read the notification counters.
    size_t get_warning_count() const;
    size_t get_error_count() const;
    size_t get_fatal_error_count() const;

  private:
    Logger&             m_logger;
    const std::string   m_input_filepath;

    size_t              m_warning_count;
    size_t              m_error_count;
    size_t              m_fatal_error_count;

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
// ElementHandlerBase class implementation.
//

template <typename ElementID>
void ElementHandlerBase<ElementID>::start_element(
    const xercesc::Attributes&  attrs)
{
}

template <typename ElementID>
void ElementHandlerBase<ElementID>::end_element()
{
}

template <typename ElementID>
void ElementHandlerBase<ElementID>::characters(
    const XMLCh* const          chars,
    const XMLSize_t             length)
{
}

template <typename ElementID>
void ElementHandlerBase<ElementID>::start_child_element(
    const ElementID             element,
    ElementHandlerType*         handler)
{
}

template <typename ElementID>
void ElementHandlerBase<ElementID>::end_child_element(
    const ElementID             element,
    ElementHandlerType*         handler)
{
}

template <typename ElementID>
inline std::string ElementHandlerBase<ElementID>::get_value(
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

template <typename ElementID>
SAX2ContentHandler<ElementID>::SAX2ContentHandler()
{
    // Push a dummy element handler on the stack to avoid special-casing for an empty stack.
    m_handler_stack.push(new ElementHandlerBase<ElementID>());
}

template <typename ElementID>
SAX2ContentHandler<ElementID>::~SAX2ContentHandler()
{
    while (!m_handler_stack.empty())
    {
        delete m_handler_stack.top();
        m_handler_stack.pop();
    }

    for (const_each<FactoryInfoMap> i = m_factory_info; i; ++i)
        delete i->second.m_handler_factory;

    m_factory_info.clear();
}

template <typename ElementID>
void SAX2ContentHandler<ElementID>::register_factory(
    const std::string&                          name,
    const ElementID                             id,
    std::unique_ptr<ElementHandlerFactoryType>  handler_factory)
{
    FactoryInfo info;
    info.m_id = id;
    info.m_handler_factory = handler_factory.release();
    m_factory_info[name] = info;
}

template <typename ElementID>
void SAX2ContentHandler<ElementID>::startElement(
    const XMLCh* const                          uri,
    const XMLCh* const                          localname,
    const XMLCh* const                          qname,
    const xercesc::Attributes&                  attrs)
{
    const typename FactoryInfoMap::const_iterator it =
        m_factory_info.find(transcode(localname));

    ElementHandlerType* handler;

    if (it == m_factory_info.end())
    {
        handler = new ElementHandlerBase<ElementID>();
    }
    else
    {
        handler = it->second.m_handler_factory->create().release();

        m_handler_stack.top()->start_child_element(it->second.m_id, handler);
    }

    m_handler_stack.push(handler);

    handler->start_element(attrs);
}

template <typename ElementID>
void SAX2ContentHandler<ElementID>::endElement(
    const XMLCh* const                          uri,
    const XMLCh* const                          localname,
    const XMLCh* const                          qname)
{
    ElementHandlerType* handler = m_handler_stack.top();

    handler->end_element();

    m_handler_stack.pop();

    const typename FactoryInfoMap::const_iterator it =
        m_factory_info.find(transcode(localname));

    if (it != m_factory_info.end())
        m_handler_stack.top()->end_child_element(it->second.m_id, handler);

    delete handler;
}

template <typename ElementID>
void SAX2ContentHandler<ElementID>::characters(
    const XMLCh* const                          chars,
    const XMLSize_t                             length)
{
    assert(!m_handler_stack.empty());

    m_handler_stack.top()->characters(chars, length);
}

}   // namespace foundation
