
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
#include "xercesc.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"

// Xerces-C++ headers.
#include "xercesc/util/PlatformUtils.hpp"
#include "xercesc/util/XMLException.hpp"
#include "xercesc/util/XMLExceptMsgs.hpp"

using namespace xercesc;

namespace foundation
{

//
// XercesCManager class implementation.
//

boost::mutex XercesCManager::s_mutex;

bool XercesCManager::initialize()
{
    boost::mutex::scoped_lock lock(s_mutex);

    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch (const XMLException&)
    {
        return false;
    }

    return true;
}

bool XercesCManager::initialize(Logger& logger)
{
    boost::mutex::scoped_lock lock(s_mutex);

    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch (const XMLException& e)
    {
        LOG_ERROR(
            logger,
            "failed to initialize Xerces-C++ library: %s.",
            transcode(e.getMessage()).c_str());

        return false;
    }

    return true;
}

void XercesCManager::terminate()
{
    boost::mutex::scoped_lock lock(s_mutex);

    XMLPlatformUtils::Terminate();
}


//
// XercesCContext class implementation.
//

XercesCContext::XercesCContext()
{
    m_initialized = XercesCManager::initialize();
}

XercesCContext::XercesCContext(Logger& logger)
{
    m_initialized = XercesCManager::initialize(logger);
}

XercesCContext::~XercesCContext()
{
    XercesCManager::terminate();
}

bool XercesCContext::is_initialized() const
{
    return m_initialized;
}


//
// ErrorLogger class implementation.
//

ErrorLogger::ErrorLogger(
    Logger&              logger,
    const std::string&   input_filepath)
  : m_logger(logger)
  , m_input_filepath(input_filepath)
{
    resetErrors();
}

void ErrorLogger::resetErrors()
{
    m_warning_count = 0;
    m_error_count = 0;
    m_fatal_error_count = 0;
}

void ErrorLogger::warning(const SAXParseException& e)
{
    ++m_warning_count;

    print(LogMessage::Warning, e);
}

void ErrorLogger::error(const SAXParseException& e)
{
    ++m_error_count;

    print(LogMessage::Error, e);
}

void ErrorLogger::fatalError(const SAXParseException& e)
{
    ++m_fatal_error_count;

#ifndef APPLESEED_WITH_EXTERNAL_XERCES
    switch (e.getOriginalExceptionCode())
    {
      // Using our modified version of Xerces-C, we can catch the case where
      // the file failed to open and print a (more) reasonable error message.
      case XMLExcepts::Scan_CouldNotOpenSource:
        LOG_ERROR(
            m_logger,
            "failed to open %s for reading.",
            m_input_filepath.c_str());
        break;

      // Report all other errors as is.
      default:
        print(LogMessage::Error, e);
        break;
    }
#else
    print(LogMessage::Error, e);
#endif
}

size_t ErrorLogger::get_warning_count() const
{
    return m_warning_count;
}

size_t ErrorLogger::get_error_count() const
{
    return m_error_count;
}

size_t ErrorLogger::get_fatal_error_count() const
{
    return m_fatal_error_count;
}

void ErrorLogger::print(
    const LogMessage::Category  category,
    const SAXParseException&    e) const
{
    LOG(
        m_logger,
        category,
        "while reading %s, at line %s, column %s: %s.",
        m_input_filepath.c_str(),
        pretty_uint(e.getLineNumber()).c_str(),
        pretty_uint(e.getColumnNumber()).c_str(),
        transcode(e.getMessage()).c_str());
}

}   // namespace foundation
