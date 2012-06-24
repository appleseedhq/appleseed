
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
#include "xercesc.h"

using namespace boost;
using namespace std;
using namespace xercesc;

namespace foundation
{

//
// XercesCManager class implementation.
//

mutex XercesCManager::s_mutex;

bool XercesCManager::initialize()
{
    mutex::scoped_lock lock(s_mutex);

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
    mutex::scoped_lock lock(s_mutex);

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
    mutex::scoped_lock lock(s_mutex);

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
    Logger&         logger,
    const string&   input_filename)
  : m_logger(logger)
  , m_input_filename(input_filename)
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

    #ifndef WITH_EXTERNAL_XERCES
        switch (e.getOriginalExceptionCode())
        {
          // Using our modified version of Xerces-C++, we can catch the case where
          // it failed to open the input file, and print a reasonable error message.
          case XMLExcepts::Scan_CouldNotOpenSource:
            LOG_ERROR(
                m_logger,
                "failed to open %s for reading.",
                m_input_filename.c_str());
            break;

          // For now, all other errors will be reported as is.
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
        "while reading %s: %s.",
        m_input_filename.c_str(),
        transcode(e.getMessage()).c_str());
}

}   // namespace foundation
