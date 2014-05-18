
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "appleseed.foundation.stubs.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/snprintf.h"
#include "foundation/utility/log/ilogtarget.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstdarg>
#include <cstdlib>
#include <list>
#include <vector>

using namespace std;

namespace foundation
{

//
// Internal utilities.
//

namespace
{
    class FormatEvaluator
      : public NonCopyable
    {
      public:
        FormatEvaluator(
            const LogMessage::Category  category,
            const string&               message)
          : m_category(LogMessage::get_padded_category_name(category))
          , m_message(message)
        {
        }

        string evaluate(const string& format) const
        {
            string result = format;
            result = replace(result, "{category}", m_category);
            result = replace(result, "{message}", m_message);
            return result;
        }

      private:
        const string    m_category;
        const string    m_message;
    };

    class Formatter
      : public NonCopyable
    {
      public:
        Formatter()
        {
            reset_all_formats();
        }

        void reset_all_formats()
        {
            for (size_t i = 0; i < LogMessage::NumMessageCategories; ++i)
                reset_format(static_cast<LogMessage::Category>(i));
        }

        void reset_format(const LogMessage::Category category)
        {
            set_format(category, "{category} | {message}");
        }

        void set_all_formats(const string& format)
        {
            for (size_t i = 0; i < LogMessage::NumMessageCategories; ++i)
                set_format(static_cast<LogMessage::Category>(i), format);
        }

        void set_format(
            const LogMessage::Category  category,
            const string&               format)
        {
            const string::size_type message_start = format.find("{message}");

            m_formats[category].m_format = format;
            m_formats[category].m_header_format = format.substr(0, message_start);
            m_formats[category].m_message_format =
                message_start != string::npos ? format.substr(message_start) :
                !format.empty() ? "\n" :
                string();
        }

        const string& get_format(const LogMessage::Category category) const
        {
            return m_formats[category].m_format;
        }

        const string& get_header_format(const LogMessage::Category category) const
        {
            return m_formats[category].m_header_format;
        }

        const string& get_message_format(const LogMessage::Category category) const
        {
            return m_formats[category].m_message_format;
        }

      private:
        struct Format
        {
            string  m_format;
            string  m_header_format;
            string  m_message_format;
        };

        Format m_formats[LogMessage::NumMessageCategories];
    };
}


//
// Logger class implementation.
//

struct Logger::Impl
{
    typedef list<ILogTarget*> LogTargetContainer;

    bool                m_enabled;
    LogTargetContainer  m_targets;
    vector<char>        m_message_buffer;
    Formatter           m_formatter;
};

namespace
{
    const size_t InitialBufferSize = 1024;      // in bytes
    const size_t MaxBufferSize = 1024 * 1024;   // in bytes
}

Logger::Logger()
  : impl(new Impl())
{
    impl->m_enabled = true;
    impl->m_message_buffer.resize(InitialBufferSize);
}

Logger::~Logger()
{
    delete impl;
}

void Logger::set_enabled(const bool enabled)
{
    impl->m_enabled = enabled;
}

void Logger::reset_all_formats()
{
    impl->m_formatter.reset_all_formats();
}

void Logger::reset_format(const LogMessage::Category category)
{
    impl->m_formatter.reset_format(category);
}

void Logger::set_all_formats(const char* format)
{
    impl->m_formatter.set_all_formats(format);
}

void Logger::set_format(const LogMessage::Category category, const char* format)
{
    impl->m_formatter.set_format(category, format);
}

const char* Logger::get_format(const LogMessage::Category category) const
{
    return impl->m_formatter.get_format(category).c_str();
}

void Logger::add_target(ILogTarget* target)
{
    assert(target);

    impl->m_targets.push_back(target);
}

void Logger::remove_target(ILogTarget* target)
{
    assert(target);
    impl->m_targets.remove(target);
}

namespace
{
    bool write_to_buffer(
        vector<char>&   buffer,
        const size_t    max_buffer_size,
        const char*     format,
        va_list         argptr)
    {
        while (true)
        {
            va_list argptr_copy;
            va_copy(argptr_copy, argptr);

            const size_t buffer_size = buffer.size();

            const int result =
                portable_vsnprintf(&buffer[0], buffer_size, format, argptr_copy);

            if (result < 0)
            {
                sprintf(
                    &buffer[0],
                    "(failed to format message, format string is \"%s\".)",
                    replace(format, "\n", "\\n").c_str());

                return false;
            }

            const size_t needed_buffer_size = static_cast<size_t>(result) + 1;

            if (needed_buffer_size <= buffer_size)
                return true;

            if (buffer_size >= max_buffer_size)
                return false;

            buffer.resize(min(needed_buffer_size, max_buffer_size));
        }
    }
}

void Logger::write(
    const LogMessage::Category  category,
    const char*                 file,
    const size_t                line,
    PRINTF_FMT const char*      format, ...)
{
    if (impl->m_enabled)
    {
        // Print the message into the temporary buffer.
        va_list argptr;
        va_start(argptr, format);
        write_to_buffer(impl->m_message_buffer, MaxBufferSize, format, argptr);

        // Format the header and message.
        const FormatEvaluator format_evaluator(category, &impl->m_message_buffer[0]);
        const string header = format_evaluator.evaluate(impl->m_formatter.get_header_format(category));
        const string message = format_evaluator.evaluate(impl->m_formatter.get_message_format(category));

        if (!message.empty())
        {
            // Send the header and message to all log targets.
            for (const_each<Impl::LogTargetContainer> i = impl->m_targets; i; ++i)
            {
                ILogTarget* target = *i;
                target->write(
                    category,
                    file,
                    line,
                    header.c_str(),
                    message.c_str());
            }
        }
    }

    // Terminate the application if the message category is 'Fatal'.
    if (category == LogMessage::Fatal)
        exit(EXIT_FAILURE);
}


//
// Stubs for memory allocation logging functions.
//

void log_allocation(const void* ptr, const size_t size) {}
void log_allocation_failure(const size_t size) {}
void log_deallocation(const void* ptr) {}

}   // namespace foundation
