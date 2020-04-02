
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
#include "logger.h"

// appleseed.foundation headers.
#include "foundation/log/ilogtarget.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/snprintf.h"
#include "foundation/platform/system.h"
#include "foundation/platform/thread.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"

// Boost headers.
#include "boost/date_time/posix_time/posix_time.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <list>
#include <map>
#include <vector>

using namespace boost::posix_time;

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
            const ptime&                datetime,
            const size_t                thread,
            const std::string&          message)
          : m_category(LogMessage::get_category_name(category))
          , m_padded_category(LogMessage::get_padded_category_name(category))
          , m_datetime(to_iso_extended_string(datetime) + 'Z')
          , m_thread(pad_left(to_string(thread), '0', 3))
          , m_process_size(pad_left(to_string(System::get_process_virtual_memory_size() / (1024 * 1024)) + " MB", ' ', 8))
          , m_message(message)
        {
        }

        std::string evaluate(const std::string& format) const
        {
            std::string result = format;
            result = replace(result, "{category}", m_category);
            result = replace(result, "{padded-category}", m_padded_category);
            result = replace(result, "{datetime-utc}", m_datetime);
            result = replace(result, "{thread}", m_thread);
            result = replace(result, "{process-size}", m_process_size);
            result = replace(result, "{message}", m_message);
            return result;
        }

      private:
        const std::string    m_category;
        const std::string    m_padded_category;
        const std::string    m_datetime;
        const std::string    m_thread;
        const std::string    m_process_size;
        const std::string    m_message;
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
            set_format(category, "{datetime-utc} <{thread}> {process-size} {padded-category} | {message}");
        }

        void set_all_formats(const std::string& format)
        {
            for (size_t i = 0; i < LogMessage::NumMessageCategories; ++i)
                set_format(static_cast<LogMessage::Category>(i), format);
        }

        void set_format(
            const LogMessage::Category  category,
            const std::string&          format)
        {
            const std::string::size_type message_start = format.find("{message}");

            m_formats[category].m_format = format;
            m_formats[category].m_header_format = format.substr(0, message_start);
            m_formats[category].m_message_format =
                message_start != std::string::npos ? format.substr(message_start) :
                !format.empty() ? "\n" :
                std::string();
        }

        const std::string& get_format(const LogMessage::Category category) const
        {
            return m_formats[category].m_format;
        }

        const std::string& get_header_format(const LogMessage::Category category) const
        {
            return m_formats[category].m_header_format;
        }

        const std::string& get_message_format(const LogMessage::Category category) const
        {
            return m_formats[category].m_message_format;
        }

      private:
        struct Format
        {
            std::string  m_format;
            std::string  m_header_format;
            std::string  m_message_format;
        };

        Format m_formats[LogMessage::NumMessageCategories];
    };

    class ThreadMap
      : public NonCopyable
    {
      public:
        ThreadMap()
          : m_thread_count(0)
        {
        }

        size_t thread_id_to_int(const boost::thread::id id)
        {
            const ThreadIdToIntMap::const_iterator i = m_thread_id_to_int.find(id);

            if (i != m_thread_id_to_int.end())
                return i->second;

            m_thread_id_to_int[id] = ++m_thread_count;

            return m_thread_count;
        }

      private:
        typedef std::map<boost::thread::id, size_t> ThreadIdToIntMap;

        size_t              m_thread_count;
        ThreadIdToIntMap    m_thread_id_to_int;
    };
}


//
// Logger class implementation.
//

struct Logger::Impl
{
    typedef std::list<ILogTarget*> LogTargetContainer;

    boost::mutex            m_mutex;
    bool                    m_enabled;
    LogMessage::Category    m_verbosity_level;
    LogTargetContainer      m_targets;
    std::vector<char>       m_message_buffer;
    ThreadMap               m_thread_map;
    Formatter               m_formatter;
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
    impl->m_verbosity_level = LogMessage::Info;
    impl->m_message_buffer.resize(InitialBufferSize);
}

Logger::~Logger()
{
    delete impl;
}

void Logger::initialize_from(const Logger& source)
{
    boost::mutex::scoped_lock source_lock(source.impl->m_mutex);
    boost::mutex::scoped_lock this_lock(impl->m_mutex);

    impl->m_enabled = source.impl->m_enabled;
    impl->m_verbosity_level = source.impl->m_verbosity_level;

    impl->m_targets.clear();
    for (const_each<Impl::LogTargetContainer> i = source.impl->m_targets; i; ++i)
        impl->m_targets.push_back(*i);

    for (size_t i = 0; i < LogMessage::NumMessageCategories; ++i)
    {
        const LogMessage::Category category = static_cast<LogMessage::Category>(i);
        impl->m_formatter.set_format(category, source.impl->m_formatter.get_format(category));
    }
}

void Logger::set_enabled(const bool enabled)
{
    boost::mutex::scoped_lock lock(impl->m_mutex);
    impl->m_enabled = enabled;
}

void Logger::set_verbosity_level(const LogMessage::Category level)
{
    boost::mutex::scoped_lock lock(impl->m_mutex);
    impl->m_verbosity_level = level;
}

LogMessage::Category Logger::get_verbosity_level() const
{
    boost::mutex::scoped_lock lock(impl->m_mutex);
    return impl->m_verbosity_level;
}

void Logger::reset_all_formats()
{
    boost::mutex::scoped_lock lock(impl->m_mutex);
    impl->m_formatter.reset_all_formats();
}

void Logger::reset_format(const LogMessage::Category category)
{
    boost::mutex::scoped_lock lock(impl->m_mutex);
    impl->m_formatter.reset_format(category);
}

void Logger::set_all_formats(const char* format)
{
    boost::mutex::scoped_lock lock(impl->m_mutex);
    impl->m_formatter.set_all_formats(format);
}

void Logger::set_format(const LogMessage::Category category, const char* format)
{
    boost::mutex::scoped_lock lock(impl->m_mutex);
    impl->m_formatter.set_format(category, format);
}

const char* Logger::get_format(const LogMessage::Category category) const
{
    boost::mutex::scoped_lock lock(impl->m_mutex);
    return impl->m_formatter.get_format(category).c_str();
}

void Logger::add_target(ILogTarget* target)
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    assert(target);
    impl->m_targets.push_back(target);
}

void Logger::remove_target(ILogTarget* target)
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    assert(target);
    impl->m_targets.remove(target);
}

namespace
{
    bool write_to_buffer(
        std::vector<char>&   buffer,
        const size_t         max_buffer_size,
        const char*          format,
        va_list              argptr)
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

            buffer.resize(std::min(needed_buffer_size, max_buffer_size));
        }
    }
}

void Logger::write(
    const LogMessage::Category          category,
    const char*                         file,
    const size_t                        line,
    APPLESEED_PRINTF_FMT const char*    format, ...)
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    if (category < impl->m_verbosity_level)
        return;

    LogMessage::Category effective_category = category;

    if (impl->m_enabled)
    {
        // Format the message into the temporary buffer.
        va_list argptr;
        va_start(argptr, format);
        const bool formatting_succeeded =
            write_to_buffer(impl->m_message_buffer, MaxBufferSize, format, argptr);

        // If formatting failed, print the message as an error.
        if (!formatting_succeeded)
            effective_category = LogMessage::Error;

        // Retrieve the current UTC time.
        const ptime datetime(microsec_clock::universal_time());

        // Format the header and message.
        const size_t thread = impl->m_thread_map.thread_id_to_int(boost::this_thread::get_id());
        const FormatEvaluator format_evaluator(effective_category, datetime, thread, &impl->m_message_buffer[0]);
        const std::string header = format_evaluator.evaluate(impl->m_formatter.get_header_format(effective_category));
        std::string message = format_evaluator.evaluate(impl->m_formatter.get_message_format(effective_category));

        // Remove trailing newline characters from the message.
        message = trim_right(message, "\n");

        if (!message.empty())
        {
            // Send the header and message to all log targets.
            for (const_each<Impl::LogTargetContainer> i = impl->m_targets; i; ++i)
            {
                ILogTarget* target = *i;
                target->write(
                    effective_category,
                    file,
                    line,
                    header.c_str(),
                    message.c_str());
            }
        }
    }

    // Terminate the application if the message category is 'Fatal'.
    if (effective_category == LogMessage::Fatal)
        exit(EXIT_FAILURE);
}

}   // namespace foundation
