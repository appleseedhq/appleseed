
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
#include "superlogger.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"

// Standard headers.
#include <cstdio>
#include <string>

// Platform headers.
#ifndef _WIN32
#include <unistd.h>
#endif

using namespace foundation;

namespace appleseed {
namespace common {

SuperLogger::SuperLogger()
  : m_log_target(nullptr)
{
    set_log_target(create_open_file_log_target(stderr));
}

SuperLogger::~SuperLogger()
{
    delete m_log_target;
}

ILogTarget& SuperLogger::get_log_target() const
{
    return *m_log_target;
}

void SuperLogger::set_log_target(ILogTarget* log_target)
{
    if (m_log_target)
    {
        remove_target(m_log_target);
        delete m_log_target;
    }

    m_log_target = log_target;
    add_target(m_log_target);
}

void SuperLogger::enable_message_coloring()
{
#ifndef _WIN32
    const int StdErrFileDesc = 2;
    if (!isatty(StdErrFileDesc))
        return;
#endif

    set_log_target(create_console_log_target(stderr));
}

void SuperLogger::set_verbosity_level_from_string(const char* level_name, const bool warn_if_invalid)
{
    const LogMessage::Category level = LogMessage::get_category_value(level_name);

    if (level < LogMessage::NumMessageCategories)
        set_verbosity_level(level);
    else if (warn_if_invalid)
        LOG_ERROR(*this, "invalid message verbosity level \"%s\".", level_name);
}

void SuperLogger::configure_from_settings(const Dictionary& settings)
{
    if (settings.strings().exist("message_coloring"))
    {
        const char* value = settings.get("message_coloring");
        try
        {
            if (from_string<bool>(value))
                enable_message_coloring();
        }
        catch (const ExceptionStringConversionError&)
        {
            LOG_ERROR(*this, "invalid value \"%s\" for parameter \"message_coloring\".", value);
        }
    }

    if (settings.strings().exist("message_verbosity"))
        set_verbosity_level_from_string(settings.get("message_verbosity"));
}

}   // namespace common
}   // namespace appleseed
