
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
#include "foundation/log/logmessage.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <string>

// Forward declarations.
namespace foundation    { class ILogTarget; }

namespace foundation
{

//
// Logger.
//
// All methods of this class are thread-safe.
//

class APPLESEED_DLLSYMBOL Logger
  : public NonCopyable
{
  public:
    // Constructor.
    Logger();

    // Destructor.
    virtual ~Logger();

    // Copy all settings from a given logger to this one.
    // Existing log targets are removed (but not deleted).
    // Log targets of the source logger are added to this one.
    // If the other logger is disabled, this one will be too.
    void initialize_from(const Logger& source);

    // Enable/disable logging.
    void set_enabled(const bool enabled = true);

    // Set/get the verbosity level.
    void set_verbosity_level(const LogMessage::Category level);
    LogMessage::Category get_verbosity_level() const;

    // Reset the format string for all message categories.
    void reset_all_formats();

    // Reset the format string for a given message category.
    void reset_format(const LogMessage::Category category);

    // Set the format string for all message categories.
    void set_all_formats(const char* format);
    void set_all_formats(const std::string& format);

    // Set the format string for a given message category.
    void set_format(const LogMessage::Category category, const char* format);
    void set_format(const LogMessage::Category category, const std::string& format);

    // Return the format string of a given message category.
    const char* get_format(const LogMessage::Category category) const;

    // Add a log target. A given log target may be added
    // multiple times. Log targets can be added at any time.
    void add_target(ILogTarget* target);

    // Remove all instances of a given log target.
    // If the specified target cannot be found, nothing happens.
    // Log targets can be removed at any time.
    void remove_target(ILogTarget* target);

    // Write a message. If the message category is Fatal,
    // this function will not return and the program will
    // be terminated.
    void write(
        const LogMessage::Category          category,
        const char*                         file,
        const size_t                        line,
        APPLESEED_PRINTF_FMT const char*    format, ...)
        APPLESEED_PRINTF_FMT_ATTR(5, 6);

  private:
    struct Impl;
    Impl* impl;
};


//
// Logger class implementation.
//

inline void Logger::set_all_formats(const std::string& format)
{
    set_all_formats(format.c_str());
}

inline void Logger::set_format(const LogMessage::Category category, const std::string& format)
{
    set_format(category, format.c_str());
}

}   // namespace foundation
