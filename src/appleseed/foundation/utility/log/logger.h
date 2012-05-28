
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

#ifndef APPLESEED_FOUNDATION_UTILITY_LOG_LOGGER_H
#define APPLESEED_FOUNDATION_UTILITY_LOG_LOGGER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/log/logmessage.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class LogTargetBase; }

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

//
// Logger.
//
// All methods of this class are thread-safe.
//

class FOUNDATIONDLL Logger
  : public NonCopyable
{
  public:
    // Constructor.
    Logger();

    // Destructor.
    virtual ~Logger();

    // Enable/disable logging.
    void set_enabled(const bool enabled = true);

    // Add a log target. A given log target may be added
    // multiple times. Log targets can be added at any time.
    void add_target(LogTargetBase* target);

    // Remove all instances of a given log target.
    // If the specified target cannot be found, nothing happens.
    // Log targets can be removed at any time.
    void remove_target(LogTargetBase* target);

    // Write a message. If the message category is Fatal,
    // this function will not return and the program will
    // be terminated.
    void write(
        const LogMessage::Category  category,
        const char*                 file,
        const size_t                line,
        const char*                 format, ...);

  private:
    struct Impl;
    Impl* impl;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_LOG_LOGGER_H
