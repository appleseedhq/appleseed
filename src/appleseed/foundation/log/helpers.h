
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
#include "foundation/log/logger.h"
#include "foundation/log/logmessage.h"

namespace foundation
{

//
// Write a message to a logger.
//

#define LOG(logger, category, ...)                      \
    do {                                                \
        (logger).write(                                 \
            category,                                   \
            __FILE__,                                   \
            __LINE__,                                   \
            __VA_ARGS__);                               \
    } while (0)


//
// Write an info message to a logger.
//

#define LOG_INFO(logger, ...)                           \
    do {                                                \
        (logger).write(                                 \
            foundation::LogMessage::Info,               \
            __FILE__,                                   \
            __LINE__,                                   \
            __VA_ARGS__);                               \
    } while (0)


//
// Write a debug message to a logger.
//

#define LOG_DEBUG(logger, ...)                          \
    do {                                                \
        (logger).write(                                 \
            foundation::LogMessage::Debug,              \
            __FILE__,                                   \
            __LINE__,                                   \
            __VA_ARGS__);                               \
    } while (0)


//
// Write a warning message to a logger.
//

#define LOG_WARNING(logger, ...)                        \
    do {                                                \
        (logger).write(                                 \
            foundation::LogMessage::Warning,            \
            __FILE__,                                   \
            __LINE__,                                   \
            __VA_ARGS__);                               \
    } while (0)


//
// Write an error message to a logger.
//

#define LOG_ERROR(logger, ...)                          \
    do {                                                \
        (logger).write(                                 \
            foundation::LogMessage::Error,              \
            __FILE__,                                   \
            __LINE__,                                   \
            __VA_ARGS__);                               \
    } while (0)


//
// Write a fatal error message to a logger.
//

#define LOG_FATAL(logger, ...)                          \
    do {                                                \
        (logger).write(                                 \
            foundation::LogMessage::Fatal,              \
            __FILE__,                                   \
            __LINE__,                                   \
            __VA_ARGS__);                               \
    } while (0)

}   // namespace foundation
