
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

#ifndef APPLESEED_FOUNDATION_UTILITY_LOG_LOGTARGETBASE_H
#define APPLESEED_FOUNDATION_UTILITY_LOG_LOGTARGETBASE_H

// appleseed.main headers.
#include "main/dllsymbol.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/iunknown.h"
#include "foundation/utility/log/logmessage.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// The base class for log targets.
//

class DLLSYMBOL LogTargetBase
  : public IUnknown
{
  public:
    // Constructor.
    LogTargetBase();

    // Write a message.
    virtual void write(
        const LogMessage::Category  category,
        const char*                 file,
        const size_t                line,
        const char*                 message) = 0;

    // Use default formatting for all message categories.
    void reset_formatting_flags();

    // Configure the formatting for all message categories.
    void set_formatting_flags(const int flags);

    // Configure the formatting for a particular category of messages.
    // Return the previous flags values for this category.
    int set_formatting_flags(
        const LogMessage::Category  category,
        const int                   flags);

    // Return the formatting flags for a given category of messages.
    int get_formatting_flags(const LogMessage::Category category) const;

    // Return true if a given formatting flag is set for a given category of messages.
    bool has_formatting_flags(
        const LogMessage::Category  category,
        const int                   flags) const;

    // Save/restore all formatting flags.
    void save_formatting_flags(int flags[]) const;
    void restore_formatting_flags(const int flags[]);

  private:
    int m_flags[LogMessage::NumMessageCategories];
};


//
// LogTargetBase class implementation.
//

inline LogTargetBase::LogTargetBase()
{
    reset_formatting_flags();
}

inline void LogTargetBase::reset_formatting_flags()
{
    for (size_t i = 0; i < LogMessage::NumMessageCategories; ++i)
        m_flags[i] = LogMessage::DefaultFormattingFlags;
}

inline void LogTargetBase::set_formatting_flags(
    const int                   flags)
{
    for (size_t i = 0; i < LogMessage::NumMessageCategories; ++i)
        m_flags[i] = flags;
}

inline int LogTargetBase::set_formatting_flags(
    const LogMessage::Category  category,
    const int                   flags)
{
    const int previous_value = m_flags[category];

    m_flags[category] = flags;

    return previous_value;
}

inline int LogTargetBase::get_formatting_flags(
    const LogMessage::Category  category) const
{
    return m_flags[category];
}

inline bool LogTargetBase::has_formatting_flags(
    const LogMessage::Category  category,
    const int                   flags) const
{
    return (m_flags[category] & flags) != 0;
}

inline void LogTargetBase::save_formatting_flags(int flags[]) const
{
    for (size_t i = 0; i < LogMessage::NumMessageCategories; ++i)
        flags[i] = m_flags[i];
}

inline void LogTargetBase::restore_formatting_flags(const int flags[])
{
    for (size_t i = 0; i < LogMessage::NumMessageCategories; ++i)
        m_flags[i] = flags[i];
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_LOG_LOGTARGETBASE_H
