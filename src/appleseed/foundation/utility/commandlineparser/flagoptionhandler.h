
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_UTILITY_COMMANDLINEPARSER_FLAGOPTIONHANDLER_H
#define APPLESEED_FOUNDATION_UTILITY_COMMANDLINEPARSER_FLAGOPTIONHANDLER_H

// appleseed.foundation headers.
#include "foundation/utility/commandlineparser/messagelist.h"
#include "foundation/utility/commandlineparser/optionhandler.h"
#include "foundation/utility/log.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>

namespace foundation
{

//
// Flag option handler.
//

class FlagOptionHandler
  : public OptionHandler
{
  private:
    // Return the maximum number of values this option can handle.
    virtual size_t get_max_value_count() const;

    // Parse a vector of values.
    virtual void parse(
        const std::string&      name,
        const StringVector&     values,
        MessageList&            messages);

    // Print this option to a string.
    virtual void print(std::string& s) const;
};


//
// FlagOptionHandler class implementation.
//

// Return the maximum number of values this option can handle.
inline size_t FlagOptionHandler::get_max_value_count() const
{
    return 0;
}

// Parse a vector of values.
inline void FlagOptionHandler::parse(
    const std::string&  name,
    const StringVector& values,
    MessageList&        messages)
{
    assert(values.empty());

    if (m_found && !(m_flags & OptionHandler::Repeatable))
    {
        messages.add(
            LogMessage::Error,
            "option '%s' already specified.\n",
            name.c_str());
        return;
    }

    // The option was successfully parsed.
    m_found = true;
}

// Print this option to a string.
inline void FlagOptionHandler::print(std::string& s) const
{
    // Print the first name of the option.
    s += m_names.front();
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_COMMANDLINEPARSER_FLAGOPTIONHANDLER_H
