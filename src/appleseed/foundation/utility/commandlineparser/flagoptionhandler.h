
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
#include "foundation/log/log.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/commandlineparser/messagelist.h"
#include "foundation/utility/commandlineparser/optionhandler.h"
#include "foundation/utility/commandlineparser/parseresults.h"

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
    size_t get_max_value_count() const override;

    // Parse a vector of values.
    void parse(
        const std::string&      name,
        const StringVector&     values,
        ParseResults&           results) override;

    // Print this option to a string.
    void print(std::string& s) const override;
};


//
// FlagOptionHandler class implementation.
//

inline size_t FlagOptionHandler::get_max_value_count() const
{
    return 0;
}

inline void FlagOptionHandler::parse(
    const std::string&  name,
    const StringVector& values,
    ParseResults&       results)
{
    assert(values.empty());

    if ((m_flags & OptionHandler::Repeatable) == 0 && m_occurrence_count == 1)
    {
        // Error: option already specified.
        results.m_messages.add(
            LogMessage::Error,
            "flag '%s' already specified, ignoring all extra occurrences.",
            name.c_str());
        ++results.m_errors;
        ++m_occurrence_count;
        return;
    }

    // The option was successfully parsed.
    ++m_occurrence_count;
}

inline void FlagOptionHandler::print(std::string& s) const
{
    // Print the first name of the option.
    s += m_names.front();
}

}   // namespace foundation
