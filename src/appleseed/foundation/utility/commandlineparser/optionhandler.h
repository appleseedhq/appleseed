
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

#ifndef APPLESEED_FOUNDATION_UTILITY_COMMANDLINEPARSER_OPTIONHANDLER_H
#define APPLESEED_FOUNDATION_UTILITY_COMMANDLINEPARSER_OPTIONHANDLER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

// Forward declarations.
namespace foundation    { class MessageList; }

namespace foundation
{

//
// A vector of strings.
//

typedef std::vector<std::string> StringVector;


//
// Base class for option handlers.
//

class OptionHandler
  : public NonCopyable
{
  public:
    // Flags.
    enum Flags
    {
        None        = 0,
        Hidden      = 1 << 0,   // don't list this option in program usage
        Repeatable  = 1 << 1    // this option can appear multiple times on a command line
    };

    // Constructor.
    OptionHandler();

    // Add a name for this option.
    void add_name(const std::string& name);

    // Set a description of this option.
    void set_description(const std::string& description);

    // Set the syntax for this option.
    void set_syntax(const std::string& syntax);

    // Set the flags for this option.
    void set_flags(const Flags flags);

    // Return true if this option was found on the command line.
    bool found() const;

  protected:
    friend class CommandLineParser;

    StringVector    m_names;
    std::string     m_description;
    std::string     m_syntax;
    Flags           m_flags;
    bool            m_found;

    // Return true if an argument matches any of the name of this option.
    bool match_name(const std::string& arg) const;

    // Return the maximum number of values this option can handle.
    virtual size_t get_max_value_count() const = 0;

    // Parse a vector of values.
    virtual void parse(
        const std::string&      name,
        const StringVector&     values,
        MessageList&            messages) = 0;

    // Print this option to a string.
    virtual void print(std::string& s) const = 0;
};


//
// OptionHandler class implementation.
//

inline OptionHandler::OptionHandler()
  : m_flags(None)
  , m_found(false)
{
}

inline void OptionHandler::add_name(const std::string& name)
{
    m_names.push_back(name);
}

inline void OptionHandler::set_description(const std::string& description)
{
    m_description = description;
}

inline void OptionHandler::set_syntax(const std::string& syntax)
{
    m_syntax = syntax;
}

inline void OptionHandler::set_flags(const Flags flags)
{
    m_flags = flags;
}

inline bool OptionHandler::found() const
{
    return m_found;
}

inline bool OptionHandler::match_name(const std::string& arg) const
{
    for (const_each<StringVector> i = m_names; i; ++i)
    {
        if (arg == *i)
            return true;
    }

    return false;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_COMMANDLINEPARSER_OPTIONHANDLER_H
