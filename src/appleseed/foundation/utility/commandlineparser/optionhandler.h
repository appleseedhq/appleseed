
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
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

// Forward declarations.
namespace foundation    { class ParseResults; }

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
        None        = 0,            // none of the flags below are set
        Required    = 1UL << 0,     // this option is required and must appear on the command line
        Hidden      = 1UL << 1,     // don't list this option in program usage
        Repeatable  = 1UL << 2      // this option can appear multiple times on a command line
    };

    // Constructor.
    OptionHandler();

    // Add a name for this option.
    OptionHandler& add_name(const std::string& name);

    // Set a description of this option.
    OptionHandler& set_description(const std::string& description);

    // Set the syntax for this option.
    OptionHandler& set_syntax(const std::string& syntax);

    // Set the flags for this option.
    OptionHandler& set_flags(const Flags flags);

    // Return true if this option is set.
    virtual bool is_set() const;

  protected:
    friend class CommandLineParser;

    StringVector    m_names;
    std::string     m_description;
    std::string     m_syntax;
    Flags           m_flags;
    size_t          m_occurrence_count;

    // Return a description of this option.
    // The returned value may be different than what was set with set_description().
    virtual std::string get_description() const;

    // Return the maximum number of values this option can handle.
    virtual size_t get_max_value_count() const = 0;

    // Parse a vector of values.
    virtual void parse(
        const std::string&      name,
        const StringVector&     values,
        ParseResults&           results) = 0;

    // Print this option to a string.
    virtual void print(std::string& s) const = 0;

    // Return true if an argument matches any of the name of this option.
    bool match_name(const std::string& arg) const;
};


//
// OptionHandler class implementation.
//

inline OptionHandler::OptionHandler()
  : m_flags(None)
  , m_occurrence_count(0)
{
}

inline OptionHandler& OptionHandler::add_name(const std::string& name)
{
    m_names.push_back(name);
    return *this;
}

inline OptionHandler& OptionHandler::set_description(const std::string& description)
{
    m_description = description;
    return *this;
}

inline OptionHandler& OptionHandler::set_syntax(const std::string& syntax)
{
    m_syntax = syntax;
    return *this;
}

inline OptionHandler& OptionHandler::set_flags(const Flags flags)
{
    m_flags = flags;
    return *this;
}

inline bool OptionHandler::is_set() const
{
    return m_occurrence_count > 0;
}

inline std::string OptionHandler::get_description() const
{
    return m_description;
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

}   // namespace foundation
