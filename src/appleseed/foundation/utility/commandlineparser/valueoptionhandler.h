
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

#ifndef APPLESEED_FOUNDATION_UTILITY_COMMANDLINEPARSER_VALUEOPTIONHANDLER_H
#define APPLESEED_FOUNDATION_UTILITY_COMMANDLINEPARSER_VALUEOPTIONHANDLER_H

// appleseed.foundation headers.
#include "foundation/utility/commandlineparser/messagelist.h"
#include "foundation/utility/commandlineparser/optionhandler.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/log.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>
#include <string>
#include <vector>

namespace foundation
{

//
// Handler for an option with one or multiple values.
//

template <typename T>
class ValueOptionHandler
  : public OptionHandler
{
  public:
    // Value and value vector types.
    typedef T ValueType;
    typedef std::vector<ValueType> ValueVectorType;

    // Constructor.
    ValueOptionHandler();

    // Set minimum/maximum number of values.
    void set_min_value_count(const size_t min_count);
    void set_max_value_count(const size_t max_count);
    void set_exact_value_count(const size_t count);

    // Set default values.
    void set_default_values(const ValueVectorType& default_values);

    // Return the string values of this option.
    const StringVector& string_values() const;

    // Return the values of this option.
    const ValueVectorType& values() const;

    // Return true if this option is set.
    virtual bool is_set() const;

  private:
    size_t              m_min_value_count;
    size_t              m_max_value_count;

    ValueVectorType     m_default_values;

    StringVector        m_string_values;
    ValueVectorType     m_values;

    // Return a description of this option.
    virtual std::string get_description() const;

    // Return the maximum number of values this option can handle.
    virtual size_t get_max_value_count() const;

    // Parse a vector of values.
    virtual void parse(
        const std::string&  name,
        const StringVector& vals,
        MessageList&        messages);

    // Print this option to a string.
    virtual void print(std::string& s) const;
};


//
// ValueOptionHandler class implementation.
//

template <typename T>
inline ValueOptionHandler<T>::ValueOptionHandler()
  : m_min_value_count(0)
  , m_max_value_count(std::numeric_limits<size_t>::max())
{
}

template <typename T>
inline void ValueOptionHandler<T>::set_min_value_count(const size_t min_count)
{
    m_min_value_count = min_count;
}

template <typename T>
inline void ValueOptionHandler<T>::set_max_value_count(const size_t max_count)
{
    m_max_value_count = max_count;
}

template <typename T>
inline void ValueOptionHandler<T>::set_exact_value_count(const size_t count)
{
    m_min_value_count = count;
    m_max_value_count = count;
}

template <typename T>
inline void ValueOptionHandler<T>::set_default_values(const ValueVectorType& default_values)
{
    m_default_values = default_values;
    m_values = default_values;

    m_string_values.clear();

    for (size_t i = 0; i < m_default_values.size(); ++i)
        m_string_values.push_back(to_string(m_values[i]));
}

template <typename T>
inline const StringVector& ValueOptionHandler<T>::string_values() const
{
    return m_string_values;
}

template <typename T>
inline const std::vector<T>& ValueOptionHandler<T>::values() const
{
    return m_values;
}

template <typename T>
inline bool ValueOptionHandler<T>::is_set() const
{
    return !m_default_values.empty() || m_occurrence_count > 0;
}

template <typename T>
std::string ValueOptionHandler<T>::get_description() const
{
    std::string result = m_description;

    if (!m_default_values.empty())
        result += " (default: " + to_string(m_default_values) + ")";

    return result;
}

template <typename T>
inline size_t ValueOptionHandler<T>::get_max_value_count() const
{
    return m_max_value_count;
}

template <typename T>
void ValueOptionHandler<T>::parse(
    const std::string&  name,
    const StringVector& vals,
    MessageList&        messages)
{
    assert(vals.size() <= m_max_value_count);

    if ((m_flags & OptionHandler::Repeatable) == 0 && m_occurrence_count == 1)
    {
        // Error: option already specified.
        messages.add(
            LogMessage::Error,
            "option '%s' already specified, ignoring all extra occurrences.\n",
            name.c_str());
        ++m_occurrence_count;
        return;
    }

    if (vals.size() < m_min_value_count)
    {
        // Error: wrong number of option values.
        std::string error =
            name.empty()
                ? "wrong number of positional arguments"
                : "option '" + name + "': wrong number of values";
        error += ", expected ";
        error += m_min_value_count == m_max_value_count ? "exactly " : "at least ";
        error += to_string(m_min_value_count) + " but ";
        error += to_string(vals.size()) + " given";
        if (!m_syntax.empty())
            error += " (syntax: " + name + " " + m_syntax + ")";
        error += ".";
        messages.add(LogMessage::Error, "%s\n", error.c_str());
        return;
    }

    // Parse option values.
    ValueVectorType parsed_values;
    for (const_each<StringVector> i = vals; i; ++i)
    {
        try
        {
            parsed_values.push_back(from_string<T>(*i));
        }
        catch (const ExceptionStringConversionError&)
        {
            // Error: value type mismatch.
            std::string error =
                name.empty()
                    ? "positional arguments: "
                    : "option '" + name + "': ";
            error += "type mismatch.";
            if (!m_syntax.empty())
                error += " syntax: " + name + " " + m_syntax + ".";
            messages.add(LogMessage::Error, "%s\n", error.c_str());
            return;
        }
    }

    // Copy string values locally.
    m_values = parsed_values;
    m_string_values = vals;

    // The option was successfully parsed.
    ++m_occurrence_count;
}

template <typename T>
void ValueOptionHandler<T>::print(std::string& s) const
{
    // Print the first name of the option.
    s += m_names.front() + " ";

    // Print the values of the option.
    for (const_each<ValueVectorType> i = m_values; i; ++i)
    {
        if (i > m_values.begin())
            s += " ";

        // Print this value.
        s += to_string(*i);
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_COMMANDLINEPARSER_VALUEOPTIONHANDLER_H
