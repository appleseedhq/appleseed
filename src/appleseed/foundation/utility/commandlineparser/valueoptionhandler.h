
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
#include "foundation/string/string.h"
#include "foundation/utility/commandlineparser/messagelist.h"
#include "foundation/utility/commandlineparser/optionhandler.h"
#include "foundation/utility/commandlineparser/parseresults.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>
#include <string>
#include <vector>

// Unit test case declarations.
DECLARE_TEST_CASE(Foundation_Utility_CommandLineParser_ValueOptionHandler, Parse_GivenMultipleInvocations_AccumulateValues);

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

    // Add a name for this option.
    ValueOptionHandler<T>& add_name(const std::string& name);

    // Set a description of this option.
    ValueOptionHandler<T>& set_description(const std::string& description);

    // Set the syntax for this option.
    ValueOptionHandler<T>& set_syntax(const std::string& syntax);

    // Set the flags for this option.
    ValueOptionHandler<T>& set_flags(const Flags flags);

    // Set minimum/maximum number of values.
    ValueOptionHandler<T>& set_min_value_count(const size_t min_count);
    ValueOptionHandler<T>& set_max_value_count(const size_t max_count);
    ValueOptionHandler<T>& set_exact_value_count(const size_t count);

    // Set default values.
    ValueOptionHandler<T>& set_default_value(const ValueType& default_value);
    ValueOptionHandler<T>& set_default_values(const ValueVectorType& default_values);

    // Return the values of this option.
    const ValueType& value() const;
    const ValueVectorType& values() const;

    // Return true if this option is set.
    bool is_set() const override;

  private:
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_CommandLineParser_ValueOptionHandler, Parse_GivenMultipleInvocations_AccumulateValues);

    size_t              m_min_value_count;
    size_t              m_max_value_count;
    ValueVectorType     m_default_values;
    ValueVectorType     m_values;

    // Return a description of this option.
    std::string get_description() const override;

    // Return the maximum number of values this option can handle.
    size_t get_max_value_count() const override;

    // Parse a vector of values.
    void parse(
        const std::string&  name,
        const StringVector& vals,
        ParseResults&       results) override;

    // Print this option to a string.
    void print(std::string& s) const override;
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
inline ValueOptionHandler<T>& ValueOptionHandler<T>::add_name(const std::string& name)
{
    OptionHandler::add_name(name);
    return *this;
}

template <typename T>
inline ValueOptionHandler<T>& ValueOptionHandler<T>::set_description(const std::string& description)
{
    OptionHandler::set_description(description);
    return *this;
}

template <typename T>
inline ValueOptionHandler<T>& ValueOptionHandler<T>::set_syntax(const std::string& syntax)
{
    OptionHandler::set_syntax(syntax);
    return *this;
}

template <typename T>
inline ValueOptionHandler<T>& ValueOptionHandler<T>::set_flags(const Flags flags)
{
    OptionHandler::set_flags(flags);
    return *this;
}

template <typename T>
inline ValueOptionHandler<T>& ValueOptionHandler<T>::set_min_value_count(const size_t min_count)
{
    m_min_value_count = min_count;
    return *this;
}

template <typename T>
inline ValueOptionHandler<T>& ValueOptionHandler<T>::set_max_value_count(const size_t max_count)
{
    m_max_value_count = max_count;
    return *this;
}

template <typename T>
inline ValueOptionHandler<T>& ValueOptionHandler<T>::set_exact_value_count(const size_t count)
{
    m_min_value_count = count;
    m_max_value_count = count;
    return *this;
}

template <typename T>
inline ValueOptionHandler<T>& ValueOptionHandler<T>::set_default_value(const ValueType& default_value)
{
    m_default_values = make_vector(default_value);
    return *this;
}

template <typename T>
inline ValueOptionHandler<T>& ValueOptionHandler<T>::set_default_values(const ValueVectorType& default_values)
{
    m_default_values = default_values;
    return *this;
}

template <typename T>
inline const T& ValueOptionHandler<T>::value() const
{
    return !m_values.empty() ? m_values.front() : m_default_values.front();
}

template <typename T>
inline const std::vector<T>& ValueOptionHandler<T>::values() const
{
    return !m_values.empty() ? m_values : m_default_values;
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
    ParseResults&       results)
{
    assert(vals.size() <= m_max_value_count);

    if ((m_flags & OptionHandler::Repeatable) == 0 && m_occurrence_count == 1)
    {
        // Error: option already specified.
        results.m_messages.add(
            LogMessage::Warning,
            "option '%s' already specified, ignoring all extra occurrences.",
            name.c_str());
        ++results.m_warnings;
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
        results.m_messages.add(LogMessage::Error, "%s", error.c_str());
        ++results.m_errors;
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
            results.m_messages.add(LogMessage::Error, "%s", error.c_str());
            ++results.m_errors;
            return;
        }
    }

    // Store values.
    m_values.insert(m_values.end(), parsed_values.begin(), parsed_values.end());

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

}   // namespace foundation
