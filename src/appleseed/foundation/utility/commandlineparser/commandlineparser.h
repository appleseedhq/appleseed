
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
#include "foundation/log/log.h"
#include "foundation/utility/commandlineparser/flagoptionhandler.h"
#include "foundation/utility/commandlineparser/messagelist.h"
#include "foundation/utility/commandlineparser/optionhandler.h"
#include "foundation/utility/commandlineparser/parseresults.h"
#include "foundation/utility/commandlineparser/valueoptionhandler.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>
#include <vector>

namespace foundation
{

//
// Command line parser.
//

class CommandLineParser
  : public NonCopyable
{
  public:
    // Constructor.
    CommandLineParser();

    // Add an option handler.
    void add_option_handler(OptionHandler* handler);

    // Set a default option handler.
    void set_default_option_handler(OptionHandler* handler);

    // Print the program usage.
    void print_usage(Logger& logger) const;

    // Parse a command line.
    // Returns true on success, or false if one or multiple errors were detected.
    void parse(
        const int       argc,
        char*           argv[],
        ParseResults&   results);

    // Print the options that were recognized during parsing.
    void print_recognized_options(Logger& logger);

  private:
    struct Option
    {
        std::string     m_name;             // option name, as found on the command line
        OptionHandler*  m_handler;          // option handler
        StringVector    m_values;           // option values
    };

    typedef std::vector<OptionHandler*> OptionHandlerVector;
    typedef std::vector<Option> OptionVector;

    OptionHandlerVector m_handlers;         // option handlers
    OptionVector        m_options;          // options
    Option              m_default_option;   // default option

    // Return the length of the longest header string.
    size_t get_max_header_length() const;

    // Find an option handler that accepts a given argument.
    OptionHandler* find_option_handler(const std::string& arg) const;

    // Return true if given handler is referenced by one of options.
    bool is_handler_used(const OptionHandler* handler) const;

    // Collect the options from a command line.
    void collect_options(
        const int       argc,
        char*           argv[],
        ParseResults&   results);

    // Check if the required options are present.
    void check_required_options(ParseResults& results);

    // Process the collected options.
    void process_options(ParseResults& results);
};


//
// CommandLineParser class implementation.
//

inline CommandLineParser::CommandLineParser()
{
    // No default option handler.
    m_default_option.m_handler = nullptr;
}

inline void CommandLineParser::add_option_handler(OptionHandler* handler)
{
    assert(handler);
    m_handlers.push_back(handler);
}

inline void CommandLineParser::set_default_option_handler(OptionHandler* handler)
{
    assert(handler);
    m_default_option.m_handler = handler;
}

inline void CommandLineParser::print_usage(Logger& logger) const
{
    const size_t max_header_length = get_max_header_length();

    // Loop over the option handlers.
    for (const_each<OptionHandlerVector> i = m_handlers; i; ++i)
    {
        // Fetch the option handler.
        const OptionHandler* handler = *i;

        // Skip hidden options.
        if (handler->m_flags & OptionHandler::Hidden)
            continue;

        std::string line;

        // Build the header string for this option.
        for (const_each<StringVector> j = handler->m_names; j; ++j)
        {
            if (j > handler->m_names.begin())
                line += ", ";
            line += *j;
        }

        // Pad with spaces so that descriptions are aligned.
        if (max_header_length > line.size())
            line += std::string(max_header_length - line.size(), ' ');

        // Append the description.
        line += "  " + handler->get_description();

        // Emit the line.
        LOG_INFO(logger, "  %s", line.c_str());
    }
}

inline void CommandLineParser::parse(
    const int       argc,
    char*           argv[],
    ParseResults&   results)
{
    collect_options(argc, argv, results);
    check_required_options(results);
    process_options(results);
}

inline void CommandLineParser::print_recognized_options(Logger& logger)
{
    size_t found_options = 0;

    for (const_each<OptionHandlerVector> i = m_handlers; i; ++i)
    {
        // Fetch the option handler.
        const OptionHandler* handler = *i;

        // Skip options that were not found on the command line.
        if (handler->m_occurrence_count == 0)
            continue;

        ++found_options;

        // Print this option.
        std::string s;
        handler->print(s);
        LOG_INFO(logger, "  %s", s.c_str());
    }

    if (m_default_option.m_handler)
    {
        for (const_each<StringVector> i = m_default_option.m_values; i; ++i)
        {
            ++found_options;
            LOG_INFO(logger, "  positional argument: %s", i->c_str());
        }
    }

    if (found_options == 0)
        LOG_INFO(logger, "  (none)");
}

inline size_t CommandLineParser::get_max_header_length() const
{
    size_t max_header_length = 0;

    // Loop over the option handlers.
    for (const_each<OptionHandlerVector> i = m_handlers; i; ++i)
    {
        // Fetch the option handler.
        const OptionHandler* handler = *i;

        // Skip hidden options.
        if (handler->m_flags & OptionHandler::Hidden)
            continue;

        // Compute the length of the header string for this option.
        size_t header_length = 0;
        for (const_each<StringVector> j = handler->m_names; j; ++j)
        {
            if (j > handler->m_names.begin())
                header_length += 2;                 // account for the separators
            header_length += j->size();
        }

        // Keep track of the longest header string.
        if (max_header_length < header_length)
            max_header_length = header_length;
    }

    return max_header_length;
}

inline OptionHandler* CommandLineParser::find_option_handler(const std::string& arg) const
{
    for (const_each<OptionHandlerVector> i = m_handlers; i; ++i)
    {
        // Fetch the handler.
        OptionHandler* handler = *i;

        // Return this handler if one of its name matches the argument.
        if (handler->match_name(arg))
            return handler;
    }

    // No handler found for this argument.
    return nullptr;
}

inline bool CommandLineParser::is_handler_used(const OptionHandler* handler) const
{
    for (const_each<OptionVector> i = m_options; i; ++i)
    {
        if (i->m_handler == handler)
            return true;
    }

    return false;
}

inline void CommandLineParser::collect_options(
    const int       argc,
    char*           argv[],
    ParseResults&   results)
{
    assert(m_options.empty());

    for (int i = 1; i < argc; ++i)
    {
        // Fetch the command line argument.
        const std::string arg = argv[i];

        // Find an option handler that accepts this argument.
        OptionHandler* handler = find_option_handler(arg);

        if (handler)
        {
            // Create a new option.
            Option option;
            option.m_name = arg;
            option.m_handler = handler;
            m_options.push_back(option);
        }
        else
        {
            if (m_options.empty() ||
                m_options.back().m_values.size() >= m_options.back().m_handler->get_max_value_count())
            {
                if (m_default_option.m_handler == nullptr ||
                    m_default_option.m_values.size() >= m_default_option.m_handler->get_max_value_count())
                {
                    // Error: unknown option.
                    results.m_messages.add(LogMessage::Warning, "unknown option: '%s'.", arg.c_str());
                    ++results.m_warnings;
                }
                else
                {
                    // Append this value to the default option.
                    m_default_option.m_values.push_back(arg);
                }
            }
            else
            {
                // Append this value to the current option.
                m_options.back().m_values.push_back(arg);
            }
        }
    }
}

inline void CommandLineParser::check_required_options(ParseResults& results)
{
    for (const_each<OptionHandlerVector> i = m_handlers; i; ++i)
    {
        OptionHandler* handler = *i;

        if ((handler->m_flags & OptionHandler::Required) != 0)
        {
            if (!is_handler_used(handler))
            {
                // Error: required option missing.
                results.m_messages.add(
                    LogMessage::Error,
                    "required option missing: '%s'.",
                    handler->m_names.front().c_str());
                ++results.m_errors;
            }
        }
    }
}

inline void CommandLineParser::process_options(ParseResults& results)
{
    for (const_each<OptionVector> i = m_options; i; ++i)
    {
        // Fetch the option.
        const Option& option = *i;

        // Let the option handler parse the option values.
        option.m_handler->parse(
            option.m_name,
            option.m_values,
            results);
    }

    // Parse the default option values.
    if (m_default_option.m_handler)
    {
        m_default_option.m_handler->parse(
            m_default_option.m_name,
            m_default_option.m_values,
            results);
    }
}

}   // namespace foundation
