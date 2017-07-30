
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

// Interface header.
#include "commandlinehandlerbase.h"

// appleseed.shared headers.
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/system.h"
#include "foundation/utility/commandlineparser.h"
#include "foundation/utility/log.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstdlib>
#include <string>

using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;

namespace appleseed {
namespace shared {

struct CommandLineHandlerBase::Impl
{
    const string                m_application_name;

    FlagOptionHandler           m_help;
    FlagOptionHandler           m_version;
    FlagOptionHandler           m_system;
    ValueOptionHandler<string>  m_message_verbosity;
    FlagOptionHandler           m_message_coloring;
    FlagOptionHandler           m_display_options;

    string                      m_executable_name;
    CommandLineParser           m_parser;
    ParseResults                m_parse_results;

    explicit Impl(const char* application_name)
      : m_application_name(application_name)
    {
    }
};

CommandLineHandlerBase::CommandLineHandlerBase(const char* application_name)
  : impl(new Impl(application_name))
{
}

void CommandLineHandlerBase::add_default_options()
{
    add_help_option();
    add_version_option();
    add_system_option();
    add_message_verbosity_option();
    add_message_coloring_option();
    add_display_options_option();
}

void CommandLineHandlerBase::add_help_option()
{
    impl->m_parser.add_option_handler(
        &impl->m_help
            .add_name("--help")
            .add_name("-h")
            .set_description("print program usage and exit"));
}

void CommandLineHandlerBase::add_version_option()
{
    impl->m_parser.add_option_handler(
        &impl->m_version
            .add_name("--version")
            .add_name("-v")
            .set_description("print program version"));
}

void CommandLineHandlerBase::add_system_option()
{
    impl->m_parser.add_option_handler(
        &impl->m_system
            .add_name("--system")
            .set_description("print system information"));
}

void CommandLineHandlerBase::add_message_verbosity_option()
{
    impl->m_parser.add_option_handler(
        &impl->m_message_verbosity
            .add_name("--message-verbosity")
            .set_description("set message verbosity")
            .set_syntax("level")
            .set_exact_value_count(1));
}

void CommandLineHandlerBase::add_message_coloring_option()
{
    impl->m_parser.add_option_handler(
        &impl->m_message_coloring
            .add_name("--message-coloring")
            .set_description("enable message coloring"));
}

void CommandLineHandlerBase::add_display_options_option()
{
    impl->m_parser.add_option_handler(
        &impl->m_display_options
            .add_name("--display-options")
            .set_description("display the recognized command line options"));
}

CommandLineHandlerBase::~CommandLineHandlerBase()
{
    delete impl;
}

void CommandLineHandlerBase::parse(const int argc, const char* argv[], SuperLogger& logger)
{
    impl->m_executable_name = bf::path(argv[0]).filename().string();

    impl->m_parser.parse(argc, argv, impl->m_parse_results);

    if (impl->m_help.is_set())
    {
        print_program_usage(impl->m_executable_name.c_str(), logger);
        exit(EXIT_SUCCESS);
    }

    //
    // Apply message coloring and verbosity settings a first time just after having
    // parsed the command line.
    //

    if (impl->m_message_coloring.is_set())
        logger.enable_message_coloring();

    if (impl->m_message_verbosity.is_set())
        logger.set_verbosity_level_from_string(impl->m_message_verbosity.value().c_str());
}

void CommandLineHandlerBase::apply(SuperLogger& logger)
{
    //
    // At this point, a settings file may have been loaded and applied, and message
    // coloring and verbosity settings set in parse() may have been overridden.
    // Apply them again here such that command line parameters have precedence.
    //

    if (impl->m_message_coloring.is_set())
        logger.enable_message_coloring();

    if (impl->m_message_verbosity.is_set())
        logger.set_verbosity_level_from_string(impl->m_message_verbosity.value().c_str());

    if (impl->m_version.is_set())
        print_version_information(logger);

    if (impl->m_system.is_set())
        System::print_information(logger);

    if (impl->m_display_options.is_set())
    {
        LOG_INFO(logger, "recognized options:");
        impl->m_parser.print_recognized_options(logger);
    }

    impl->m_parse_results.m_messages.print(logger);

    if (impl->m_parse_results.m_errors > 0 ||
        impl->m_parse_results.m_warnings > 0)
    {
        LOG(
            logger,
            impl->m_parse_results.m_errors > 0 ? LogMessage::Fatal : LogMessage::Warning,
            FMT_SIZE_T " error%s, " FMT_SIZE_T " warning%s encountered while parsing the command line.",
            impl->m_parse_results.m_errors,
            impl->m_parse_results.m_errors > 1 ? "s" : "",
            impl->m_parse_results.m_warnings,
            impl->m_parse_results.m_warnings > 1 ? "s" : "");
    }
}

CommandLineParser& CommandLineHandlerBase::parser()
{
    return impl->m_parser;
}

const CommandLineParser& CommandLineHandlerBase::parser() const
{
    return impl->m_parser;
}

void CommandLineHandlerBase::print_version_information(SuperLogger& logger) const
{
    LOG_INFO(
        logger,
        "%s, using %s version %s, %s configuration\n"
        "compiled on %s at %s using %s version %s\n"
        "copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited.\n"
        "copyright (c) 2014-2017 The appleseedhq Organization.\n"
        "this software is released under the MIT license (http://opensource.org/licenses/MIT).\n"
        "visit http://appleseedhq.net/ for additional information and resources.",
        impl->m_application_name.c_str(),
        Appleseed::get_lib_name(),
        Appleseed::get_lib_version(),
        Appleseed::get_lib_configuration(),
        Appleseed::get_lib_compilation_date(),
        Appleseed::get_lib_compilation_time(),
        Compiler::get_compiler_name(),
        Compiler::get_compiler_version());
}

}   // namespace shared
}   // namespace appleseed
