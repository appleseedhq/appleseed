
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "foundation/utility/string.h"

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstdlib>
#include <string>

using namespace boost;
using namespace foundation;
using namespace std;

namespace appleseed {
namespace shared {

struct CommandLineHandlerBase::Impl
{
    const string        m_application_name;
    CommandLineParser   m_parser;
    FlagOptionHandler   m_help;
    FlagOptionHandler   m_version;
    FlagOptionHandler   m_system;
    FlagOptionHandler   m_message_coloring;
    FlagOptionHandler   m_display_options;

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
    add_message_coloring_option();
    add_display_options_option();
}

void CommandLineHandlerBase::add_help_option()
{
    impl->m_help.add_name("--help");
    impl->m_help.add_name("-h");
    impl->m_help.set_description("print program usage and exit");
    impl->m_parser.add_option_handler(&impl->m_help);
}

void CommandLineHandlerBase::add_version_option()
{
    impl->m_version.add_name("--version");
    impl->m_version.add_name("-v");
    impl->m_version.set_description("print program version");
    impl->m_parser.add_option_handler(&impl->m_version);
}

void CommandLineHandlerBase::add_system_option()
{
    impl->m_system.add_name("--system");
    impl->m_system.set_description("print system information");
    impl->m_parser.add_option_handler(&impl->m_system);
}

void CommandLineHandlerBase::add_message_coloring_option()
{
    impl->m_message_coloring.add_name("--message-coloring");
    impl->m_message_coloring.set_description("enable message coloring");
    impl->m_parser.add_option_handler(&impl->m_message_coloring);
}

void CommandLineHandlerBase::add_display_options_option()
{
    impl->m_display_options.add_name("--display-options");
    impl->m_display_options.set_description("display the recognized command line options");
    impl->m_parser.add_option_handler(&impl->m_display_options);
}

CommandLineHandlerBase::~CommandLineHandlerBase()
{
    delete impl;
}

void CommandLineHandlerBase::parse(
    const int       argc,
    const char*     argv[],
    SuperLogger&    logger)
{
    impl->m_parser.parse(argc, argv);

    if (impl->m_message_coloring.is_set())
        logger.enable_message_coloring();

    if (impl->m_version.is_set())
        print_version_information(logger);

    if (impl->m_system.is_set())
        print_system_information(logger);

    if (impl->m_help.is_set())
    {
        const string program_name = filesystem::path(argv[0]).filename().string();
        print_program_usage(program_name.c_str(), logger);
        exit(0);
    }

    if (impl->m_display_options.is_set())
    {
        LOG_INFO(logger, "recognized options:");
        impl->m_parser.print_recognized_options(logger);
    }

    impl->m_parser.print_messages(logger);
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
    SaveLogFormatterConfig save_config(logger);
    logger.set_format(LogMessage::Info, "{message}");

    LOG_INFO(
        logger,
        "%s, using %s version %s, %s configuration\n"
        "compiled on %s at %s using %s version %s\n"
        "copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited.\n"
        "this software is released under the MIT license (http://www.opensource.org/licenses/mit-license.php).\n"
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

void CommandLineHandlerBase::print_system_information(SuperLogger& logger)
{
    LOG_INFO(
        logger,
        "system information:\n"
        "  L1 data cache    size %s, line size %s\n"
        "  L2 cache         size %s, line size %s\n"
        "  L3 cache         size %s, line size %s\n",
        pretty_size(System::get_l1_data_cache_size()).c_str(),
        pretty_size(System::get_l1_data_cache_line_size()).c_str(),
        pretty_size(System::get_l2_cache_size()).c_str(),
        pretty_size(System::get_l2_cache_line_size()).c_str(),
        pretty_size(System::get_l3_cache_size()).c_str(),
        pretty_size(System::get_l3_cache_line_size()).c_str());
}

}   // namespace shared
}   // namespace appleseed
