
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

// Interface header.
#include "commandlinehandlerbase.h"

// appleseed.common headers.
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/core/thirdparties.h"
#include "foundation/log/log.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/system.h"
#include "foundation/platform/types.h"
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif
#include "foundation/utility/commandlineparser.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <string>

using namespace foundation;
namespace bf = boost::filesystem;

namespace appleseed {
namespace common {

struct CommandLineHandlerBase::Impl
{
    const std::string                m_application_name;

    FlagOptionHandler                m_help;
    FlagOptionHandler                m_version;
    FlagOptionHandler                m_libraries;
    FlagOptionHandler                m_system;
    ValueOptionHandler<std::string>  m_message_verbosity;
    FlagOptionHandler                m_message_coloring;
    FlagOptionHandler                m_display_options;

#ifdef _WIN32
    FlagOptionHandler                m_disable_abort_dialogs;
#endif

    std::string                      m_executable_name;
    CommandLineParser                m_parser;
    ParseResults                     m_parse_results;

    explicit Impl(const char* application_name)
      : m_application_name(application_name)
    {
    }

    static const char* to_enabled_disabled(const bool value)
    {
        return value ? "enabled" : "disabled";
    }

    void print_version_information(SuperLogger& logger) const
    {
        LOG_INFO(
            logger,
            "%s, using %s version %s, %s configuration\n"
            "compiled on %s at %s using %s version %s\n"
            "copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited\n"
            "copyright (c) 2014-2019 The appleseedhq Organization\n"
            "this software is released under the MIT license (https://opensource.org/licenses/MIT).\n"
            "visit https://appleseedhq.net/ for additional information and resources.",
            m_application_name.c_str(),
            Appleseed::get_lib_name(),
            Appleseed::get_lib_version(),
            Appleseed::get_lib_configuration(),
            Appleseed::get_lib_compilation_date(),
            Appleseed::get_lib_compilation_time(),
            Compiler::get_compiler_name(),
            Compiler::get_compiler_version());

        const bool WithEmbree =
#ifdef APPLESEED_WITH_EMBREE
            true;
#else
            false;
#endif

        const bool WithSpectralSupport =
#ifdef APPLESEED_WITH_SPECTRAL_SUPPORT
            true;
#else
            false;
#endif

        const bool WithGPUSupport =
#ifdef APPLESEED_WITH_GPU
            true;
#else
            false;
#endif

        LOG_INFO(
            logger,
            "library features:\n"
            "  Instruction sets              %s\n"
            "  Embree                        %s\n"
            "  Spectral support              %s\n"
            "  GPU support                   %s",
            Appleseed::get_lib_cpu_features(),
            to_enabled_disabled(WithEmbree),
            to_enabled_disabled(WithSpectralSupport),
            to_enabled_disabled(WithGPUSupport));
    }

    static void print_libraries_information(SuperLogger& logger)
    {
        LOG_INFO(logger, "third party libraries:");

        const LibraryVersionArray versions = ThirdParties::get_versions();

        for (size_t i = 0, e = versions.size(); i < e; ++i)
        {
            const APIStringPair& version = versions[i];
            const char* lib_name = version.m_first.c_str();
            const char* lib_version = version.m_second.c_str();
            const size_t lib_name_length = strlen(lib_name);
            const std::string spacing(30 - lib_name_length, ' ');
            LOG_INFO(logger, "  %s%s%s", lib_name, spacing.c_str(), lib_version);
        }
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
    add_libraries_option();
    add_system_option();
    add_message_verbosity_option();
    add_message_coloring_option();
#ifdef _WIN32
    add_disable_abort_dialogs_option();
#endif
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

void CommandLineHandlerBase::add_libraries_option()
{
    impl->m_parser.add_option_handler(
        &impl->m_libraries
            .add_name("--libraries")
            .set_description("print third party libraries information"));
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

#ifdef _WIN32

void CommandLineHandlerBase::add_disable_abort_dialogs_option()
{
    impl->m_parser.add_option_handler(
        &impl->m_disable_abort_dialogs
            .add_name("--disable-abort-dialogs")
            .set_description("disable abort dialogs requiring user intervention"));
}

#endif

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

void CommandLineHandlerBase::parse(const int argc, char* argv[], SuperLogger& logger)
{
    impl->m_executable_name = bf::path(argv[0]).filename().string();

    impl->m_parser.parse(argc, argv, impl->m_parse_results);

    if (impl->m_help.is_set())
    {
        print_program_usage(impl->m_executable_name.c_str(), logger);
        exit(EXIT_SUCCESS);
    }

#ifdef _WIN32

    // Disable all abort dialogs as soon as possible.
    if (impl->m_disable_abort_dialogs.is_set())
        disable_all_windows_abort_dialogs();

#endif

    //
    // Apply message coloring and verbosity settings a first time just after having
    // parsed the command line.
    //

    if (impl->m_message_coloring.is_set())
        logger.enable_message_coloring();

    if (impl->m_message_verbosity.is_set())
        logger.set_verbosity_level_from_string(impl->m_message_verbosity.value().c_str(), false);
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
        impl->print_version_information(logger);

    if (impl->m_libraries.is_set())
        Impl::print_libraries_information(logger);

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

}   // namespace common
}   // namespace appleseed
