
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
#include "boost/version.hpp"

// Embree headers.
#ifdef APPLESEED_WITH_EMBREE
#include <embree3/rtcore_version.h>
#endif

// IlmBase headers.
#include "foundation/platform/_beginexrheaders.h"
#include <OpenEXR/IlmBaseConfig.h>
#include "foundation/platform/_endexrheaders.h"

// libpng headers.
#include <png.h>

// OpenColorIO headers.
#include <OpenColorIO/OpenColorABI.h>

// OpenEXR headers.
#include "foundation/platform/_beginexrheaders.h"
#include <OpenEXR/OpenEXRConfig.h>
#include "foundation/platform/_endexrheaders.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include <OpenImageIO/oiioversion.h>
#include "foundation/platform/_endoiioheaders.h"

// OSL headers.
#include "foundation/platform/_beginoslheaders.h"
#include <OSL/oslversion.h>
#include "foundation/platform/_endoslheaders.h"

// Xerces-C++ headers.
#include <xercesc/util/XercesVersion.hpp>

// zlib headers.
#include <zlib.h>

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
    FlagOptionHandler           m_libraries;
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
    add_libraries_option();
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

    if (impl->m_libraries.is_set())
        print_libraries_information(logger);

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
        "copyright (c) 2014-2018 The appleseedhq Organization.\n"
        "this software is released under the MIT license (https://opensource.org/licenses/MIT).\n"
        "visit https://appleseedhq.net/ for additional information and resources.",
        impl->m_application_name.c_str(),
        Appleseed::get_lib_name(),
        Appleseed::get_lib_version(),
        Appleseed::get_lib_configuration(),
        Appleseed::get_lib_compilation_date(),
        Appleseed::get_lib_compilation_time(),
        Compiler::get_compiler_name(),
        Compiler::get_compiler_version());
}

void CommandLineHandlerBase::print_libraries_information(SuperLogger& logger) const
{
    static const char* BCDVersion = "v1.1";
    static const char* LibJpegTurboVersion = "1.3.1";
    static const char* LibTIFFVersion = "4.0.3";
    static const char* LLVMVersion = "3.4.2";
    static const char* LZ4Version = "revision 98 (July 1st, 2013)";
    static const char* SeExprVersion = "commit db9610a24401fa7198c54c8768d0484175f54172";

    LOG_INFO(logger, "this version of appleseed is using the following third party libraries:");

    LOG_INFO(logger, "  BCD %s", BCDVersion);
    LOG_INFO(logger, "  Boost %d.%d.%d", BOOST_VERSION / 100000, (BOOST_VERSION / 100) % 1000, BOOST_VERSION % 100);
#ifdef APPLESEED_WITH_EMBREE
    LOG_INFO(logger, "  Embree %s", RTC_VERSION_STRING);
#endif
    LOG_INFO(logger, "  IlmBase %s", ILMBASE_VERSION_STRING);
    LOG_INFO(logger, "  libjpeg-turbo %s", LibJpegTurboVersion);
    LOG_INFO(logger, "  libpng %s", PNG_LIBPNG_VER_STRING);
    LOG_INFO(logger, "  LibTIFF %s", LibTIFFVersion);
    LOG_INFO(logger, "  LLVM %s", LLVMVersion);
    LOG_INFO(logger, "  LZ4 %s", LZ4Version);
    LOG_INFO(logger, "  OpenColorIO %s", OCIO_VERSION);
    LOG_INFO(logger, "  OpenEXR %s", OPENEXR_VERSION_STRING);
    LOG_INFO(logger, "  OpenImageIO %s", OIIO_VERSION_STRING);
    LOG_INFO(logger, "  OpenShadingLanguage %s", OSL_LIBRARY_VERSION_STRING);
    LOG_INFO(logger, "  SeExpr %s", SeExprVersion);
    LOG_INFO(logger, "  Xerces-C++ %s", XERCES_FULLVERSIONDOT);
    LOG_INFO(logger, "  zlib %s", ZLIB_VERSION);
}

}   // namespace shared
}   // namespace appleseed
