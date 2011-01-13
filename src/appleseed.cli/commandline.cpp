
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

// Interface header.
#include "commandline.h"

// appleseed.cli headers.
#include "superlogger.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/log.h"

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstdlib>

using namespace boost;
using namespace foundation;
using namespace std;

namespace appleseed {
namespace cli {

//
// CommandLine class implementation.
//

// Constructor.
CommandLine::CommandLine()
{
    //
    // General options.
    //

    // Help.
    m_help.add_name("--help");
    m_help.add_name("-h");
    m_help.set_description("print program usage and exit");
    m_parser.add_option_handler(&m_help);

    // Version.
    m_version.add_name("--version");
    m_version.add_name("-v");
    m_version.set_description("print program version");
    m_parser.add_option_handler(&m_version);

    // Message coloring.
    m_message_coloring.add_name("--message-coloring");
    m_message_coloring.set_description("enable message coloring");
    m_parser.add_option_handler(&m_message_coloring);

    // Display options.
    m_display_options.add_name("--display-options");
    m_display_options.set_description("display the recognized command line options");
    m_parser.add_option_handler(&m_display_options);

#if defined __APPLE__ || defined _WIN32

    // Display output.
    m_display_output.add_name("--display-output");
    m_display_output.set_description("display the output image");
    m_parser.add_option_handler(&m_display_output);

#endif

    // Run unit tests.
    m_run_unit_tests.add_name("--run-unit-tests");
    m_run_unit_tests.set_description("run unit tests; filter them based on the optional regular expression argument");
    m_run_unit_tests.set_min_value_count(0);
    m_run_unit_tests.set_max_value_count(1);
    m_parser.add_option_handler(&m_run_unit_tests);

    // Run unit benchmarks.
    m_run_unit_benchmarks.add_name("--run-unit-benchmarks");
    m_run_unit_benchmarks.set_description("run unit benchmarks; filter them based on the optional regular expression argument");
    m_run_unit_benchmarks.set_min_value_count(0);
    m_run_unit_benchmarks.set_max_value_count(1);
    m_parser.add_option_handler(&m_run_unit_benchmarks);

    // Verbose unit tests.
    m_verbose_unit_tests.add_name("--verbose-unit-tests");
    m_verbose_unit_tests.set_description("enable verbose mode while unit testing");
    m_parser.add_option_handler(&m_verbose_unit_tests);

    // Configuration.
    m_configuration.add_name("--configuration");
    m_configuration.add_name("-c");
    m_configuration.set_description("select the configuration");
    m_configuration.set_min_value_count(1);
    m_configuration.set_max_value_count(1);
    m_parser.add_option_handler(&m_configuration);

    // Parameters.
    m_params.add_name("--parameter");
    m_params.add_name("-p");
    m_params.set_description("define a custom parameter");
    m_params.set_syntax("name=value");
    m_params.set_flags(OptionHandler::Repeatable);
    m_params.set_min_value_count(1);
    m_params.set_max_value_count(1);
    m_parser.add_option_handler(&m_params);

    // Filenames.
    m_filenames.set_min_value_count(0);
    m_filenames.set_max_value_count(1);
    m_parser.set_default_option_handler(&m_filenames);

    //
    // Aliases for rendering options.
    //

    // Rendering threads.
    m_rendering_threads.add_name("--threads");
    m_rendering_threads.set_description("set the number of rendering threads");
    m_rendering_threads.set_syntax("n");
    m_rendering_threads.set_min_value_count(1);
    m_rendering_threads.set_max_value_count(1);
    m_parser.add_option_handler(&m_rendering_threads);

    // Output.
    m_output.add_name("--output");
    m_output.add_name("-o");
    m_output.set_description("set the name of the output file");
    m_output.set_syntax("filename");
    m_output.set_min_value_count(1);
    m_output.set_max_value_count(1);
    m_parser.add_option_handler(&m_output);

    // Resolution.
    m_resolution.add_name("--resolution");
    m_resolution.add_name("-r");
    m_resolution.set_description("set the resolution of the rendered image");
    m_resolution.set_syntax("width height");
    m_resolution.set_min_value_count(2);
    m_resolution.set_max_value_count(2);
    m_parser.add_option_handler(&m_resolution);

    // Window.
    m_window.add_name("--window");
    m_window.add_name("-w");
    m_window.set_description("restrict rendering to a given rectangle");
    m_window.set_syntax("x0 y0 x1 y1");
    m_window.set_min_value_count(4);
    m_window.set_max_value_count(4);
    m_parser.add_option_handler(&m_window);

    // Samples.
    m_samples.add_name("--samples");
    m_samples.add_name("-s");
    m_samples.set_description("set the minimum and maximum sampling rate");
    m_samples.set_syntax("min max");
    m_samples.set_min_value_count(2);
    m_samples.set_max_value_count(2);
    m_parser.add_option_handler(&m_samples);

    // Override shading.
    m_override_shading.add_name("--override-shading");
    m_override_shading.set_description("override shading with a diagnostic shader");
    m_override_shading.set_syntax("shader");
    m_override_shading.set_min_value_count(1);
    m_override_shading.set_max_value_count(1);
    m_parser.add_option_handler(&m_override_shading);
}

// Parse program command line.
void CommandLine::parse(
    const int       argc,
    const char*     argv[],
    SuperLogger&    logger)
{
    // Parse the command line.
    m_parser.parse(argc, argv);

    if (m_message_coloring.found())
        logger.enable_message_coloring();

    if (m_version.found())
        print_version_information(logger);

    // Print program usage and exit if --help was found on the command line.
    if (m_help.found())
    {
        print_program_usage(argv[0], logger);
        exit(0);
    }

    // Display the command line options that were recognized.
    if (m_display_options.found())
    {
        LOG_INFO(logger, "recognized options:");
        m_parser.print_recognized_options(logger);
    }

    // Print messages generated during command line parsing.
    m_parser.print_messages(logger);
}

void CommandLine::print_version_information(SuperLogger& logger) const
{
    LOG_INFO(
        logger,
        "appleseed.cli version %s %s, build " FMT_SIZE_T ", %s configuration\n"
        "compiled on %s at %s using %s version %s\n"
        "copyright (c) 2010-2011 francois beaune.\n"
        "this software is released under the MIT license (http://www.opensource.org/licenses/mit-license.php).\n"
        "visit http://appleseedhq.net/ for additional information and resources.",
        Appleseed::get_lib_version(),
        Appleseed::get_lib_maturity_level(),
        Appleseed::get_lib_build_number(),
        Appleseed::get_lib_configuration(),
        Appleseed::get_lib_compilation_date(),
        Appleseed::get_lib_compilation_time(),
        Compiler::get_compiler_name(),
        Compiler::get_compiler_version());
}

void CommandLine::print_program_usage(
    const char*     program_name,
    SuperLogger&    logger) const
{
    const string program_file = filesystem::path(program_name).filename();

    LOG_INFO(logger, "usage: %s [options] project.appleseed", program_file.c_str());
    LOG_INFO(logger, "options:");

    m_parser.print_usage(logger);
}

}   // namespace cli
}   // namespace appleseed
