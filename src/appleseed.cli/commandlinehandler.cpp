
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "commandlinehandler.h"

// appleseed.shared headers.
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/utility/log.h"

using namespace appleseed::shared;
using namespace foundation;

namespace appleseed {
namespace cli {

CommandLineHandler::CommandLineHandler()
  : CommandLineHandlerBase("appleseed.cli")
{
    add_default_options();

    m_filename.set_min_value_count(0);
    m_filename.set_max_value_count(1);
    parser().set_default_option_handler(&m_filename);

    m_configuration.add_name("--configuration");
    m_configuration.add_name("-c");
    m_configuration.set_description("select the configuration");
    m_configuration.set_syntax("name");
    m_configuration.set_exact_value_count(1);
    parser().add_option_handler(&m_configuration);

    m_params.add_name("--parameter");
    m_params.add_name("-p");
    m_params.set_description("set a custom parameter");
    m_params.set_syntax("name=value");
    m_params.set_flags(OptionHandler::Repeatable);
    m_params.set_exact_value_count(1);
    parser().add_option_handler(&m_params);

#if defined __APPLE__ || defined _WIN32

    m_display_output.add_name("--display-output");
    m_display_output.set_description("display the output image");
    parser().add_option_handler(&m_display_output);

#endif

    m_disable_autosave.add_name("--disable-autosave");
    m_disable_autosave.set_description("disable automatic saving of rendered images");
    parser().add_option_handler(&m_disable_autosave);

    m_threads.add_name("--threads");
    m_threads.add_name("-t");
    m_threads.set_description("set the number of rendering threads");
    m_threads.set_syntax("n");
    m_threads.set_exact_value_count(1);
    parser().add_option_handler(&m_threads);

    m_output.add_name("--output");
    m_output.add_name("-o");
    m_output.set_description("set the name of the output file");
    m_output.set_syntax("filename");
    m_output.set_exact_value_count(1);
    parser().add_option_handler(&m_output);

    m_continuous_saving.add_name("--continuous-saving");
    m_continuous_saving.set_description("write tiles to disk as soon as they are rendered");
    parser().add_option_handler(&m_continuous_saving);

    m_resolution.add_name("--resolution");
    m_resolution.add_name("-r");
    m_resolution.set_description("set the resolution of the rendered image");
    m_resolution.set_syntax("width height");
    m_resolution.set_exact_value_count(2);
    parser().add_option_handler(&m_resolution);

    m_window.add_name("--window");
    m_window.add_name("-w");
    m_window.set_description("restrict rendering to a given rectangle (expressed in pixels)");
    m_window.set_syntax("x0 y0 x1 y1");
    m_window.set_exact_value_count(4);
    parser().add_option_handler(&m_window);

    m_samples.add_name("--samples");
    m_samples.add_name("-s");
    m_samples.set_description("set the minimum and maximum numbers of samples per pixel");
    m_samples.set_syntax("min max");
    m_samples.set_exact_value_count(2);
    parser().add_option_handler(&m_samples);

    m_passes.add_name("--passes");
    m_passes.set_description("set the number of rendering passes");
    m_passes.set_syntax("n");
    m_passes.set_exact_value_count(1);
    parser().add_option_handler(&m_passes);

    m_override_shading.add_name("--override-shading");
    m_override_shading.set_description("override shading with a diagnostic shader");
    m_override_shading.set_syntax("shader");
    m_override_shading.set_exact_value_count(1);
    parser().add_option_handler(&m_override_shading);

    m_select_object_instances.add_name("--select-object-instances");
    m_select_object_instances.set_description("select which object instances to include in the render using a regular expression");
    m_select_object_instances.set_syntax("regex");
    m_select_object_instances.set_exact_value_count(1);
    parser().add_option_handler(&m_select_object_instances);

    m_mplay_display.add_name("--mplay");
    m_mplay_display.set_description("use Houdini's mplay");
    parser().add_option_handler(&m_mplay_display);

    m_hrmanpipe_display.add_name("--hrmanpipe");
    m_hrmanpipe_display.set_description("use Houdini's hrmanpipe; the argument is the socket number to pass to hrmanpipe");
    m_hrmanpipe_display.set_syntax("socket");
    m_hrmanpipe_display.set_exact_value_count(1);
    parser().add_option_handler(&m_hrmanpipe_display);

    m_run_unit_tests.add_name("--run-unit-tests");
    m_run_unit_tests.set_description("run unit tests; filter them based on the optional regular expression argument");
    m_run_unit_tests.set_min_value_count(0);
    m_run_unit_tests.set_max_value_count(1);
    parser().add_option_handler(&m_run_unit_tests);

    m_run_unit_benchmarks.add_name("--run-unit-benchmarks");
    m_run_unit_benchmarks.set_description("run unit benchmarks; filter them based on the optional regular expression argument");
    m_run_unit_benchmarks.set_min_value_count(0);
    m_run_unit_benchmarks.set_max_value_count(1);
    parser().add_option_handler(&m_run_unit_benchmarks);

    m_verbose_unit_tests.add_name("--verbose-unit-tests");
    m_verbose_unit_tests.set_description("enable verbose mode while unit testing");
    parser().add_option_handler(&m_verbose_unit_tests);

    m_benchmark_mode.add_name("--benchmark-mode");
    m_benchmark_mode.set_description("enable benchmark mode");
    parser().add_option_handler(&m_benchmark_mode);
}

void CommandLineHandler::print_program_usage(
    const char*     program_name,
    SuperLogger&    logger) const
{
    SaveLogFormatterConfig save_config(logger);
    logger.set_format(LogMessage::Info, "{message}");

    LOG_INFO(logger, "usage: %s [options] project.appleseed", program_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);
}

}   // namespace cli
}   // namespace appleseed
