
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
#include "commandlinehandler.h"

// appleseed.common headers.
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/log/log.h"

using namespace appleseed::common;
using namespace foundation;

namespace appleseed {
namespace cli {

CommandLineHandler::CommandLineHandler()
  : CommandLineHandlerBase("appleseed.cli")
{
    add_default_options();

    parser().set_default_option_handler(
        &m_filename
            .set_min_value_count(0)
            .set_max_value_count(1));

    parser().add_option_handler(
        &m_configuration
            .add_name("--configuration")
            .add_name("-c")
            .set_description("select the configuration")
            .set_syntax("name")
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_params
            .add_name("--parameter")
            .add_name("-p")
            .set_description("set a custom parameter")
            .set_syntax("name=value")
            .set_flags(OptionHandler::Repeatable)
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_threads
            .add_name("--threads")
            .add_name("-t")
            .set_description("set the number of rendering threads")
            .set_syntax("n")
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_resolution
            .add_name("--resolution")
            .add_name("-r")
            .set_description("set the resolution of the rendered image")
            .set_syntax("width height")
            .set_exact_value_count(2));

    parser().add_option_handler(
        &m_window
            .add_name("--window")
            .add_name("-w")
            .set_description("restrict rendering to a given rectangle (expressed in pixels)")
            .set_syntax("x0 y0 x1 y1")
            .set_exact_value_count(4));

    parser().add_option_handler(
        &m_noise_seed
            .add_name("--noise-seed")
            .set_description("set the noise seed")
            .set_syntax("seed")
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_samples
            .add_name("--samples")
            .add_name("-s")
            .set_description("set the number of samples per pixel")
            .set_syntax("n")
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_passes
            .add_name("--passes")
            .set_description("set the number of rendering passes")
            .set_syntax("n")
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_override_shading
            .add_name("--override-shading")
            .set_description("override shading with a diagnostic shader")
            .set_syntax("shader")
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_show_object_instances
            .add_name("--show-object-instances")
            .set_description("show object instances based on a regular expression")
            .set_syntax("regex")
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_hide_object_instances
            .add_name("--hide-object-instances")
            .set_description("hide object instances based on a regular expression")
            .set_syntax("regex")
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_output.add_name("--output")
            .add_name("-o")
            .set_description("set the name of the output file")
            .set_syntax("filename")
            .set_exact_value_count(1));

#if defined __APPLE__ || defined _WIN32
    parser().add_option_handler(
        &m_display_output
            .add_name("--display-output")
            .set_description("display the output image"));
#endif

    parser().add_option_handler(
        &m_checkpoint_create
            .add_name("--checkpoint-create")
            .set_description("write a rendering checkpoint after each pass")
            .set_syntax("filename")
            .set_min_value_count(0)
            .set_max_value_count(1));

    parser().add_option_handler(
        &m_checkpoint_resume
            .add_name("--checkpoint-resume")
            .set_description("resume rendering from a checkpoint")
            .set_syntax("filename")
            .set_min_value_count(0)
            .set_max_value_count(1));

    parser().add_option_handler(
        &m_send_to_stdout
            .add_name("--to-stdout")
            .set_description("send render to standard output"));

    parser().add_option_handler(
        &m_save_light_paths
            .add_name("--save-light-paths")
            .set_description("save recorded light paths to disk")
            .set_syntax("filename")
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_disable_autosave
            .add_name("--disable-autosave")
            .set_description("disable automatic saving of rendered images"));

    parser().add_option_handler(
        &m_run_unit_tests
            .add_name("--run-unit-tests")
            .add_name("-ut")
            .set_description("run unit tests; filter them based on the optional regular expression argument")
            .set_min_value_count(0)
            .set_max_value_count(1));

    parser().add_option_handler(
        &m_run_unit_benchmarks
            .add_name("--run-unit-benchmarks")
            .add_name("-ub")
            .set_description("run unit benchmarks; filter them based on the optional regular expression argument")
            .set_min_value_count(0)
            .set_max_value_count(1));

    parser().add_option_handler(
        &m_verbose_unit_tests
            .add_name("--verbose-unit-tests")
            .add_name("-utv")
            .set_description("enable verbose mode while unit testing"));

    parser().add_option_handler(
        &m_benchmark_mode
            .add_name("--benchmark-mode")
            .set_description("enable benchmark mode"));
}

void CommandLineHandler::print_program_usage(
    const char*     executable_name,
    SuperLogger&    logger) const
{
    SaveLogFormatterConfig save_config(logger);
    logger.set_verbosity_level(LogMessage::Info);
    logger.set_format(LogMessage::Info, "{message}");

    LOG_INFO(logger, "usage: %s [options] project.appleseed", executable_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);
}

}   // namespace cli
}   // namespace appleseed
