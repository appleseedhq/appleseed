
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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
namespace makefluffy {

CommandLineHandler::CommandLineHandler()
  : CommandLineHandlerBase("makefluffy")
{
    add_default_options();

    m_filenames.set_exact_value_count(2);
    parser().set_default_option_handler(&m_filenames);

    parser().add_option_handler(
        &m_curves
            .add_name("--curves")
            .add_name("-n")
            .set_description("set the total number of curves to generate")
            .set_syntax("count")
            .set_exact_value_count(1)
            .set_flags(OptionHandler::Required));

    parser().add_option_handler(
        &m_length
            .add_name("--length")
            .add_name("-l")
            .set_description("set the base length of the curves")
            .set_syntax("length")
            .set_exact_value_count(1)
            .set_flags(OptionHandler::Required));

    parser().add_option_handler(
        &m_length_fuzziness
            .add_name("--length-fuzziness")
            .add_name("-lf")
            .set_description("set the amount of length variation")
            .set_syntax("fuzziness")
            .set_exact_value_count(1)
            .set_default_value(0.6));

    parser().add_option_handler(
        &m_root_width
            .add_name("--root-width")
            .add_name("-rw")
            .set_description("set the width at the curves roots")
            .set_syntax("width")
            .set_exact_value_count(1)
            .set_flags(OptionHandler::Required));

    parser().add_option_handler(
        &m_tip_width
            .add_name("--tip-width")
            .add_name("-tw")
            .set_description("set the width at the curves tips")
            .set_syntax("width")
            .set_exact_value_count(1)
            .set_flags(OptionHandler::Required));

    parser().add_option_handler(
        &m_curliness
            .add_name("--curliness")
            .add_name("-c")
            .set_description("set the amount of curliness")
            .set_syntax("curliness")
            .set_exact_value_count(1)
            .set_default_value(1.5));

    parser().add_option_handler(
        &m_presplits
            .add_name("--presplits")
            .add_name("-p")
            .set_description("set the number of presplits to apply to the curves")
            .set_syntax("presplits")
            .set_exact_value_count(1)
            .set_default_value(0));

    parser().add_option_handler(
        &m_include
            .add_name("--include")
            .add_name("-i")
            .set_description("only consider object instances whose names match a given regex")
            .set_syntax("regex")
            .set_exact_value_count(1)
            .set_default_value(".*"));          // match everything

    parser().add_option_handler(
        &m_exclude
            .add_name("--exclude")
            .add_name("-e")
            .set_description("exclude object instances whose names match a given regex")
            .set_syntax("regex")
            .set_exact_value_count(1)
            .set_default_value("/(?!)/"));      // match nothing -- http://stackoverflow.com/a/4589566/393756
}

void CommandLineHandler::print_program_usage(
    const char*     executable_name,
    SuperLogger&    logger) const
{
    SaveLogFormatterConfig save_config(logger);
    logger.set_verbosity_level(LogMessage::Info);
    logger.set_format(LogMessage::Info, "{message}");

    LOG_INFO(logger, "usage: %s [options] input.appleseed output.appleseed", executable_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);
}

}   // namespace makefluffy
}   // namespace appleseed
