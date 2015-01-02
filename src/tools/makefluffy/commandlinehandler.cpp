
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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
using namespace std;

namespace appleseed {
namespace makefluffy {

CommandLineHandler::CommandLineHandler()
  : CommandLineHandlerBase("makefluffy")
{
    add_default_options();

    m_filenames.set_exact_value_count(2);
    parser().set_default_option_handler(&m_filenames);

    m_curves.add_name("--curves");
    m_curves.add_name("-n");
    m_curves.set_description("set the total number of curves to generate");
    m_curves.set_syntax("count");
    m_curves.set_exact_value_count(1);
    m_curves.set_flags(OptionHandler::Required);
    parser().add_option_handler(&m_curves);

    m_length.add_name("--length");
    m_length.add_name("-l");
    m_length.set_description("set the base length of the curves");
    m_length.set_syntax("length");
    m_length.set_exact_value_count(1);
    m_length.set_flags(OptionHandler::Required);
    parser().add_option_handler(&m_length);

    m_length_fuzziness.add_name("--length-fuzziness");
    m_length_fuzziness.add_name("-lf");
    m_length_fuzziness.set_description("set the amount of length variation");
    m_length_fuzziness.set_syntax("fuzziness");
    m_length_fuzziness.set_exact_value_count(1);
    m_length_fuzziness.set_default_value(0.6);
    parser().add_option_handler(&m_length_fuzziness);

    m_root_width.add_name("--root-width");
    m_root_width.add_name("-rw");
    m_root_width.set_description("set the width at the curves roots");
    m_root_width.set_syntax("width");
    m_root_width.set_exact_value_count(1);
    m_root_width.set_flags(OptionHandler::Required);
    parser().add_option_handler(&m_root_width);

    m_tip_width.add_name("--tip-width");
    m_tip_width.add_name("-tw");
    m_tip_width.set_description("set the width at the curves tips");
    m_tip_width.set_syntax("width");
    m_tip_width.set_exact_value_count(1);
    m_tip_width.set_flags(OptionHandler::Required);
    parser().add_option_handler(&m_tip_width);

    m_curliness.add_name("--curliness");
    m_curliness.add_name("-c");
    m_curliness.set_description("set the amount of curliness");
    m_curliness.set_syntax("curliness");
    m_curliness.set_exact_value_count(1);
    m_curliness.set_default_value(1.5);
    parser().add_option_handler(&m_curliness);

    m_presplits.add_name("--presplits");
    m_presplits.add_name("-p");
    m_presplits.set_description("set the number of presplits to apply to the curves");
    m_presplits.set_syntax("presplits");
    m_presplits.set_exact_value_count(1);
    m_presplits.set_default_value(0);
    parser().add_option_handler(&m_presplits);

    m_include.add_name("--include");
    m_include.add_name("-i");
    m_include.set_description("only consider object instances whose names match a given regex");
    m_include.set_syntax("regex");
    m_include.set_exact_value_count(1);
    m_include.set_default_value(".*");          // match everything
    parser().add_option_handler(&m_include);

    m_exclude.add_name("--exclude");
    m_exclude.add_name("-e");
    m_exclude.set_description("exclude object instances whose names match a given regex");
    m_exclude.set_syntax("regex");
    m_exclude.set_exact_value_count(1);
    m_exclude.set_default_value("/(?!)/");      // match nothing -- http://stackoverflow.com/a/4589566/393756
    parser().add_option_handler(&m_exclude);
}

void CommandLineHandler::print_program_usage(
    const char*     program_name,
    SuperLogger&    logger) const
{
    SaveLogFormatterConfig save_config(logger);
    logger.set_format(LogMessage::Info, "{message}");

    LOG_INFO(logger, "usage: %s [options] input.appleseed output.appleseed", program_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);
}

}   // namespace makefluffy
}   // namespace appleseed
