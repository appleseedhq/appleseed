
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Esteban Tovagliari
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
namespace studio {

CommandLineHandler::CommandLineHandler()
  : CommandLineHandlerBase("appleseed.studio")
{
    m_filenames.set_min_value_count(0);
    m_filenames.set_max_value_count(1);
    parser().set_default_option_handler(&m_filenames);

    m_render_final.add_name("--final");
    m_render_final.set_description("start final rendering");
    parser().add_option_handler(&m_render_final);
}

void CommandLineHandler::print_program_usage(
    const char*     program_name,
    SuperLogger&    logger) const
{
    SaveLogFormatterConfig save_config(logger);
    logger.set_format(LogMessage::Info, "{message}");

    LOG_INFO(logger, "usage: %s project.appleseed", program_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);
}

}   // namespace studio
}   // namespace appleseed
