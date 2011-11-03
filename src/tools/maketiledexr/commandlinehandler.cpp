
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
#include "commandlinehandler.h"

// appleseed.shared headers.
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/utility/log.h"

// Standard headers.
#include <cstdlib>

using namespace appleseed::shared;
using namespace foundation;
using namespace std;

namespace appleseed {
namespace maketiledexr {

CommandLineHandler::CommandLineHandler()
  : CommandLineHandlerBase("maketiledexr")
{
    m_progress_messages.add_name("--progress");
    m_progress_messages.add_name("-p");
    m_progress_messages.set_description("print progress messages");
    parser().add_option_handler(&m_progress_messages);

    m_filenames.set_min_value_count(2);
    m_filenames.set_max_value_count(2);
    parser().set_default_option_handler(&m_filenames);

    m_tile_size.add_name("--tile-size");
    m_tile_size.add_name("-t");
    m_tile_size.set_description("set the size of the tiles in the output image");
    m_tile_size.set_syntax("width height");
    m_tile_size.set_min_value_count(2);
    m_tile_size.set_max_value_count(2);
    parser().add_option_handler(&m_tile_size);
}

void CommandLineHandler::parse(
    const int       argc,
    const char*     argv[],
    SuperLogger&    logger)
{
    CommandLineHandlerBase::parse(argc, argv, logger);

    if (!m_filenames.found())
        exit(0);
}

void CommandLineHandler::print_program_usage(
    const char*     program_name,
    SuperLogger&    logger) const
{
    LogTargetBase& log_target = logger.get_log_target();

    const LogMessage::FormattingFlags old_flags =
        log_target.set_formatting_flags(LogMessage::Info, LogMessage::DisplayMessage);

    LOG_INFO(logger, "usage: %s [options] input output.exr", program_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);

    log_target.set_formatting_flags(LogMessage::Info, old_flags);
}

}   // namespace maketiledexr
}   // namespace appleseed
