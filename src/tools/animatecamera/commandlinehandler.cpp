
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
#include "foundation/utility/makevector.h"

// Standard headers.
#include <cstdlib>

using namespace appleseed::shared;
using namespace foundation;
using namespace std;

namespace appleseed {
namespace animatecamera {

CommandLineHandler::CommandLineHandler()
  : CommandLineHandlerBase("animatecamera")
{
    m_filenames.set_exact_value_count(2);
    parser().set_default_option_handler(&m_filenames);

    m_frame_count.add_name("--frames");
    m_frame_count.add_name("-f");
    m_frame_count.set_description("set the number of frames in the animation");
    m_frame_count.set_syntax("count");
    m_frame_count.set_exact_value_count(1);
    m_frame_count.set_default_values(make_vector(20));
    parser().add_option_handler(&m_frame_count);

    m_camera_distance.add_name("--distance");
    m_camera_distance.add_name("-d");
    m_camera_distance.set_description("set the normalized distance from the camera to the scene");
    m_camera_distance.set_syntax("scalar");
    m_camera_distance.set_exact_value_count(1);
    m_camera_distance.set_default_values(make_vector(10.0));
    parser().add_option_handler(&m_camera_distance);

    m_camera_elevation.add_name("--elevation");
    m_camera_elevation.add_name("-e");
    m_camera_elevation.set_description("set the normalized elevation of the camera");
    m_camera_elevation.set_syntax("scalar");
    m_camera_elevation.set_exact_value_count(1);
    m_camera_elevation.set_default_values(make_vector(2.0));
    parser().add_option_handler(&m_camera_elevation);
}

void CommandLineHandler::parse(
    const int       argc,
    const char*     argv[],
    SuperLogger&    logger)
{
    CommandLineHandlerBase::parse(argc, argv, logger);

    if (!m_filenames.is_set())
        exit(0);
}

void CommandLineHandler::print_program_usage(
    const char*     program_name,
    SuperLogger&    logger) const
{
    LogTargetBase& log_target = logger.get_log_target();

    const LogMessage::FormattingFlags old_flags =
        log_target.set_formatting_flags(LogMessage::Info, LogMessage::DisplayMessage);

    LOG_INFO(logger, "usage: %s [options] master.appleseed output.appleseed", program_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);

    log_target.set_formatting_flags(LogMessage::Info, old_flags);
}

}   // namespace animatecamera
}   // namespace appleseed
