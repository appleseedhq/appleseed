
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

// appleseed.shared headers.
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/utility/log.h"
#include "foundation/utility/makevector.h"

using namespace appleseed::shared;
using namespace foundation;
using namespace std;

namespace appleseed {
namespace animatecamera {

CommandLineHandler::CommandLineHandler()
  : CommandLineHandlerBase("animatecamera")
{
    add_default_options();

    m_filenames.set_exact_value_count(2);
    parser().set_default_option_handler(&m_filenames);

    parser().add_option_handler(
        &m_animation_path
            .add_name("--animation-path")
            .add_name("-a")
            .set_description("load an animation path file")
            .set_syntax("filename.txt")
            .set_exact_value_count(1));

    parser().add_option_handler(
        &m_3dsmax_mode
            .add_name("--3dsmax")
            .set_description("assume the animation path file uses Autodesk 3ds Max's coordinate system"));

    parser().add_option_handler(
        &m_output_format
            .add_name("--output-format")
            .add_name("-o")
            .set_description("set the format of the output frames")
            .set_syntax("format")
            .set_exact_value_count(1)
            .set_default_value("exr"));

    parser().add_option_handler(
        &m_frame_count
            .add_name("--frame-count")
            .add_name("-f")
            .set_description("set the number of frames in the animation")
            .set_syntax("count")
            .set_exact_value_count(1)
            .set_default_value(20));

    parser().add_option_handler(
        &m_part_count
            .add_name("--part-count")
            .add_name("-p")
            .set_description("split the render script in that many parts")
            .set_syntax("count")
            .set_exact_value_count(1)
            .set_default_value(1));

    parser().add_option_handler(
        &m_camera_target
            .add_name("--target")
            .add_name("-t")
            .set_description("set the target of the camera relatively to the center of the scene")
            .set_syntax("x y z")
            .set_exact_value_count(3)
            .set_default_values(make_vector(0.0, 0.0, 0.0)));

    parser().add_option_handler(
        &m_camera_distance
            .add_name("--distance")
            .add_name("-d")
            .set_description("set the normalized distance from the camera to the scene")
            .set_syntax("scalar")
            .set_exact_value_count(1)
            .set_default_value(10.0));

    parser().add_option_handler(
        &m_camera_elevation
            .add_name("--elevation")
            .add_name("-e")
            .set_description("set the normalized elevation of the camera")
            .set_syntax("scalar")
            .set_exact_value_count(1)
            .set_default_value(2.0));

    parser().add_option_handler(
        &m_motion_blur
            .add_name("--motion-blur")
            .add_name("-m")
            .set_description("set the amount of motion blur where 0 = none and 1 = full")
            .set_syntax("amount")
            .set_exact_value_count(1)
            .set_default_value(1.0f));
}

void CommandLineHandler::print_program_usage(
    const char*     executable_name,
    SuperLogger&    logger) const
{
    SaveLogFormatterConfig save_config(logger);
    logger.set_verbosity_level(LogMessage::Info);
    logger.set_format(LogMessage::Info, "{message}");

    LOG_INFO(logger, "usage: %s [options] master.appleseed output.appleseed", executable_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);
}

}   // namespace animatecamera
}   // namespace appleseed
