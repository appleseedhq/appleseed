
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

    m_animation_path.add_name("--animation-path");
    m_animation_path.add_name("-a");
    m_animation_path.set_description("load an animation path file");
    m_animation_path.set_syntax("filename.txt");
    m_animation_path.set_exact_value_count(1);
    parser().add_option_handler(&m_animation_path);

    m_3dsmax_mode.add_name("--3dsmax");
    m_3dsmax_mode.set_description("assume the animation path file uses Autodesk 3ds Max's coordinate system");
    parser().add_option_handler(&m_3dsmax_mode);

    m_output_format.add_name("--output-format");
    m_output_format.add_name("-o");
    m_output_format.set_description("set the format of the output frames");
    m_output_format.set_syntax("format");
    m_output_format.set_exact_value_count(1);
    m_output_format.set_default_value("exr");
    parser().add_option_handler(&m_output_format);

    m_frame_count.add_name("--frame-count");
    m_frame_count.add_name("-f");
    m_frame_count.set_description("set the number of frames in the animation");
    m_frame_count.set_syntax("count");
    m_frame_count.set_exact_value_count(1);
    m_frame_count.set_default_value(20);
    parser().add_option_handler(&m_frame_count);

    m_part_count.add_name("--part-count");
    m_part_count.add_name("-p");
    m_part_count.set_description("split the render script in that many parts");
    m_part_count.set_syntax("count");
    m_part_count.set_exact_value_count(1);
    m_part_count.set_default_value(1);
    parser().add_option_handler(&m_part_count);

    m_camera_target.add_name("--target");
    m_camera_target.add_name("-t");
    m_camera_target.set_description("set the target of the camera relatively to the center of the scene");
    m_camera_target.set_syntax("x y z");
    m_camera_target.set_exact_value_count(3);
    m_camera_target.set_default_values(make_vector(0.0, 0.0, 0.0));
    parser().add_option_handler(&m_camera_target);

    m_camera_distance.add_name("--distance");
    m_camera_distance.add_name("-d");
    m_camera_distance.set_description("set the normalized distance from the camera to the scene");
    m_camera_distance.set_syntax("scalar");
    m_camera_distance.set_exact_value_count(1);
    m_camera_distance.set_default_value(10.0);
    parser().add_option_handler(&m_camera_distance);

    m_camera_elevation.add_name("--elevation");
    m_camera_elevation.add_name("-e");
    m_camera_elevation.set_description("set the normalized elevation of the camera");
    m_camera_elevation.set_syntax("scalar");
    m_camera_elevation.set_exact_value_count(1);
    m_camera_elevation.set_default_value(2.0);
    parser().add_option_handler(&m_camera_elevation);
}

void CommandLineHandler::print_program_usage(
    const char*     program_name,
    SuperLogger&    logger) const
{
    SaveLogFormatterConfig save_config(logger);
    logger.set_format(LogMessage::Info, "{message}");

    LOG_INFO(logger, "usage: %s [options] master.appleseed output.appleseed", program_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);
}

}   // namespace animatecamera
}   // namespace appleseed
