
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

// Project headers.
#include "commandlinehandler.h"
#include "defaults.h"

// appleseed.shared headers.
#include "application/application.h"
#include "application/superlogger.h"

// appleseed.renderer headers.
#include "renderer/api/camera.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/log.h"
#include "foundation/utility/string.h"

// boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <iomanip>
#include <sstream>
#include <string>

using namespace appleseed::animatecamera;
using namespace appleseed::shared;
using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace
{
    CommandLineHandler  g_cl;
    Logger              g_logger;

    string make_numbered_filename(
        const string    filename,
        const size_t    frame,
        const size_t    digits = 4)
    {
        const filesystem::path path(filename);

        stringstream sstr;
        sstr << path.stem();
        sstr << '.';
        sstr << setw(digits) << setfill('0') << frame;
        sstr << path.extension();

        return sstr.str();
    }

    void generate_project_files(
        const string    base_output_filename,
        const int       frame_count)
    {
        // Construct the schema filename.
        const filesystem::path schema_path =
              filesystem::path(Application::get_root_path())
            / "schemas/project.xsd";

        // Load the input project from disk.
        ProjectFileReader reader;
        auto_release_ptr<Project> project(
            reader.read(
                g_cl.m_filenames.values()[0].c_str(),
                schema_path.file_string().c_str()));

        // Bail out if the project couldn't be loaded.
        if (project.get() == 0)
            exit(1);

        const double normalized_distance =
            g_cl.m_camera_distance.found()
                ? g_cl.m_camera_distance.values()[0]
                : DefaultNormalizedCameraDistance;

        const double normalized_elevation =
            g_cl.m_camera_elevation.found()
                ? g_cl.m_camera_elevation.values()[0]
                : DefaultNormalizedCameraElevation;

        // Retrieve the scene's bounding box.
        const AABB3d scene_bbox(project->get_scene()->compute_bbox());
        const Vector3d extent = scene_bbox.extent();
        const double max_radius = 0.5 * max(extent.x, extent.z);
        const double max_height = 0.5 * extent.y;

        for (int i = 0; i < frame_count; ++i)
        {
            // Compute the position of the camera at this frame.
            const double angle = static_cast<double>(i) / frame_count * TwoPi;
            const double distance = max_radius * normalized_distance;
            const double elevation = max_height * normalized_elevation;
            const Vector3d position(
                distance * cos(angle),
                elevation,
                distance * sin(angle));

            // Compute and set the transform of the camera at this frame.
            const Vector3d center = scene_bbox.center();
            const Vector3d Up(0.0, 1.0, 0.0);
            Camera* camera = project->get_scene()->get_camera();
            camera->set_transform(Transformd(Matrix4d::lookat(position, center, Up)));

            // Write the project file for this frame.
            const string new_path = make_numbered_filename(base_output_filename + ".appleseed", i + 1);
            project->set_path(new_path.c_str());
            ProjectFileWriter::write(project.ref(), false);
        }
    }

    void generate_windows_render_script(
        const string    base_output_filename,
        const int       frame_count)
    {
        LOG_INFO(g_logger, "generating batch file...");

        const char* BatchFileName = "render.bat";

        FILE* batch_file = fopen(BatchFileName, "wt");

        if (batch_file == 0)
            LOG_FATAL(g_logger, "could not write to %s.", BatchFileName);

        fprintf(
            batch_file,
            "%s",
            "@echo off\n"
            "\n"
            "set bin=\"%1\"\n"
            "\n"
            "if %bin% == \"\" (\n"
            "    echo Usage: %0 path-to-appleseed-binary\n"
            "    goto :end\n"
            ")\n"
            "\n"
            "if not exist %bin% (\n"
            "    echo Could not find %bin%, exiting.\n"
            "    goto :end\n"
            ")\n"
            "\n"
            "if not exist frames (\n"
            "    mkdir frames\n"
            ")\n"
            "\n");

        for (int i = 0; i < frame_count; ++i)
        {
            const string project_filename = make_numbered_filename(base_output_filename + ".appleseed", i + 1);
            const string image_filename = make_numbered_filename(base_output_filename + ".png", i + 1);

            const size_t LineLength = 80 + 1;   // +1 to account for the escape character
            const string header = "--- " + project_filename + " -^> " + image_filename + " ";
            const string header_suffix(header.size() < LineLength ? LineLength - header.size() : 0, '-');

            fprintf(
                batch_file,
                "echo %s%s\n",
                header.c_str(),
                header_suffix.c_str());

            fprintf(
                batch_file,
                "%%bin%% %s -o frames\\%s\n",
                project_filename.c_str(),
                image_filename.c_str());
        }

        fprintf(
            batch_file,
            "%s",
            "\n"
            "echo.\n"
            "\n"
            ":end\n");

        fclose(batch_file);
    }
}


//
// Entry point of animatecamera.
//

int main(int argc, const char* argv[])
{
    SuperLogger logger;

    Application::check_installation(logger);

    g_cl.parse(argc, argv, logger);

    const string base_output_filename =
        filesystem::path(g_cl.m_filenames.values()[1]).stem();

    const int frame_count =
        g_cl.m_frame_count.found()
            ? g_cl.m_frame_count.values()[0]
            : DefaultFrameCount;

    if (frame_count < 1)
        LOG_FATAL(g_logger, "the frame count must be greater than or equal to 1.");

    global_logger().add_target(&logger.get_log_target());

    generate_project_files(base_output_filename, frame_count);

#ifdef _WIN32
    generate_windows_render_script(base_output_filename, frame_count);
#endif

    return 0;
}
