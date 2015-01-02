
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

// Project headers.
#include "commandlinehandler.h"

// appleseed.shared headers.
#include "application/application.h"
#include "application/superlogger.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <string>

using namespace appleseed::shared;
using namespace appleseed::updateprojectfile;
using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;


//
// Entry point of updateprojectfile.
//

int main(int argc, const char* argv[])
{
    SuperLogger logger;
    Application::check_installation(logger);

    CommandLineHandler cl;
    cl.parse(argc, argv, logger);

    // Initialize the renderer's logger.
    global_logger().add_target(&logger.get_log_target());

    // Retrieve the input file path.
    const string& input_filepath = cl.m_filename.value();

    // Construct the schema file path.
    const filesystem::path schema_filepath =
          filesystem::path(Application::get_root_path())
        / "schemas"
        / "project.xsd";

    // Read the input project from disk.
    // Note: it is crucial that we read mesh files as well, so that we can collect
    // material slots declared by objects. Material slots are required by the project
    // file updater, for instance when migrating projects from rev. 7 to rev. 8.
    ProjectFileReader reader;
    auto_release_ptr<Project> project(
        reader.read(
            input_filepath.c_str(),
            schema_filepath.string().c_str(),
            ProjectFileReader::OmitProjectFileUpdate));

    // Bail out if the project couldn't be loaded.
    if (project.get() == 0)
        return 1;

    // Update the project file to the desired revision.
    ProjectFileUpdater updater;
    if (cl.m_to_revision.is_set())
        updater.update(project.ref(), cl.m_to_revision.value());
    else updater.update(project.ref());

    // Write the project back to disk.
    const bool success =
        ProjectFileWriter::write(
            project.ref(),
            project->get_path(),
            ProjectFileWriter::OmitWritingGeometryFiles | ProjectFileWriter::OmitBringingAssets);

    return success ? 0 : 1;
}
