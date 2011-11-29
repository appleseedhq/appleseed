
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

// appleseed.shared headers.
#include "application/application.h"
#include "application/superlogger.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"

// boost headers.
#include "boost/filesystem/path.hpp"

using namespace appleseed::normalizeprojectfile;
using namespace appleseed::shared;
using namespace boost;
using namespace foundation;
using namespace renderer;


//
// Entry point of normalizeprojectfile.
//

int main(int argc, const char* argv[])
{
    SuperLogger logger;

    Application::check_installation(logger);

    CommandLineHandler cl;
    cl.parse(argc, argv, logger);

    global_logger().add_target(&logger.get_log_target());

    // Construct the schema filename.
    const filesystem::path schema_path =
          filesystem::path(Application::get_root_path())
        / "schemas/project.xsd";

    // Load the input project from disk.
    ProjectFileReader reader;
    auto_release_ptr<Project> project(
        reader.read(
            cl.m_filename.values()[0].c_str(),
            schema_path.file_string().c_str()));

    // Bail out if the project couldn't be loaded.
    if (project.get() == 0)
        return 1;

    // Write the project back to disk.
    return ProjectFileWriter::write(project.ref()) ? 0 : 1;
}
