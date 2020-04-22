
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

// projecttool headers.
#include "commandlinehandler.h"

// appleseed.common headers.
#include "application/application.h"
#include "application/superlogger.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/containers/dictionary.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <string>

using namespace appleseed::projecttool;
using namespace appleseed::common;
using namespace foundation;
using namespace renderer;
namespace bf = boost::filesystem;

namespace
{
    CommandLineHandler g_cl;

    auto_release_ptr<Project> load_project(const std::string& project_filepath)
    {
        // Construct the schema file path.
        const bf::path schema_filepath =
              bf::path(Application::get_root_path())
            / "schemas"
            / "project.xsd";

        // Read the input project from disk.
        // Note: it is crucial that we read mesh files as well, so that we can collect
        // material slots declared by objects. Material slots are required by the project
        // file updater, for instance when migrating projects from rev. 7 to rev. 8.
        return
            ProjectFileReader::read(
                project_filepath.c_str(),
                schema_filepath.string().c_str(),
                ProjectFileReader::OmitProjectFileUpdate);
    }
}


//
// Update a project to a given revision.
//

bool update_project()
{
    // Retrieve the input project path.
    const std::string& input_filepath = g_cl.m_positional_args.values()[1];

    // Read the input project from disk.
    auto_release_ptr<Project> project(load_project(input_filepath));
    if (project.get() == nullptr)
        return false;

    // Update the project file to the desired revision.
    ProjectFileUpdater updater;
    const bool success =
        g_cl.m_to_revision.is_set()
            ? updater.update(project.ref(), g_cl.m_to_revision.value())
            : updater.update(project.ref());
    if (!success)
        return false;

    // Write the project back to disk.
    return
        ProjectFileWriter::write(
            project.ref(),
            input_filepath.c_str(),
            ProjectFileWriter::OmitWritingGeometryFiles | ProjectFileWriter::OmitHandlingAssetFiles);
}


//
// Update a project to the latest revision and remove unused entities.
//

bool remove_unused_entities()
{
    // Retrieve the input project path.
    const std::string& input_filepath = g_cl.m_positional_args.values()[1];

    // Read the input project from disk.
    auto_release_ptr<Project> project(load_project(input_filepath));
    if (project.get() == nullptr)
        return false;

    // Update the project file to the latest revision.
    ProjectFileUpdater updater;
    const bool success = updater.update(project.ref());
    if (!success)
        return false;

    // Remove unused entities.
    ProjectTracker tracker(project.ref());
    tracker.remove_unused_entities();

    // Write the project back to disk.
    return
        ProjectFileWriter::write(
            project.ref(),
            input_filepath.c_str(),
            ProjectFileWriter::OmitWritingGeometryFiles | ProjectFileWriter::OmitHandlingAssetFiles);
}


//
// Pack a project to an *.appleseedz file.
//

bool pack_project()
{
    // Retrieve the input project path.
    const std::string& input_filepath = g_cl.m_positional_args.values()[1];

    // Read the input project from disk.
    auto_release_ptr<Project> project(load_project(input_filepath));
    if (project.get() == nullptr)
        return false;

    // Build the path of the output project.
    const std::string packed_file_path =
        bf::path(input_filepath).replace_extension(".appleseedz").string();

    // Write the project to disk.
    return ProjectFileWriter::write(project.ref(), packed_file_path.c_str());
}


//
// Unpack an *.appleseedz file.
//

bool unpack_project()
{
    // Retrieve the input project path.
    const std::string& input_filepath = g_cl.m_positional_args.values()[1];

    // Read the input project from disk.
    auto_release_ptr<Project> project(load_project(input_filepath));
    if (project.get() == nullptr)
        return false;

    // Build the path of the output project.
    const std::string unpacked_file_path =
        bf::path(input_filepath).replace_extension(".appleseed").string();

    // Write the project to disk.
    const bool success =
        ProjectFileWriter::write(
            project.ref(),
            unpacked_file_path.c_str());

    // Delete the packed project file.
    if (success)
        bf::remove(input_filepath);

    return success;
}


//
// Print dependencies between entities.
//

bool print_entity_dependencies(SuperLogger& logger)
{
    // Retrieve the input project path.
    const std::string& input_filepath = g_cl.m_positional_args.values()[1];

    // Read the input project from disk.
    auto_release_ptr<Project> project(load_project(input_filepath));
    if (project.get() == nullptr)
        return false;

    // Print dependencies between entities.
    ProjectTracker tracker(project.ref());
    tracker.print_dependencies(logger);

    return true;
}


//
// Entry point of projecttool.
//

int main(int argc, char* argv[])
{
    // Construct the logger that will be used throughout the program.
    SuperLogger logger;

    // Make sure this build can run on this host.
    Application::check_compatibility_with_host(logger);

    // Make sure appleseed is correctly installed.
    Application::check_installation(logger);

    // Parse the command line.
    g_cl.parse(argc, argv, logger);

    // Load an apply settings from the settings file.
    Dictionary settings;
    Application::load_settings("appleseed.tools.xml", settings, logger);
    logger.configure_from_settings(settings);

    // Apply command line arguments.
    g_cl.apply(logger);

    // Configure the renderer's global logger.
    // Must be done after settings have been loaded and the command line
    // has been parsed, because these two operations may replace the log
    // target of the global logger.
    global_logger().initialize_from(logger);

    // Retrieve the command.
    const std::string& command = g_cl.m_positional_args.values()[0];

    // Execute the command.
    bool success = false;
    if (command == "update")
        success = update_project();
    else if (command == "clean")
        success = remove_unused_entities();
    else if (command == "pack")
        success = pack_project();
    else if (command == "unpack")
        success = unpack_project();
    else if (command == "deps")
        success = print_entity_dependencies(logger);
    else LOG_ERROR(logger, "unknown command: %s", command.c_str());

    return success ? 0 : 1;
}
