
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
#include "projectfilereader.h"

// appleseed.renderer headers.
#include "renderer/modeling/project-builtin/cornellboxproject.h"
#include "renderer/modeling/project-builtin/defaultproject.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/eventcounters.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/projectfileupdater.h"
#include "renderer/modeling/project/projectformatrevision.h"
#include "renderer/modeling/project/xmlprojectfilereader.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

//
// ProjectFileReader class implementation.
//

namespace
{
    bool is_builtin_project(const std::string& project_filepath, std::string& project_name)
    {
        const std::string BuiltInPrefix = "builtin:";

        if (starts_with(project_filepath, BuiltInPrefix))
        {
            project_name = project_filepath.substr(BuiltInPrefix.size());
            return true;
        }

        return false;
    }
}

auto_release_ptr<Project> ProjectFileReader::read(
    const char*             project_filepath,
    const char*             schema_filepath,
    const int               options)
{
    assert(project_filepath);

    // Handle built-in projects.
    std::string project_name;
    if (is_builtin_project(project_filepath, project_name))
        return load_builtin(project_name.c_str());

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    EventCounters event_counters;

    auto_release_ptr<Project> project =
        XMLProjectFileReader::read(
                project_filepath,
                schema_filepath,
                options,
                event_counters);

    if (project.get())
        postprocess_project(project.ref(), event_counters, options);

    stopwatch.measure();

    print_loading_results(
        project_filepath,
        false,
        event_counters,
        stopwatch.get_seconds());

    return event_counters.has_errors() ? auto_release_ptr<Project>(nullptr) : project;
}

auto_release_ptr<Assembly> ProjectFileReader::read_archive(
    const char*             archive_filepath,
    const char*             schema_filepath,
    const SearchPaths&      search_paths,
    const int               options)
{
    assert(archive_filepath);

    EventCounters event_counters;

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    auto_release_ptr<Project> project =
        XMLProjectFileReader::read_archive(
            archive_filepath,
            schema_filepath,
            search_paths,
            options | OmitSearchPaths,
            event_counters);

    if (project.get())
    {
        if (!event_counters.has_errors() &&
            !(options & OmitProjectFileUpdate) &&
            project->get_format_revision() < ProjectFormatRevision)
            upgrade_project(*project, event_counters);
    }

    stopwatch.measure();

    print_loading_results(
        archive_filepath,
        false,
        event_counters,
        stopwatch.get_seconds());

    if (event_counters.has_errors())
        return auto_release_ptr<Assembly>(nullptr);

    if (project->get_scene())
    {
        if (Assembly* assembly = project->get_scene()->assemblies().get_by_name("assembly"))
        {
            return auto_release_ptr<Assembly>(
                project->get_scene()->assemblies().remove(assembly));
        }
    }

    return auto_release_ptr<Assembly>(nullptr);
}

auto_release_ptr<Project> ProjectFileReader::load_builtin(
    const char*             project_name)
{
    assert(project_name);

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    EventCounters event_counters;
    auto_release_ptr<Project> project(
        construct_builtin_project(project_name, event_counters));

    if (project.get())
        postprocess_project(project.ref(), event_counters);

    stopwatch.measure();

    print_loading_results(
        project_name,
        true,
        event_counters,
        stopwatch.get_seconds());

    return event_counters.has_errors() ? auto_release_ptr<Project>(nullptr) : project;
}

auto_release_ptr<Project> ProjectFileReader::construct_builtin_project(
    const char*             project_name,
    EventCounters&          event_counters)
{
    if (!strcmp(project_name, "cornell_box"))
    {
        return CornellBoxProjectFactory::create();
    }
    else if (!strcmp(project_name, "default"))
    {
        return DefaultProjectFactory::create();
    }
    else
    {
        RENDERER_LOG_ERROR("unknown built-in project %s.", project_name);
        event_counters.signal_error();
        return auto_release_ptr<Project>(nullptr);
    }
}

void ProjectFileReader::postprocess_project(
    Project&                project,
    EventCounters&          event_counters,
    const int               options)
{
    if (!event_counters.has_errors())
        validate_project(project, event_counters);

    if (!event_counters.has_errors())
        complete_project(project, event_counters);

    if (!event_counters.has_errors() &&
        !(options & OmitProjectFileUpdate) &&
        project.get_format_revision() < ProjectFormatRevision)
        upgrade_project(project, event_counters);
}

void ProjectFileReader::validate_project(
    const Project&          project,
    EventCounters&          event_counters)
{
    // Make sure the project contains a scene.
    if (project.get_scene())
    {
        // Make sure the scene contains at least one camera.
        if (project.get_scene()->cameras().empty())
        {
            RENDERER_LOG_ERROR("the scene does not define any camera.");
            event_counters.signal_error();
        }
    }
    else
    {
        RENDERER_LOG_ERROR("the project does not define a scene.");
        event_counters.signal_error();
    }

    // Make sure the project contains at least one output frame.
    if (project.get_frame() == nullptr)
    {
        RENDERER_LOG_ERROR("the project does not define any frame.");
        event_counters.signal_error();
    }

    // Make sure the project contains the required configurations.
    if (project.configurations().get_by_name("final") == nullptr)
    {
        RENDERER_LOG_ERROR("the project must define a \"final\" configuration.");
        event_counters.signal_error();
    }
    if (project.configurations().get_by_name("interactive") == nullptr)
    {
        RENDERER_LOG_ERROR("the project must define an \"interactive\" configuration.");
        event_counters.signal_error();
    }
}

void ProjectFileReader::complete_project(
    Project&                project,
    EventCounters&          event_counters)
{
    // Add a default environment if the project doesn't define any.
    if (project.get_scene()->get_environment() == nullptr)
    {
        auto_release_ptr<Environment> environment(
            EnvironmentFactory::create("environment", ParamArray()));
        project.get_scene()->set_environment(environment);
    }
}

void ProjectFileReader::upgrade_project(
    Project&                project,
    EventCounters&          event_counters)
{
    ProjectFileUpdater updater;
    updater.update(project, event_counters);
}

void ProjectFileReader::print_loading_results(
    const char*             project_name,
    const bool              builtin_project,
    const EventCounters&    event_counters,
    const double            loading_time)
{
    const size_t warning_count = event_counters.get_warning_count();
    const size_t error_count = event_counters.get_error_count();

    const LogMessage::Category log_category =
        error_count > 0 ? LogMessage::Error :
        warning_count > 0 ? LogMessage::Warning :
        LogMessage::Info;

    if (error_count > 0)
    {
        RENDERER_LOG(
            log_category,
            "failed to load %s %s (" FMT_SIZE_T " %s, " FMT_SIZE_T " %s).",
            builtin_project ? "built-in project" : "project file",
            project_name,
            error_count,
            plural(error_count, "error").c_str(),
            warning_count,
            plural(warning_count, "warning").c_str());
    }
    else
    {
        RENDERER_LOG(
            log_category,
            "successfully loaded %s %s in %s (" FMT_SIZE_T " %s, " FMT_SIZE_T " %s).",
            builtin_project ? "built-in project" : "project file",
            project_name,
            pretty_time(loading_time).c_str(),
            error_count,
            plural(error_count, "error").c_str(),
            warning_count,
            plural(warning_count, "warning").c_str());
    }
}

}   // namespace renderer
