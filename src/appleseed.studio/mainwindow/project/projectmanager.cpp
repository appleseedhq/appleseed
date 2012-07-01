
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "projectmanager.h"

// appleseed.shared headers.
#include "application/application.h"

// boost headers.
#include "boost/filesystem/path.hpp"

using namespace appleseed::shared;
using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

//
// ProjectManager class implementation.
//

ProjectManager::ProjectManager()
  : m_dirty_flag(false)
{
}

void ProjectManager::create_project()
{
    const bool result = load_builtin_project("default");
    assert(result);
}

bool ProjectManager::load_project(const string& filepath)
{
    const string schema_filepath = get_project_schema_filepath();

    ProjectFileReader reader;
    auto_release_ptr<Project> loaded_project(
        reader.read(filepath.c_str(), schema_filepath.c_str()));

    return try_set_project(loaded_project);
}

bool ProjectManager::load_builtin_project(const string& name)
{
    ProjectFileReader reader;
    auto_release_ptr<Project> loaded_project = reader.load_builtin(name.c_str());

    return try_set_project(loaded_project);
}

bool ProjectManager::reload_project()
{
    assert(m_project.get());
    assert(m_project->has_path());

    const string project_path(m_project->get_path());

    return load_project(project_path);
}

bool ProjectManager::save_project()
{
    assert(m_project.get());

    return save_project_as(m_project->get_path());
}

bool ProjectManager::save_project_as(const string& filepath)
{
    assert(m_project.get());

    if (!ProjectFileWriter::write(m_project.ref(), filepath.c_str()))
        return false;

    m_project->set_path(filepath.c_str());

    m_dirty_flag = false;

    return true;
}

void ProjectManager::close_project()
{
    m_project.reset();
    m_dirty_flag = false;
}

Project* ProjectManager::get_project() const
{
    return m_project.get();
}

bool ProjectManager::is_project_open() const
{
    return m_project.get() != 0;
}

string ProjectManager::get_project_display_name() const
{
    assert(m_project.get());

    if (m_project->has_path())
    {
        const filesystem::path filepath(m_project->get_path());
        return filepath.filename().string();
    }
    else
    {
        return "Untitled";
    }
}

void ProjectManager::clear_project_dirty_flag()
{
    m_dirty_flag = false;
}

void ProjectManager::set_project_dirty_flag()
{
    m_dirty_flag = true;
}

bool ProjectManager::is_project_dirty() const
{
    return m_dirty_flag;
}

string ProjectManager::get_project_schema_filepath()
{
    const filesystem::path schema_path =
          filesystem::path(Application::get_root_path())
        / "schemas"
        / "project.xsd";

    return schema_path.string();
}

bool ProjectManager::try_set_project(auto_release_ptr<Project> project)
{
    if (project.get() == 0)
        return false;

    m_project = project;
    m_dirty_flag = false;

    return true;
}

}   // namespace studio
}   // namespace appleseed
