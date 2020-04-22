
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
#include "projectmanager.h"

// appleseed.common headers.
#include "application/application.h"

// appleseed.renderer headers.
#include "renderer/api/log.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/searchpaths.h"

// Qt headers.
#include <QFutureWatcher>
#include <QtConcurrent>

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <exception>
#include <new>

using namespace appleseed::common;
using namespace foundation;
using namespace renderer;
namespace bf = boost::filesystem;

namespace appleseed {
namespace qtcommon {

//
// ProjectManager class implementation.
//

ProjectManager::ProjectManager()
  : m_is_loading(false)
  , m_dirty_flag(false)
{

    connect(
        &m_async_io_future_watcher, &QFutureWatcher<bool>::finished,
        this, &ProjectManager::slot_load_project_async_complete);
}

void ProjectManager::create_project()
{
    APPLESEED_UNUSED const bool result = load_builtin_project("default");
    assert(result);
}

bool ProjectManager::load_project(const std::string& filepath)
{
    return do_load_project(filepath);
}

void ProjectManager::load_project_async(const std::string& filepath)
{
    m_is_loading = true;

    m_async_io_filepath = QString::fromStdString(filepath);

    m_async_io_future_watcher.setFuture(
        QtConcurrent::run(this, &ProjectManager::do_load_project, filepath));
}

bool ProjectManager::load_builtin_project(const std::string& name)
{
    m_project = ProjectFileReader::load_builtin(name.c_str());
    m_dirty_flag = false;

    return true;
}

bool ProjectManager::save_project_as(const std::string& filepath)
{
    const bool successful = do_save_project(filepath, ProjectFileWriter::Defaults);

    if (successful)
    {
        m_project->set_path(filepath.c_str());
        m_project->search_paths().set_root_path(bf::absolute(filepath).parent_path().string());
        emit signal_project_path_changed(QString::fromStdString(filepath));
        m_dirty_flag = false;
    }

    return successful;
}

bool ProjectManager::pack_project_as(const std::string& filepath)
{
    return do_save_project(filepath, ProjectFileWriter::Defaults);
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
    return m_project.get() != nullptr;
}

bool ProjectManager::is_project_loading() const
{
    return m_is_loading;
}

std::string ProjectManager::get_project_display_name() const
{
    assert(m_project.get());

    if (m_project->has_path())
    {
        const bf::path filepath(m_project->get_path());
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

void ProjectManager::slot_load_project_async_complete()
{
    m_is_loading = false;

    // Can't use qobject_cast<>: https://bugreports.qt.io/browse/QTBUG-10727.
    const bool successful =
        static_cast<QFutureWatcher<bool>*>(sender())->future().result();

    emit signal_load_project_async_complete(m_async_io_filepath, successful);
}

std::string ProjectManager::get_project_schema_filepath()
{
    const bf::path schema_filepath =
          bf::path(Application::get_root_path())
        / "schemas"
        / "project.xsd";

    return schema_filepath.string();
}

bool ProjectManager::do_load_project(const std::string& filepath)
{
    try
    {
        const std::string schema_filepath = get_project_schema_filepath();

        auto_release_ptr<Project> loaded_project(
            ProjectFileReader::read(filepath.c_str(), schema_filepath.c_str()));

        if (loaded_project.get() == nullptr)
            return false;

        m_project = loaded_project;
        m_dirty_flag = false;

        return true;
    }
    catch (const std::bad_alloc&)
    {
        RENDERER_LOG_ERROR("failed to load project file %s (ran out of memory).", filepath.c_str());
        return false;
    }
#ifdef NDEBUG
    catch (const std::exception& e)
    {
        RENDERER_LOG_ERROR("failed to load project file %s (%s).", filepath.c_str(), e.what());
        return false;
    }
    catch (...)
    {
        RENDERER_LOG_ERROR("failed to load project file %s (unknown exception).", filepath.c_str());
        return false;
    }
#endif
}

bool ProjectManager::do_save_project(
    const std::string&                  filepath,
    const ProjectFileWriter::Options    options)
{
    assert(m_project.get());

    return ProjectFileWriter::write(m_project.ref(), filepath.c_str(), options);
}

}   // namespace qtcommon
}   // namespace appleseed
