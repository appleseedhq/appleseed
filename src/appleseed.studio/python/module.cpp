
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Gleb Mishchenko, The appleseedhq Organization
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

// appleseed.studio headers.
#include "mainwindow/mainwindow.h"
#include "mainwindow/minimizebutton.h"
#include "python/pythoninterpreter.h"

// appleseed.qtcommon headers.
#include "project/projectmanager.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/platform/python.h"
#include "foundation/utility/casts.h"

// appleseed.common headers.
#include "application/application.h"

// Qt headers.
#include <QDockWidget>

// Standard headers.
#include <cstdint>

namespace bpy = boost::python;
using namespace appleseed::qtcommon;
using namespace appleseed::studio;
using namespace foundation;
using namespace renderer;

MainWindow* main_window()
{
    return PythonInterpreter::instance().get_main_window();
}

ProjectManager* project_manager()
{
    return main_window()->get_project_manager();
}

void new_project()
{
    main_window()->new_project();
}

bool open_project(const char* project_path)
{
    return main_window()->open_project(project_path);
}

bool save_project()
{
    const Project* project = project_manager()->get_project();
    return
        project != nullptr && project->has_path()
            ? main_window()->save_project(project->get_path())
            : false;
}

bool save_project_as(const char* project_path)
{
    return main_window()->save_project(project_path);
}

bool pack_project_as(const char* project_path)
{
    return main_window()->pack_project(project_path);
}

void close_project()
{
    main_window()->close_project();
}

Project* current_project()
{
    return project_manager()->get_project();
}

bool is_project_dirty()
{
    return project_manager()->is_project_dirty();
}

void set_project_dirty()
{
    project_manager()->set_project_dirty_flag();
}

bpy::long_ main_window_as_pylong()
{
    const auto ptr = binary_cast<std::uintptr_t>(main_window());
    return bpy::long_(ptr);
}

bpy::long_ create_dock_widget(const char* dock_name)
{
    const QDockWidget* dock_widget = main_window()->create_dock_widget(dock_name);
    const auto ptr = binary_cast<std::uintptr_t>(dock_widget);
    return bpy::long_(ptr);
}

BOOST_PYTHON_MODULE(_appleseedstudio)
{
    bpy::def("new_project", new_project);
    bpy::def("open_project", open_project, bpy::args("project_path"));
    bpy::def("save_project", save_project);
    bpy::def("save_project_as", save_project_as, bpy::args("project_path"));
    bpy::def("pack_project_as", pack_project_as, bpy::args("project_path"));
    bpy::def("close_project", close_project);

    bpy::def("current_project", current_project, bpy::return_value_policy<bpy::reference_existing_object>());
    bpy::def("is_project_dirty", is_project_dirty);
    bpy::def("set_project_dirty", set_project_dirty);

    bpy::def("main_window", main_window_as_pylong);
    bpy::def("create_dock_widget", create_dock_widget, bpy::args("dock_name"));

    boost::python::def("get_root_path", appleseed::common::Application::get_root_path);
}
