
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Gleb Mishchenko, The appleseedhq Organization
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
#include "python/pythoninterpreter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/platform/python.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

namespace bpy = boost::python;
using namespace appleseed::studio;
using namespace renderer;
using namespace foundation;

MainWindow* main_window()
{
    return PythonInterpreter::instance().get_main_window();
}

ProjectManager* project_manager()
{
    return main_window()->get_project_manager();
}

Project* new_project()
{
    main_window()->new_project();
    return project_manager()->get_project();
}

Project* open_project(const char* project_path)
{
    main_window()->open_project(project_path);
    return project_manager()->get_project();
}

void save_project(const char* project_path = nullptr)
{
    if (project_path == nullptr)
        main_window()->save_project(project_manager()->get_project()->get_path());
    else
        main_window()->save_project(project_path);
}
BOOST_PYTHON_FUNCTION_OVERLOADS(save_project_overloads, save_project, 0, 1)

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

BOOST_PYTHON_MODULE(_appleseedstudio)
{
    bpy::def("new_project", new_project,
             bpy::return_value_policy<bpy::reference_existing_object>());
    bpy::def("open_project", open_project, bpy::args("project_path"),
             bpy::return_value_policy<bpy::reference_existing_object>());
    bpy::def("save_project", save_project,
             save_project_overloads(bpy::args("project_path")));
    bpy::def("close_project", close_project);

    bpy::def("current_project", current_project,
             bpy::return_value_policy<bpy::reference_existing_object>());
    bpy::def("is_project_dirty", is_project_dirty);
}
