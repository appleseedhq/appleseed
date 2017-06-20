
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
#include "pythoninterpreter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/platform/python.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

namespace bpy = boost::python;
using namespace appleseed::studio;
using namespace renderer;
using namespace foundation;

void new_project(bool fail_on_unsaved_project = false)
{
    MainWindow* mainWindow = PythonInterpreter::instance().get_mainwindow();

    if (fail_on_unsaved_project && !mainWindow->can_close_project())
        throw Exception("Opened project was not saved");
    else
        mainWindow->new_project();
}
BOOST_PYTHON_FUNCTION_OVERLOADS(new_project_overloads, new_project, 0, 1)

void open_project(const char* project_path)
{
    PythonInterpreter::instance().get_mainwindow()->open_project(project_path);
}

Project* opened_project()
{
    return PythonInterpreter::instance().get_mainwindow()->opened_project();
}

void save_project(const char* project_path = 0)
{
    if (project_path == 0)
        PythonInterpreter::instance().get_mainwindow()->save_project(opened_project()->get_path());
    else
        PythonInterpreter::instance().get_mainwindow()->save_project(project_path);
}
BOOST_PYTHON_FUNCTION_OVERLOADS(save_project_overloads, save_project, 0, 1)

BOOST_PYTHON_MODULE(_appleseedstudio)
{
    bpy::def("new_project", new_project, new_project_overloads(
        bpy::args("fail_on_unsaved_project")));
    bpy::def("open_project", open_project, bpy::args("project_path"));
    bpy::def("save_project", save_project, save_project_overloads(
        bpy::args("project_path")));
}
