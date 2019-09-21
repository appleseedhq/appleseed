
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2019 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/api/log.h"
#include "renderer/modeling/project/eventcounters.h"
#include "renderer/utility/oiiomaketexture.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"
#include "foundation/utility/api/apistring.h"

// Standard headers.
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    void make_texture(
        const std::string&   in_filename,
        const std::string&   out_filename,
        const std::string&   in_colorspace,
        const std::string&   out_depth)
    {
        APIString error_msg;
        const bool success =
            oiio_make_texture(
                in_filename.c_str(),
                out_filename.c_str(),
                in_colorspace.c_str(),
                out_depth.c_str(),
                error_msg);

        if (!success)
            PyErr_SetString(PyExc_RuntimeError, error_msg.c_str());
    }
}

void bind_utility()
{
    bpy::class_<EventCounters, boost::noncopyable>("EventCounters")
        .def("clear", &EventCounters::clear)
        .def("signal_warning", &EventCounters::signal_warning)
        .def("signal_warnings", &EventCounters::signal_warnings)
        .def("signal_error", &EventCounters::signal_error)
        .def("signal_errors", &EventCounters::signal_errors)
        .def("get_warning_count", &EventCounters::get_warning_count)
        .def("get_error_count", &EventCounters::get_error_count)
        .def("has_errors", &EventCounters::has_errors);

    bpy::class_<ILogTarget, boost::noncopyable>("ILogTarget", bpy::no_init);

    bpy::class_<Logger, boost::noncopyable>("Logger", bpy::no_init)
        .def("set_enabled", &Logger::set_enabled)
        .def("add_target", &Logger::add_target);

    bpy::def("global_logger", global_logger, bpy::return_value_policy<bpy::reference_existing_object>());

    bpy::def("oiio_make_texture", &make_texture);
}
