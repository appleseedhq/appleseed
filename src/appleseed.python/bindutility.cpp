
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.python headers.
#include "pyseed.h" // has to be first, to avoid redefinition warnings

// appleseed.renderer headers.
#include "renderer/api/log.h"
#include "renderer/modeling/project/eventcounters.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

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
        .def("has_errors", &EventCounters::has_errors)
        ;

    bpy::class_<ILogTarget, boost::noncopyable>("ILogTarget", bpy::no_init);

    bpy::class_<Logger, boost::noncopyable>("Logger", bpy::no_init)
        .def("set_enabled", &Logger::set_enabled)
        .def("add_target", &Logger::add_target)
        ;

    bpy::def("global_logger", global_logger, bpy::return_value_policy<bpy::reference_existing_object>());
}
