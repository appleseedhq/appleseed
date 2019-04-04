
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

// appleseed.foundation headers.
#include "foundation/platform/python.h"

// OIIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imagebufalgo.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <sstream>
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace OIIO;
using namespace renderer;
using namespace std;

void oiio_make_texture(
    const string&   in_filename,
    const string&   out_filename,
    const string&   in_colorspace,
    const string&   out_depth)
{
    unordered_map<string, TypeDesc> out_depth_map;
    out_depth_map["sint8"] = TypeDesc::INT8;
    out_depth_map["uint8"] = TypeDesc::UINT8;
    out_depth_map["uint16"] = TypeDesc::UINT16;
    out_depth_map["sint16"] = TypeDesc::INT16;
    out_depth_map["half"] = TypeDesc::HALF;
    out_depth_map["float"] = TypeDesc::FLOAT;

    ImageSpec spec;

    if (out_depth != "default")
        spec.format = out_depth_map[out_depth];

    spec.attribute("maketx:updatemode", 1);
    spec.attribute("maketx:constant_color_detect", 1);
    spec.attribute("maketx:monochrome detect", 1);
    spec.attribute("maketx:opaque detect", 1);
    spec.attribute("maketx:unpremult", 1);
    spec.attribute("maketx:incolorspace", in_colorspace);
    spec.attribute("maketx:outcolorspace", "linear");
    spec.attribute("maketx:fixnan", "box3");

    ImageBufAlgo::MakeTextureMode mode = ImageBufAlgo::MakeTxTexture;

    stringstream s;

    if (!ImageBufAlgo::make_texture(mode, in_filename, out_filename, spec, &s))
        PyErr_SetString(PyExc_RuntimeError, s.str().c_str());
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

    bpy::def("oiio_make_texture", &oiio_make_texture);
}
