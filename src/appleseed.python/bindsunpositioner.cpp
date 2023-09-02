
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Joao Marcos Costa, The appleseedhq Organization
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
#include "bindentitycontainers.h"
#include "dict2dict.h"
#include "metadata.h"

// appleseed.renderer header.
#include "renderer/utility/solarpositionalgorithm.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<SunPositioner> create_sun_positioner(const bpy::dict& params)
    {
        return SunPositionerFactory::create("Sun Positioner", bpy_dict_to_param_array(params));
    }

    void compute_sun_position(SunPositioner* sun_position)
    {
        sun_position->fetch_data();
        sun_position->compute_sun_position();
    }

    bpy::list get_input_metadata()
    {
        return dictionary_array_to_bpy_list(SunPositionerFactory::get_input_metadata());
    }
}

void bind_sun_positioner()
{
    bpy::class_<SunPositioner, auto_release_ptr<SunPositioner>, bpy::bases<Entity>, boost::noncopyable>("SunPositioner", bpy::no_init)
        .def("get_input_metadata", get_input_metadata).staticmethod("get_input_metadata")
        .def("__init__", bpy::make_constructor(create_sun_positioner))
        .def("compute_sun_position", compute_sun_position)

        .def("get_zenith", &SunPositioner::get_zenith)
        .def("get_azimuth", &SunPositioner::get_azimuth)
        .def("get_solar_noon", &SunPositioner::get_solar_noon)
        .def("get_sunrise", &SunPositioner::get_sunrise)
        .def("get_sunset", &SunPositioner::get_sunset);
}
