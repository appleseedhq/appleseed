
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/modeling/color/colorentity.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/platform/python.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    ColorValueArray color_value_array_from_bpy_list(const bpy::list& vals)
    {
        ColorValueArray result;

        ssize_t size = bpy::len(vals);
        result.reserve(size);

        for (ssize_t i = 0; i < size; ++i)
        {
            bpy::extract<float> ex(vals[i]);
            if (ex.check())
                result.push_back(ex());
            else
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible type, only float is allowed.");
                bpy::throw_error_already_set();
            }
        }

        return result;
    }

    bpy::list bpy_list_from_color_value_array(const ColorValueArray& vals)
    {
        bpy::list result;

        for (size_t i = 0, e = vals.size(); i < e; ++i)
            result.append(vals[i]);

        return result;
    }

    auto_release_ptr<ColorEntity> create_color_entity(
        const std::string&  name,
        const bpy::dict&    params)
    {
        return ColorEntityFactory::create(name.c_str(), bpy_dict_to_param_array(params));
    }

    auto_release_ptr<ColorEntity> create_color_entity_vals(
        const std::string&  name,
        const bpy::dict&    params,
        const bpy::list&    values)
    {
        return
            ColorEntityFactory::create(
                name.c_str(),
                bpy_dict_to_param_array(params),
                color_value_array_from_bpy_list(values));
    }

    auto_release_ptr<ColorEntity> create_color_entity_vals_alpha(
        const std::string&  name,
        const bpy::dict&    params,
        const bpy::list&    values,
        const bpy::list&    alpha)
    {
        return
            ColorEntityFactory::create(
                name.c_str(),
                bpy_dict_to_param_array(params),
                color_value_array_from_bpy_list(values),
                color_value_array_from_bpy_list(alpha));
    }

    bpy::list color_entity_get_vals(const ColorEntity* color)
    {
        return bpy_list_from_color_value_array(color->get_values());
    }

    bpy::list color_entity_get_alpha(const ColorEntity* color)
    {
        return bpy_list_from_color_value_array(color->get_alpha());
    }
}

void bind_color()
{
    bpy::enum_<ColorSpace>("ColorSpace")
        .value("LinearRGB", ColorSpaceLinearRGB)
        .value("SRGB", ColorSpaceSRGB)
        .value("CIEXYZ", ColorSpaceCIEXYZ)
        .value("Spectral", ColorSpaceSpectral);

    bpy::class_<ColorEntity, auto_release_ptr<ColorEntity>, bpy::bases<Entity>, boost::noncopyable>("ColorEntity", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_color_entity))
        .def("__init__", bpy::make_constructor(create_color_entity_vals))
        .def("__init__", bpy::make_constructor(create_color_entity_vals_alpha))
        .def("get_values", color_entity_get_vals)
        .def("get_alpha", color_entity_get_alpha)
        .def("get_color_space", &ColorEntity::get_color_space)
        .def("get_wavelength_range", &ColorEntity::get_wavelength_range, bpy::return_value_policy<bpy::copy_const_reference>())
        .def("get_multiplier", &ColorEntity::get_multiplier);

    bind_typed_entity_vector<ColorEntity>("ColorContainer");
}
