
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/api/aov.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/platform/python.h"

// Standard headers.
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<AOV> create_aov(
        const std::string&    model,
        const bpy::dict&      params)
    {
        AOVFactoryRegistrar factories;
        const IAOVFactory* factory = factories.lookup(model.c_str());

        if (factory)
            return factory->create(bpy_dict_to_param_array(params));
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "AOV model not found");
            bpy::throw_error_already_set();
        }

        return auto_release_ptr<AOV>();
    }

    auto_release_ptr<AOV> factory_create_aov(
        const IAOVFactory*  factory,
        const bpy::dict&    params)
    {
        return factory->create(bpy_dict_to_param_array(params));
    }

    bpy::list get_channel_names(const AOV* aov)
    {
        bpy::list channels;

        const char** names = aov->get_channel_names();

        for (size_t i = 0, e = aov->get_channel_count(); i < e; ++i)
            channels.append(names[i]);

        return channels;
    }

    Image* get_cryptomatte_image(const AOV* aov)
    {
        return static_cast<const CryptomatteAOV*>(aov)->get_cryptomatte_image();
    }
}

void bind_aov()
{
    bpy::class_<AOV, auto_release_ptr<AOV>, bpy::bases<Entity>, boost::noncopyable>("AOV", bpy::no_init)
        .def("get_model_metadata", &detail::get_entity_model_metadata<AOVFactoryRegistrar>).staticmethod("get_model_metadata")
        .def("get_input_metadata", &detail::get_entity_input_metadata<AOVFactoryRegistrar>).staticmethod("get_input_metadata")
        .def("__init__", bpy::make_constructor(create_aov))
        .def("get_model", &AOV::get_model)
        .def("get_channel_count", &AOV::get_channel_count)
        .def("get_channel_names", &get_channel_names)
        .def("has_color_data", &AOV::has_color_data)
        .def("get_image", &AOV::get_image, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_cryptomatte_image", &get_cryptomatte_image, bpy::return_value_policy<bpy::reference_existing_object>());

    bind_typed_entity_vector<AOV>("AOVContainer");

    bpy::class_<IAOVFactory, boost::noncopyable>("IAOVFactory", bpy::no_init)
        .def("create", &factory_create_aov);

    bpy::class_<AOVFactoryRegistrar, boost::noncopyable>("AOVFactoryRegistrar", bpy::no_init)
        .def("lookup", &AOVFactoryRegistrar::lookup, bpy::return_value_policy<bpy::reference_existing_object>());
}
