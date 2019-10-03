
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
#include "metadata.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"

// Standard headers.
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<BSDF> create_bsdf(
        const std::string&    model,
        const std::string&    name,
        const bpy::dict&      params)
    {
        BSDFFactoryRegistrar factories;
        const IBSDFFactory* factory = factories.lookup(model.c_str());

        if (factory)
            return factory->create(name.c_str(), bpy_dict_to_param_array(params));
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "BSDF model not found");
            bpy::throw_error_already_set();
        }

        return auto_release_ptr<BSDF>();
    }

    auto_release_ptr<BSDF> factory_create_bsdf(
        const IBSDFFactory*     factory,
        const char*             name,
        const bpy::dict&        params)
    {
        return factory->create(name, bpy_dict_to_param_array(params));
    }
}

void bind_bsdf()
{
    bpy::class_<BSDF, auto_release_ptr<BSDF>, bpy::bases<ConnectableEntity>, boost::noncopyable>("BSDF", bpy::no_init)
        .def("get_model_metadata", &detail::get_entity_model_metadata<BSDFFactoryRegistrar>).staticmethod("get_model_metadata")
        .def("get_input_metadata", &detail::get_entity_input_metadata<BSDFFactoryRegistrar>).staticmethod("get_input_metadata")
        .def("__init__", bpy::make_constructor(create_bsdf))
        .def("get_model", &BSDF::get_model);

    bind_typed_entity_vector<BSDF>("BSDFContainer");

    bpy::class_<IBSDFFactory, boost::noncopyable>("IBSDFFactory", bpy::no_init)
        .def("create", &factory_create_bsdf);

    bpy::class_<BSDFFactoryRegistrar, boost::noncopyable>("BSDFFactoryRegistrar", bpy::no_init)
        .def("lookup", &BSDFFactoryRegistrar::lookup, bpy::return_value_policy<bpy::reference_existing_object>());
}
