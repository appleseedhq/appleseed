
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
#include "renderer/api/edf.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<EDF> create_edf(
        const std::string&    model,
        const std::string&    name,
        const bpy::dict&      params)
    {
        EDFFactoryRegistrar factories;
        const IEDFFactory* factory = factories.lookup(model.c_str());

        if (factory)
            return factory->create(name.c_str(), bpy_dict_to_param_array(params));
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "EDF model not found");
            bpy::throw_error_already_set();
        }

        return auto_release_ptr<EDF>();
    }

    auto_release_ptr<EDF> factory_create_edf(
        const IEDFFactory*  factory,
        const char*         name,
        const bpy::dict&    params)
    {
        return factory->create(name, bpy_dict_to_param_array(params));
    }
}

void bind_edf()
{
    bpy::class_<EDF, auto_release_ptr<EDF>, bpy::bases<ConnectableEntity>, boost::noncopyable>("EDF", bpy::no_init)
        .def("get_model_metadata", &detail::get_entity_model_metadata<EDFFactoryRegistrar>).staticmethod("get_model_metadata")
        .def("get_input_metadata", &detail::get_entity_input_metadata<EDFFactoryRegistrar>).staticmethod("get_input_metadata")
        .def("__init__", bpy::make_constructor(create_edf))
        .def("get_model", &EDF::get_model);

    bind_typed_entity_vector<EDF>("EDFContainer");

    bpy::class_<IEDFFactory, boost::noncopyable>("IEDFFactory", bpy::no_init)
        .def("create", &factory_create_edf);

    bpy::class_<EDFFactoryRegistrar, boost::noncopyable>("EDFFactoryRegistrar", bpy::no_init)
        .def("lookup", &EDFFactoryRegistrar::lookup, bpy::return_value_policy<bpy::reference_existing_object>());
}
