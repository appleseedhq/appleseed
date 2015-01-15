
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
#include "bindentitycontainers.h"
#include "dict2dict.h"
#include "metadata.h"

// appleseed.renderer headers.
#include "renderer/api/environment.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<EnvironmentEDF> create_environment_edf(const std::string env_type,
                                                            const std::string& name,
                                                            const bpy::dict& params)
    {
        EnvironmentEDFFactoryRegistrar factories;
        const IEnvironmentEDFFactory* factory = factories.lookup(env_type.c_str());

        if (factory)
            return factory->create(name.c_str(), bpy_dict_to_param_array(params));
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "EnvironmentEDF type not found");
            bpy::throw_error_already_set();
        }

        return auto_release_ptr<EnvironmentEDF>();
    }

    auto_release_ptr<EnvironmentShader> create_environment_shader(const std::string& env_shader_type,
                                                                  const std::string& name,
                                                                  const bpy::dict& params)
    {
        EnvironmentShaderFactoryRegistrar factories;
        const IEnvironmentShaderFactory* factory = factories.lookup(env_shader_type.c_str());

        if (factory)
            return factory->create(name.c_str(), bpy_dict_to_param_array(params));
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "EnvironmentShader type not found");
            bpy::throw_error_already_set();
        }

        return auto_release_ptr<EnvironmentShader>();
    }

    auto_release_ptr<Environment> create_environment(const std::string& name, const bpy::dict& params)
    {
        return EnvironmentFactory::create(name.c_str(), bpy_dict_to_param_array(params));
    }
}

void bind_environment()
{
    bpy::class_<EnvironmentEDF, auto_release_ptr<EnvironmentEDF>, bpy::bases<ConnectableEntity>, boost::noncopyable >("EnvironmentEDF", bpy::no_init)
        .def("get_input_metadata", &detail::get_entity_input_metadata<EnvironmentEDFFactoryRegistrar>).staticmethod("get_input_metadata")
        .def("__init__", bpy::make_constructor(create_environment_edf))
        .def("get_model", &EnvironmentEDF::get_model)
        ;

    bind_typed_entity_vector<EnvironmentEDF>("EnvironmentEDFContainer");

    bpy::class_<EnvironmentShader, auto_release_ptr<EnvironmentShader>, bpy::bases<ConnectableEntity>, boost::noncopyable >("EnvironmentShader", bpy::no_init)
        .def("get_input_metadata", &detail::get_entity_input_metadata<EnvironmentShaderFactoryRegistrar>).staticmethod("get_input_metadata")
        .def("__init__", bpy::make_constructor(create_environment_shader))
        .def("get_model", &EnvironmentShader::get_model)
        ;

    bind_typed_entity_vector<EnvironmentShader>("EnvironmentShaderContainer");

    bpy::class_<Environment, auto_release_ptr<Environment>, bpy::bases<Entity>, boost::noncopyable >("Environment", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_environment))
        .def("get_environment_edf", &Environment::get_environment_edf, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_environment_shader", &Environment::get_environment_shader, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_model", &Environment::get_model)
        ;
}
