
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
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyfactoryregistrar.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/iassemblyfactory.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"

// Standard headers.
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<Assembly> create_assembly(const std::string& name)
    {
        return AssemblyFactory().create(name.c_str(), ParamArray());
    }

    auto_release_ptr<Assembly> create_assembly_with_params(
        const std::string&    name,
        const bpy::dict&      params)
    {
        return AssemblyFactory().create(name.c_str(), bpy_dict_to_param_array(params));
    }

    auto_release_ptr<Assembly> create_assembly_with_model_and_params(
        const std::string&    model,
        const std::string&    name,
        const bpy::dict&      params)
    {
        AssemblyFactoryRegistrar factories;
        const IAssemblyFactory* factory = factories.lookup(model.c_str());

        if (factory)
            return factory->create(name.c_str(), bpy_dict_to_param_array(params));
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "Assembly model not found");
            bpy::throw_error_already_set();
        }

        return auto_release_ptr<Assembly>();
    }

    auto_release_ptr<Assembly> factory_create_assembly(
        const IAssemblyFactory* factory,
        const char*             name,
        const bpy::dict&        params)
    {
        return factory->create(name, bpy_dict_to_param_array(params));
    }

    auto_release_ptr<AssemblyInstance> create_assembly_instance(
        const std::string&    name,
        const bpy::dict&      params,
        const std::string&    assembly_name)
    {
        return
            AssemblyInstanceFactory::create(
                name.c_str(),
                bpy_dict_to_param_array(params),
                assembly_name.c_str());
    }

    TransformSequence& get_transform_sequence(AssemblyInstance* instance)
    {
        return instance->transform_sequence();
    }

    void set_transform_sequence(AssemblyInstance* instance, const TransformSequence& seq)
    {
        instance->transform_sequence() = seq;
    }

    std::string get_assembly_name(AssemblyInstance* instance)
    {
        return instance->get_assembly_name();
    }
}

void bind_assembly()
{
    bpy::class_<BaseGroup, boost::noncopyable>("BaseGroup", bpy::no_init)
        .def("colors", &BaseGroup::colors, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("textures", &BaseGroup::textures, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("texture_instances", &BaseGroup::texture_instances, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("shader_groups", &BaseGroup::shader_groups, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("assemblies", &BaseGroup::assemblies, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("assembly_instances", &BaseGroup::assembly_instances, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("clear", &BaseGroup::clear);

    bpy::class_<Assembly, auto_release_ptr<Assembly>, bpy::bases<Entity, BaseGroup>, boost::noncopyable>("Assembly", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_assembly))
        .def("__init__", bpy::make_constructor(create_assembly_with_params))
        .def("__init__", bpy::make_constructor(create_assembly_with_model_and_params))
        .def("bsdfs", &Assembly::bsdfs, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("bssrdfs", &Assembly::bssrdfs, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("edfs", &Assembly::edfs, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("surface_shaders", &Assembly::surface_shaders, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("materials", &Assembly::materials, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("lights", &Assembly::lights, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("objects", &Assembly::objects, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("object_instances", &Assembly::object_instances, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("volumes", &Assembly::volumes, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("clear", &Assembly::clear)
        .def("compute_local_bbox", &Assembly::compute_local_bbox)
        .def("compute_non_hierarchical_local_bbox", &Assembly::compute_non_hierarchical_local_bbox);

    bind_typed_entity_map<Assembly>("AssemblyContainer");

    bpy::class_<AssemblyInstance, auto_release_ptr<AssemblyInstance>, bpy::bases<Entity>, boost::noncopyable>("AssemblyInstance", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_assembly_instance))
        .def("transform_sequence", get_transform_sequence, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("set_transform_sequence", set_transform_sequence)
        .def("get_vis_flags", &AssemblyInstance::get_vis_flags)
        .def("compute_parent_bbox", &AssemblyInstance::compute_parent_bbox)
        .def("get_assembly_name", &get_assembly_name)
        .def("find_assembly", &AssemblyInstance::find_assembly, bpy::return_value_policy<bpy::reference_existing_object>());

    bind_typed_entity_map<AssemblyInstance>("AssemblyInstanceContainer");

    bpy::class_<IAssemblyFactory, boost::noncopyable>("IAssemblyFactory", bpy::no_init)
        .def("create", &factory_create_assembly);

    bpy::class_<AssemblyFactoryRegistrar, boost::noncopyable>("AssemblyFactoryRegistrar", bpy::no_init)
        .def("lookup", &AssemblyFactoryRegistrar::lookup, bpy::return_value_policy<bpy::reference_existing_object>());
}
