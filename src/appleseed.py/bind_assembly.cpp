//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012 Esteban Tovagliari.
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

// Has to be first, to avoid redifinition warnings.
#include "bind_auto_release_ptr.h"

#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"

#include "bind_typed_entity_containers.hpp"
#include "dict2dict.hpp"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{

auto_release_ptr<Assembly> create_assembly( const std::string& name)
{
    return AssemblyFactory::create( name.c_str(), ParamArray());
}

auto_release_ptr<Assembly> create_assembly_with_params( const std::string& name, const bpy::dict& params)
{
    return AssemblyFactory::create( name.c_str(), bpy_dict_to_param_array( params));
}

auto_release_ptr<AssemblyInstance> create_assembly_instance( const std::string& name,
                                                                const bpy::dict& params,
                                                                const Assembly *assembly)
{
    return AssemblyInstanceFactory::create( name.c_str(), bpy_dict_to_param_array( params), *assembly);
}

TransformSequence& assembly_instance_get_transform_sequence( AssemblyInstance *instance)
{
    return instance->transform_sequence();
}

} // unnamed

void bind_assembly()
{
    bpy::class_<Assembly, auto_release_ptr<Assembly>, bpy::bases<Entity>, boost::noncopyable>( "Assembly", bpy::no_init)
        .def( "__init__", bpy::make_constructor( create_assembly))
        .def( "__init__", bpy::make_constructor( create_assembly_with_params))
        .def( "colors", &Assembly::colors, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "textures", &Assembly::textures, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "texture_instances", &Assembly::texture_instances, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "bsdfs", &Assembly::bsdfs, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "edfs", &Assembly::edfs, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "surface_shaders", &Assembly::surface_shaders, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "materials", &Assembly::materials, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "lights", &Assembly::lights, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "objects", &Assembly::objects, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "object_instances", &Assembly::object_instances, bpy::return_value_policy<bpy::reference_existing_object>())
        ;

    bind_typed_entity_map<Assembly>( "AssemblyContainer");

    bpy::class_<AssemblyInstance, auto_release_ptr<AssemblyInstance>, bpy::bases<Entity>, boost::noncopyable>( "AssemblyInstance", bpy::no_init)
        .def( "__init__", bpy::make_constructor( create_assembly_instance))
        .def( "get_assembly", &AssemblyInstance::get_assembly, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "get_assembly_uid", &AssemblyInstance::get_assembly_uid)
        .def( "transform_sequence", assembly_instance_get_transform_sequence, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "compute_parent_bbox", &AssemblyInstance::compute_parent_bbox)
        ;

    bind_typed_entity_map<AssemblyInstance>( "AssemblyInstanceContainer");
}
