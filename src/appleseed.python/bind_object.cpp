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

#include "renderer/api/object.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "foundation/utility/searchpaths.h"

#include "bind_typed_entity_containers.h"
#include "dict2dict.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace detail
{

void bpy_list_to_string_array( const bpy::list& l, StringArray& strings)
{
    strings.clear();

	for (unsigned i = 0, e = bpy::len( l); i < e; ++i)
	{
		bpy::extract<const char*> ex( l[i]);
		if( !ex.check())
		{
            PyErr_SetString( PyExc_TypeError, "Incompatible type. Only strings." );
            bpy::throw_error_already_set();
		}

		strings.push_back( ex());
	}
}

auto_release_ptr<ObjectInstance> create_obj_instance_with_back_mat( const std::string& name,
                                                                    const bpy::dict& params,
                                                                    Object& obj,
                                                                    const Transformd& transform,
                                                                    const bpy::list& front_materials,
                                                                    const bpy::list& back_materials)
{
    StringArray front_mats;
    bpy_list_to_string_array( front_materials, front_mats);

    StringArray back_mats;
    bpy_list_to_string_array( back_materials, back_mats);

    return ObjectInstanceFactory::create( name.c_str(), bpy_dict_to_param_array( params),
                                            obj, transform, front_mats, back_mats);
}

auto_release_ptr<ObjectInstance> create_obj_instance( const std::string& name, const bpy::dict& params,
                                                       Object& obj, const Transformd& transform,
                                                       const bpy::list& front_materials)
{
    return create_obj_instance_with_back_mat( name, params, obj, transform, front_materials, bpy::list());
}

} // detail

void bind_object()
{
    bpy::class_<Object, auto_release_ptr<Object>, bpy::bases<Entity>, boost::noncopyable>( "Object", bpy::no_init)
        .def( "compute_local_bbox", &Object::compute_local_bbox)
        ;

    bind_typed_entity_vector<Object>( "ObjectContainer");

    bpy::class_<ObjectInstance, auto_release_ptr<ObjectInstance>, bpy::bases<Entity>, boost::noncopyable>( "ObjectInstance", bpy::no_init)
        .def( "__init__", bpy::make_constructor( detail::create_obj_instance))
        .def( "__init__", bpy::make_constructor( detail::create_obj_instance_with_back_mat))

        .def( "get_object", &ObjectInstance::get_object, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "get_transform", &ObjectInstance::get_transform, bpy::return_value_policy<bpy::copy_const_reference>())
        .def( "get_parent_bbox", &ObjectInstance::get_parent_bbox, bpy::return_value_policy<bpy::copy_const_reference>())
        ;

    bind_typed_entity_vector<ObjectInstance>( "ObjectInstanceContainer");
}
