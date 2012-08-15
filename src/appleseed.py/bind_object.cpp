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

#include "bind_typed_entity_containers.hpp"
#include "dict2dict.hpp"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{

void bpy_list_to_string_array( const bpy::list& l, StringArray& strings)
{
    strings.clear();

	for( unsigned i = 0, e = bpy::len( l); i < e; ++i)
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

auto_release_ptr<MeshObject> create_mesh_obj( const std::string& name, const bpy::dict& params)
{
    return MeshObjectFactory::create( name.c_str(), bpy_dict_to_param_array( params));
}

auto_release_ptr<ObjectInstance> create_obj_instance_with_back_mat( const std::string& name, const bpy::dict& params,
                                                    Object& obj, const Transformd& transform,
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

bpy::list read_mesh_objects( const bpy::list& search_paths, const std::string& base_object_name,
                                const bpy::dict& params)
{
    SearchPaths paths;

	for( unsigned i = 0, e = bpy::len( search_paths); i < e; ++i)
	{
		bpy::extract<const char*> ex( search_paths[i]);
		if( !ex.check())
		{
            PyErr_SetString( PyExc_TypeError, "Incompatible type. Only strings." );
            bpy::throw_error_already_set();
		}

		paths.push_back( ex());
	}

    MeshObjectArray objs;
    bpy::list py_objects;

    if( MeshObjectReader::read( paths, base_object_name.c_str(), bpy_dict_to_param_array( params), objs))
    {
        for( int i = 0, e = objs.size(); i < e; ++i)
            py_objects.append( auto_release_ptr<MeshObject>( objs[i]));
    }
    else
    {
        PyErr_SetString( PyExc_RuntimeError, "MeshObjectReader failed" );
        bpy::throw_error_already_set();
        return py_objects;
    }

    return py_objects;
}

bool write_mesh_object( MeshObject *obj, const std::string& obj_name, const std::string& filename)
{
    return MeshObjectWriter::write( *obj, obj_name.c_str(), filename.c_str());
}

} // unnamed

void bind_object()
{
    bpy::class_<Object, auto_release_ptr<Object>, bpy::bases<Entity>, boost::noncopyable>( "Object", bpy::no_init)
        .def( "compute_local_bbox", &Object::compute_local_bbox)
        ;

    bind_typed_entity_vector<Object>( "ObjectContainer");

    bpy::class_<MeshObject, auto_release_ptr<MeshObject>, bpy::bases<Object>, boost::noncopyable>( "MeshObject", bpy::no_init)
        .def( "__init__", bpy::make_constructor( create_mesh_obj))
        ;

    boost::python::implicitly_convertible<auto_release_ptr<MeshObject>, auto_release_ptr<Object> >();

    bpy::class_<MeshObjectReader>( "MeshObjectReader", bpy::no_init)
        .def( "read", read_mesh_objects).staticmethod( "read")
        ;

    bpy::class_<MeshObjectWriter>( "MeshObjectWriter", bpy::no_init)
        .def( "write", write_mesh_object).staticmethod( "write")
        ;

    bpy::class_<ObjectInstance, auto_release_ptr<ObjectInstance>, bpy::bases<Entity>, boost::noncopyable>( "ObjectInstance", bpy::no_init)
        .def( "__init__", bpy::make_constructor( create_obj_instance))
        .def( "__init__", bpy::make_constructor( create_obj_instance_with_back_mat))

        .def( "get_object", &ObjectInstance::get_object, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "get_transform", &ObjectInstance::get_transform, bpy::return_value_policy<bpy::copy_const_reference>())
        .def( "get_parent_bbox", &ObjectInstance::get_parent_bbox, bpy::return_value_policy<bpy::copy_const_reference>())
        ;

    bind_typed_entity_vector<ObjectInstance>( "ObjectInstanceContainer");
}
