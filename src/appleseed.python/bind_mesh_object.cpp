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
#include "foundation/utility/searchpaths.h"

#include "bind_typed_entity_containers.h"
#include "dict2dict.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace detail
{

auto_release_ptr<MeshObject> create_mesh_obj(const std::string& name, const bpy::dict& params)
{
    return MeshObjectFactory::create(name.c_str(), bpy_dict_to_param_array(params));
}

bpy::list read_mesh_objects(const bpy::list& search_paths, const std::string& base_object_name,
                                const bpy::dict& params)
{
    SearchPaths paths;

	for (unsigned i = 0, e = bpy::len(search_paths); i < e; ++i)
	{
		bpy::extract<const char*> ex(search_paths[i]);
		if(!ex.check())
		{
            PyErr_SetString(PyExc_TypeError, "Incompatible type. Only strings." );
            bpy::throw_error_already_set();
		}

		paths.push_back(ex());
	}

    MeshObjectArray objs;
    bpy::list py_objects;

    if (MeshObjectReader::read(paths, base_object_name.c_str(), bpy_dict_to_param_array(params), objs))
    {
        for (int i = 0, e = objs.size(); i < e; ++i)
            py_objects.append(auto_release_ptr<MeshObject>(objs[i]));
    }
    else
    {
        PyErr_SetString(PyExc_RuntimeError, "MeshObjectReader failed" );
        bpy::throw_error_already_set();
        return py_objects;
    }

    return py_objects;
}

bool write_mesh_object(MeshObject* obj, const std::string& obj_name, const std::string& filename)
{
    return MeshObjectWriter::write(*obj, obj_name.c_str(), filename.c_str());
}

} // detail

void bind_mesh_object()
{
    bpy::class_<Triangle>("Triangle")
        .def(bpy::init<const size_t, const size_t, const size_t>())
        .def(bpy::init<const size_t, const size_t, const size_t, const size_t>())

        .def(bpy::init<const size_t, const size_t, const size_t, const size_t,
                        const size_t, const size_t, const size_t>())

        .def(bpy::init<const size_t, const size_t, const size_t, const size_t,
                        const size_t, const size_t, const size_t, const size_t,
                        const size_t, const size_t>())

        .def("has_vertex_attributes", &Triangle::has_vertex_attributes)

        .def_readwrite("v0", &Triangle::m_v0)
        .def_readwrite("v1", &Triangle::m_v1)
        .def_readwrite("v2", &Triangle::m_v2)
        .def_readwrite("n0", &Triangle::m_n0)
        .def_readwrite("n1", &Triangle::m_n1)
        .def_readwrite("n2", &Triangle::m_n2)
        .def_readwrite("a0", &Triangle::m_a0)
        .def_readwrite("a1", &Triangle::m_a1)
        .def_readwrite("a2", &Triangle::m_a2)
        .def_readwrite("pa", &Triangle::m_pa)
        ;

    bpy::class_<MeshObject, auto_release_ptr<MeshObject>, bpy::bases<Object>, boost::noncopyable>("MeshObject", bpy::no_init)
        .def("__init__", bpy::make_constructor(detail::create_mesh_obj))

        .def("reserve_vertices", &MeshObject::reserve_vertices)
        .def("push_vertex", &MeshObject::push_vertex)
        .def("get_vertex_count", &MeshObject::get_vertex_count)
        .def("get_vertex", &MeshObject::get_vertex)

        .def("reserve_vertex_normals", &MeshObject::reserve_vertex_normals)
        .def("push_vertex_normal", &MeshObject::push_vertex_normal)
        .def("get_vertex_normal_count", &MeshObject::get_vertex_normal_count)
        .def("get_vertex_normal", &MeshObject::get_vertex_normal)

        .def("push_tex_coords", &MeshObject::push_tex_coords)
        .def("get_tex_coords_count", &MeshObject::get_tex_coords_count)
        .def("get_tex_coords", &MeshObject::get_tex_coords)

        .def("reserve_triangles", &MeshObject::reserve_triangles)
        .def("push_triangle", &MeshObject::push_triangle)
        .def("get_triangle_count", &MeshObject::get_triangle_count)
        .def("get_triangle", &MeshObject::get_triangle)

        .def("set_motion_segment_count", &MeshObject::set_motion_segment_count)
        .def("get_motion_segment_count", &MeshObject::get_motion_segment_count)

        .def("set_vertex_pose", &MeshObject::set_vertex_pose)
        .def("get_vertex_pose", &MeshObject::get_vertex_pose)
        ;

    boost::python::implicitly_convertible<auto_release_ptr<MeshObject>, auto_release_ptr<Object> >();

    bpy::class_<MeshObjectReader>("MeshObjectReader", bpy::no_init)
        .def("read", detail::read_mesh_objects).staticmethod("read")
        ;

    bpy::class_<MeshObjectWriter>("MeshObjectWriter", bpy::no_init)
        .def("write", detail::write_mesh_object).staticmethod("write")
        ;
}
