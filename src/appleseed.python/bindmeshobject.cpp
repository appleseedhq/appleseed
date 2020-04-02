
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
#include "renderer/api/object.h"

// appleseed.foundation headers.
#include "foundation/hash/murmurhash.h"
#include "foundation/platform/python.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<MeshObject> create_mesh_obj(
        const std::string&    name,
        const bpy::dict&      params)
    {
        return
            auto_release_ptr<MeshObject>(
                MeshObjectFactory().create(name.c_str(), bpy_dict_to_param_array(params)));
    }

    Triangle& get_triangle(MeshObject* object, const size_t index)
    {
        return object->get_triangle(index);
    }

    void set_triangle(MeshObject* object, const size_t index, const Triangle& triangle)
    {
        object->get_triangle(index) = triangle;
    }

    bpy::list read_mesh_objects(
        const bpy::list&      search_paths,
        const std::string&    base_object_name,
        const bpy::dict&      params)
    {
        SearchPaths paths;

        for (bpy::ssize_t i = 0, e = bpy::len(search_paths); i < e; ++i)
        {
            bpy::extract<const char*> ex(search_paths[i]);
            if (!ex.check())
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible type. Only strings.");
                bpy::throw_error_already_set();
            }

            paths.push_back_explicit_path(ex());
        }

        MeshObjectArray objs;
        bpy::list py_objects;

        if (MeshObjectReader::read(paths, base_object_name.c_str(), bpy_dict_to_param_array(params), objs))
        {
            for (size_t i = 0, e = objs.size(); i < e; ++i)
            {
                auto_release_ptr<MeshObject> object(objs[i]);
                py_objects.append(object);
            }
        }
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "appleseed.MeshObjectReader failed");
            bpy::throw_error_already_set();
            return py_objects;
        }

        return py_objects;
    }

    bool write_mesh_object(
        const MeshObject*     object,
        const std::string&    object_name,
        const std::string&    filename)
    {
        return MeshObjectWriter::write(*object, object_name.c_str(), filename.c_str());
    }

    auto_release_ptr<MeshObject> create_mesh_prim(
        const std::string&    name,
        const bpy::dict&      params)
    {
        return create_primitive_mesh(name.c_str(), bpy_dict_to_param_array(params));
    }

    void compute_mesh_signature(MurmurHash& hash, const MeshObject* mesh)
    {
        compute_signature(hash, *mesh);
    }
}

void bind_mesh_object()
{
    bpy::class_<Triangle>("Triangle")
        .def(bpy::init<size_t, size_t, size_t>())
        .def(bpy::init<size_t, size_t, size_t, size_t>())
        .def(bpy::init<size_t, size_t, size_t, size_t, size_t, size_t, size_t>())
        .def(bpy::init<size_t, size_t, size_t, size_t, size_t, size_t, size_t, size_t, size_t, size_t>())

        .def_readwrite("m_v0", &Triangle::m_v0)
        .def_readwrite("m_v1", &Triangle::m_v1)
        .def_readwrite("m_v2", &Triangle::m_v2)
        .def_readwrite("m_n0", &Triangle::m_n0)
        .def_readwrite("m_n1", &Triangle::m_n1)
        .def_readwrite("m_n2", &Triangle::m_n2)
        .def_readwrite("m_a0", &Triangle::m_a0)
        .def_readwrite("m_a1", &Triangle::m_a1)
        .def_readwrite("m_a2", &Triangle::m_a2)
        .def_readwrite("m_pa", &Triangle::m_pa)

        .def("has_vertex_attributes", &Triangle::has_vertex_attributes);

    bpy::class_<MeshObject, auto_release_ptr<MeshObject>, bpy::bases<Object>, boost::noncopyable>("MeshObject", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_mesh_obj))

        .def("reserve_vertices", &MeshObject::reserve_vertices)
        .def("push_vertex", &MeshObject::push_vertex)
        .def("get_vertex_count", &MeshObject::get_vertex_count)
        .def("get_vertex", &MeshObject::get_vertex, bpy::return_value_policy<bpy::reference_existing_object>())

        .def("reserve_vertex_normals", &MeshObject::reserve_vertex_normals)
        .def("push_vertex_normal", &MeshObject::push_vertex_normal)
        .def("get_vertex_normal_count", &MeshObject::get_vertex_normal_count)
        .def("get_vertex_normal", &MeshObject::get_vertex_normal, bpy::return_value_policy<bpy::reference_existing_object>())

        .def("reserve_vertex_tangents", &MeshObject::reserve_vertex_tangents)
        .def("push_vertex_tangent", &MeshObject::push_vertex_tangent)
        .def("get_vertex_tangent_count", &MeshObject::get_vertex_tangent_count)
        .def("get_vertex_tangent", &MeshObject::get_vertex_tangent)

        .def("reserve_tex_coords", &MeshObject::reserve_tex_coords)
        .def("push_tex_coords", &MeshObject::push_tex_coords)
        .def("get_tex_coords_count", &MeshObject::get_tex_coords_count)
        .def("get_tex_coords", &MeshObject::get_tex_coords)

        .def("reserve_triangles", &MeshObject::reserve_triangles)
        .def("push_triangle", &MeshObject::push_triangle)
        .def("get_triangle_count", &MeshObject::get_triangle_count)
        .def("get_triangle", get_triangle, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("set_triangle", set_triangle)

        .def("set_motion_segment_count", &MeshObject::set_motion_segment_count)
        .def("get_motion_segment_count", &MeshObject::get_motion_segment_count)

        .def("set_vertex_pose", &MeshObject::set_vertex_pose)
        .def("get_vertex_pose", &MeshObject::get_vertex_pose)
        .def("clear_vertex_poses", &MeshObject::clear_vertex_poses)

        .def("set_vertex_normal_pose", &MeshObject::set_vertex_normal_pose)
        .def("get_vertex_normal_pose", &MeshObject::get_vertex_normal_pose)
        .def("clear_vertex_normal_poses", &MeshObject::clear_vertex_normal_poses)

        .def("set_vertex_tangent_pose", &MeshObject::set_vertex_tangent_pose)
        .def("get_vertex_tangent_pose", &MeshObject::get_vertex_tangent_pose)
        .def("clear_vertex_tangent_poses", &MeshObject::clear_vertex_tangent_poses)

        .def("reserve_material_slots", &MeshObject::reserve_material_slots)
        .def("push_material_slot", &MeshObject::push_material_slot);

    boost::python::implicitly_convertible<auto_release_ptr<MeshObject>, auto_release_ptr<Object>>();

    bpy::class_<MeshObjectReader>("MeshObjectReader", bpy::no_init)
        .def("read", read_mesh_objects).staticmethod("read");

    bpy::class_<MeshObjectWriter>("MeshObjectWriter", bpy::no_init)
        .def("write", write_mesh_object).staticmethod("write");

    bpy::def("compute_smooth_vertex_normals", compute_smooth_vertex_normals);
    bpy::def("compute_smooth_vertex_tangents", compute_smooth_vertex_tangents);
    bpy::def("compute_signature", compute_mesh_signature);
    bpy::def("create_primitive_mesh", create_mesh_prim);
}
