
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "unalignedtransform.h"

// appleseed.renderer headers.
#include "renderer/api/object.h"
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/searchpaths.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;
using namespace std;

// Work around a regression in Visual Studio 2015 Update 3.
#if defined(_MSC_VER) && _MSC_VER == 1900
namespace boost
{
    template <> Object const volatile* get_pointer<Object const volatile>(Object const volatile* p) { return p; }
    template <> ObjectInstance const volatile* get_pointer<ObjectInstance const volatile>(ObjectInstance const volatile* p) { return p; }
}
#endif

namespace
{
    void bpy_list_to_string_array(const bpy::list& l, StringArray& strings)
    {
        strings.clear();

        for (bpy::ssize_t i = 0, e = bpy::len(l); i < e; ++i)
        {
            bpy::extract<const char*> ex(l[i]);
            if (!ex.check())
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible type. Only strings.");
                bpy::throw_error_already_set();
            }

            strings.push_back(ex());
        }
    }

    bpy::list obj_material_slots(const Object* obj)
    {
        bpy::list result;

        for (size_t i = 0, e = obj->get_material_slot_count(); i < e; ++i)
            result.append(obj->get_material_slot(i));

        return result;
    }

    auto_release_ptr<ObjectInstance> create_obj_instance_with_back_mat(
        const string&                   name,
        const bpy::dict&                params,
        const string&                   object_name,
        const UnalignedTransformd&      transform,
        const bpy::dict&                front_material_mappings,
        const bpy::dict&                back_material_mappings)
    {
        return
            ObjectInstanceFactory::create(
                name.c_str(),
                bpy_dict_to_param_array(params),
                object_name.c_str(),
                transform.as_foundation_transform(),
                bpy_dict_to_dictionary(front_material_mappings).strings(),
                bpy_dict_to_dictionary(back_material_mappings).strings());
    }

    auto_release_ptr<ObjectInstance> create_obj_instance(
        const string&                   name,
        const bpy::dict&                params,
        const string&                   object_name,
        const UnalignedTransformd&      transform,
        const bpy::dict&                front_material_mappings)
    {
        return
            create_obj_instance_with_back_mat(
                name,
                params,
                object_name,
                transform,
                front_material_mappings,
                bpy::dict());
    }

    UnalignedTransformd obj_inst_get_transform(const ObjectInstance* obj)
    {
        return UnalignedTransformd(obj->get_transform());
    }

    string obj_inst_get_obj_name(const ObjectInstance* obj)
    {
        return obj->get_object_name();
    }

    bpy::dict material_mappings_to_dict(const StringDictionary& mappings)
    {
        bpy::dict result;

        for (const_each<StringDictionary> it = mappings; it; ++it)
            result[it->key()] = it->value();

        return result;
    }

    bpy::dict obj_inst_get_front_material_mappings(const ObjectInstance* obj)
    {
        return material_mappings_to_dict(obj->get_front_material_mappings());
    }

    bpy::dict obj_inst_get_back_material_mappings(const ObjectInstance* obj)
    {
        return material_mappings_to_dict(obj->get_back_material_mappings());
    }
}

void bind_object()
{
    bpy::class_<Object, auto_release_ptr<Object>, bpy::bases<Entity>, boost::noncopyable>("Object", bpy::no_init)
        .def("get_model", &Object::get_model)
        .def("compute_local_bbox", &Object::compute_local_bbox)
        .def("material_slots", &obj_material_slots)
        .def("get_material_slot_count", &Object::get_material_slot_count)
        .def("get_material_slot", &Object::get_material_slot)
        ;

    bind_typed_entity_vector<Object>("ObjectContainer");

    bpy::class_<ObjectInstance, auto_release_ptr<ObjectInstance>, bpy::bases<Entity>, boost::noncopyable>("ObjectInstance", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_obj_instance))
        .def("__init__", bpy::make_constructor(create_obj_instance_with_back_mat))
        .def("get_object_name", &obj_inst_get_obj_name)
        .def("find_object", &ObjectInstance::find_object, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_transform", &obj_inst_get_transform)
        .def("bbox", &ObjectInstance::compute_parent_bbox)
        .def("get_front_material_mappings", &obj_inst_get_front_material_mappings)
        .def("get_back_material_mappings", &obj_inst_get_back_material_mappings)
        ;

    bind_typed_entity_vector<ObjectInstance>("ObjectInstanceContainer");
}
