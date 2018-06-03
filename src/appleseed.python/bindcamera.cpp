
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
#include "renderer/api/camera.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;
using namespace std;

// Work around a regression in Visual Studio 2015 Update 3.
#if defined(_MSC_VER) && _MSC_VER == 1900
namespace boost
{
    template <> Camera const volatile* get_pointer<Camera const volatile>(Camera const volatile* p) { return p; }
    template <> ICameraFactory const volatile* get_pointer<ICameraFactory const volatile>(ICameraFactory const volatile* p) { return p; }
    template <> CameraFactoryRegistrar const volatile* get_pointer<CameraFactoryRegistrar const volatile>(CameraFactoryRegistrar const volatile* p) { return p; }
}
#endif

namespace
{
    auto_release_ptr<Camera> create_camera(
        const string&       model,
        const string&       name,
        const bpy::dict&    params)
    {
        CameraFactoryRegistrar factories;
        const ICameraFactory* factory = factories.lookup(model.c_str());

        if (factory)
            return factory->create(name.c_str(), bpy_dict_to_param_array(params));
        else
        {
            PyErr_SetString(PyExc_RuntimeError, "Camera model not found");
            bpy::throw_error_already_set();
        }

        return auto_release_ptr<Camera>();
    }

    auto_release_ptr<Camera> factory_create_camera(
        const ICameraFactory*   factory,
        const char*             name,
        const bpy::dict&        params)
    {
        return factory->create(name, bpy_dict_to_param_array(params));
    }

    TransformSequence* camera_get_transform_sequence(Camera* camera)
    {
        return &(camera->transform_sequence());
    }

    void camera_set_transform_sequence(Camera* camera, const TransformSequence& seq)
    {
        camera->transform_sequence() = seq;
    }
}

void bind_camera()
{
    bpy::class_<Camera, auto_release_ptr<Camera>, bpy::bases<Entity>, boost::noncopyable>("Camera", bpy::no_init)
        .def("get_model_metadata", &detail::get_entity_model_metadata<CameraFactoryRegistrar>).staticmethod("get_model_metadata")
        .def("get_input_metadata", &detail::get_entity_input_metadata<CameraFactoryRegistrar>).staticmethod("get_input_metadata")
        .def("__init__", bpy::make_constructor(create_camera))
        .def("get_model", &Camera::get_model)
        .def("transform_sequence", camera_get_transform_sequence, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("set_transform_sequence", camera_set_transform_sequence)
        .def("get_shutter_open_begin_time", &Camera::get_shutter_open_begin_time)
        .def("get_shutter_open_end_time", &Camera::get_shutter_open_end_time)
        .def("get_shutter_close_begin_time", &Camera::get_shutter_close_begin_time)
        .def("get_shutter_close_end_time", &Camera::get_shutter_close_end_time)
        .def("get_shutter_time_interval", &Camera::get_shutter_time_interval)
        .def("get_shutter_middle_time", &Camera::get_shutter_middle_time);

    bind_typed_entity_vector<Camera>("CameraContainer");

    bpy::class_<ICameraFactory, boost::noncopyable>("ICameraFactory", bpy::no_init)
        .def("create", &factory_create_camera);

    bpy::class_<CameraFactoryRegistrar, boost::noncopyable>("CameraFactoryRegistrar", bpy::no_init)
        .def("lookup", &CameraFactoryRegistrar::lookup, bpy::return_value_policy<bpy::reference_existing_object>());
}
