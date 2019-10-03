
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

// appleseed.renderer headers.
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<Scene> create_scene()
    {
        return SceneFactory::create();
    }
}

void bind_scene()
{
    bpy::class_<Scene, auto_release_ptr<Scene>, bpy::bases<Entity, BaseGroup>, boost::noncopyable>("Scene", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_scene))
        .def("get_uid", &Identifiable::get_uid)
        .def("cameras", &Scene::cameras, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("get_environment", &Scene::get_environment, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("set_environment", &Scene::set_environment)
        .def("environment_edfs", &Scene::environment_edfs, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("environment_shaders", &Scene::environment_shaders, bpy::return_value_policy<bpy::reference_existing_object>())
        .def("compute_bbox", &Scene::compute_bbox);
}
