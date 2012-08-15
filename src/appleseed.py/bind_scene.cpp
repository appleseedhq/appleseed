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

#include "renderer/modeling/scene/scene.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{

auto_release_ptr<Scene> create_scene() { return SceneFactory::create();}

} // unnamed

void bind_scene()
{
    bpy::class_<Scene, auto_release_ptr<Scene>, boost::noncopyable>( "Scene", bpy::no_init)
        .def( "__init__", bpy::make_constructor( create_scene))

        .def( "get_uid", &Identifiable::get_uid)

        .def( "get_geometry_version_id", &Scene::get_geometry_version_id)
        .def( "bump_geometry_version_id", &Scene::bump_geometry_version_id)

        .def( "set_camera", &Scene::set_camera)
        .def( "get_camera", &Scene::get_camera, bpy::return_value_policy<bpy::reference_existing_object>())

        .def( "get_environment", &Scene::get_environment, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "set_environment", &Scene::set_environment)

        .def( "colors", &Scene::colors, bpy::return_value_policy<bpy::reference_existing_object>())

        .def( "textures", &Scene::textures, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "texture_instances", &Scene::texture_instances, bpy::return_value_policy<bpy::reference_existing_object>())

        .def( "environment_edfs", &Scene::environment_edfs, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "environment_shaders", &Scene::environment_shaders, bpy::return_value_policy<bpy::reference_existing_object>())

        .def( "assemblies", &Scene::assemblies, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "assembly_instances", &Scene::assembly_instances, bpy::return_value_policy<bpy::reference_existing_object>())

        .def( "compute_bbox", &Scene::compute_bbox)
        .def( "compute_radius", &Scene::compute_radius)
        ;
}
