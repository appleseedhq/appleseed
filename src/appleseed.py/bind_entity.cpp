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

#include "bind_auto_release_ptr.h"

#include <string>

#include "renderer/modeling/entity/entity.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

void bind_entity()
{
    bpy::class_<Entity, auto_release_ptr<Entity>, boost::noncopyable>( "Entity", bpy::no_init)
        .def( "get_uid", &Identifiable::get_uid)

        .def( "get_version_id", &Versionable::get_version_id)
        .def( "bump_version_id", &Versionable::bump_version_id)

        .def( "get_class_uid", &Entity::get_class_uid)

        .def( "get_name", &Entity::get_name)
        .def( "set_name", &Entity::set_name)

        .def( "get_render_layer_name", &Entity::get_render_layer_name)
        .def( "set_render_layer_name", &Entity::set_render_layer_name)

        .def( "set_render_layer_index", &Entity::set_render_layer_index)
        .def( "get_render_layer_index", &Entity::get_render_layer_index)
        ;
}
