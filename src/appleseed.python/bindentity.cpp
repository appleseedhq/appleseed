
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
#include "dict2dict.h"

// appleseed.renderer headers.
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/entity/entitymap.h"
#include "renderer/modeling/entity/entityvector.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    bpy::dict entity_get_parameters(const Entity* e)
    {
        return param_array_to_bpy_dict(e->get_parameters());
    }

    void entity_set_parameters(Entity* e, const bpy::dict& params)
    {
        e->get_parameters() = bpy_dict_to_param_array(params);
    }
}

void bind_entity()
{
    bpy::class_<Entity, auto_release_ptr<Entity>, boost::noncopyable>("Entity", bpy::no_init)
        .def("get_uid", &Identifiable::get_uid)

        .def("get_version_id", &Versionable::get_version_id)
        .def("bump_version_id", &Versionable::bump_version_id)

        .def("get_class_uid", &Entity::get_class_uid)

        .def("get_name", &Entity::get_name)
        .def("set_name", &Entity::set_name)

        .def("get_parameters", entity_get_parameters)
        .def("set_parameters", entity_set_parameters);

    bpy::class_<ConnectableEntity, auto_release_ptr<ConnectableEntity>, bpy::bases<Entity>, boost::noncopyable>("ConnectableEntity", bpy::no_init);

    bpy::class_<EntityVector, boost::noncopyable>("EntityVector")
        .def("clear", &EntityVector::clear)
        .def("__len__", &EntityVector::size);

    bpy::class_<EntityMap, boost::noncopyable>("EntityMap")
        .def("clear", &EntityMap::clear)
        .def("__len__", &EntityMap::size);
}
