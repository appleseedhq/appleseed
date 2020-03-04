
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

#pragma once

// appleseed.python headers.
#include "dict2dict.h"

// appleseed.renderer headers.
#include "renderer/modeling/entity/entitymap.h"
#include "renderer/modeling/entity/entityvector.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/python.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace detail
{
    template <typename T>
    T* typed_entity_vector_get_item(renderer::TypedEntityVector<T>& vec, const int relative_index)
    {
        const size_t index =
            static_cast<size_t>(
                relative_index >= 0 ? relative_index : vec.size() + relative_index);

        if (index >= vec.size())
        {
            PyErr_SetString(PyExc_IndexError, "Invalid index in appleseed.EntityVector");
            boost::python::throw_error_already_set();
        }

        return vec.get_by_index(index);
    }

    template <typename T>
    boost::python::object typed_entity_vector_remove(renderer::TypedEntityVector<T>& vec, T* entity)
    {
        foundation::auto_release_ptr<T> e = vec.remove(entity);
        return boost::python::object(e);
    }

    template <typename T>
    boost::python::object typed_entity_vector_get_iter(renderer::TypedEntityVector<T>& vec)
    {
        boost::python::list items;

        typedef typename renderer::TypedEntityVector<T>::iterator iterator;

        for (iterator it(vec.begin()), e(vec.end()); it != e; ++it)
            items.append(boost::python::ptr(&(*it)));

        return items.attr("__iter__")();
    }

    template <typename T>
    size_t typed_entity_vector_insert(renderer::TypedEntityVector<T>* vector, foundation::auto_release_ptr<T> entity)
    {
        if (vector->get_by_name(entity->get_name()) != nullptr)
            throw foundation::Exception(foundation::format("Entity {0} already exists", entity->get_name()).c_str());
        else
            return vector->insert(entity);
    }

    template <typename T>
    T* typed_entity_map_get_item(renderer::TypedEntityMap<T>& map, const std::string& key)
    {
        return map.get_by_name(key.c_str());
    }

    template <typename T>
    boost::python::object typed_entity_map_remove(renderer::TypedEntityMap<T>* map, T* entity)
    {
        foundation::auto_release_ptr<T> e = map->remove(entity);
        return boost::python::object(e);
    }

    template <typename T>
    boost::python::object typed_entity_map_remove_by_uid(renderer::TypedEntityMap<T>* map, const foundation::UniqueID id)
    {
        foundation::auto_release_ptr<T> e = map->remove(id);
        return boost::python::object(e);
    }

    template <typename T>
    boost::python::object typed_entity_map_get_iter(renderer::TypedEntityMap<T>* map)
    {
        boost::python::dict items;

        typedef typename renderer::TypedEntityMap<T>::iterator iterator;

        for (iterator it(map->begin()), e(map->end()); it != e; ++it)
            items[it->get_name()] = boost::python::ptr(&(*it));

        return items.attr("__iter__")();
    }

    template <typename T>
    boost::python::list typed_entity_map_get_keys(renderer::TypedEntityMap<T>* map)
    {
        boost::python::list items;

        typedef typename renderer::TypedEntityMap<T>::iterator iterator;

        for (iterator it(map->begin()), e(map->end()); it != e; ++it)
            items.append(it->get_name());

        return items;
    }

    template <typename T>
    boost::python::list typed_entity_map_get_values(renderer::TypedEntityMap<T>* map)
    {
        boost::python::list items;

        typedef typename renderer::TypedEntityMap<T>::iterator iterator;

        for (iterator it(map->begin()), e(map->end()); it != e; ++it)
            items.append(boost::python::ptr(&(*it)));

        return items;
    }

    template <typename T>
    void typed_entity_map_insert(renderer::TypedEntityMap<T>* map, foundation::auto_release_ptr<T> entity)
    {
        if (map->get_by_name(entity->get_name()) != nullptr)
            throw foundation::Exception(foundation::format("Entity {0} already exists", entity->get_name()).c_str());
        else map->insert(entity);
    }
}

template <typename T>
void bind_typed_entity_vector(const char* name)
{
    boost::python::class_<renderer::TypedEntityVector<T>, boost::python::bases<renderer::EntityVector>, boost::noncopyable>(name)
        .def("__getitem__", detail::typed_entity_vector_get_item<T>, boost::python::return_value_policy<boost::python::reference_existing_object>())
        .def("get_by_uid", &renderer::TypedEntityVector<T>::get_by_uid, boost::python::return_value_policy<boost::python::reference_existing_object>())
        .def("get_by_name", &renderer::TypedEntityVector<T>::get_by_name, boost::python::return_value_policy<boost::python::reference_existing_object>())

        .def("insert", &detail::typed_entity_vector_insert<T>)
        .def("remove", &detail::typed_entity_vector_remove<T>)

        .def("__iter__", &detail::typed_entity_vector_get_iter<T>);
}

template <typename T>
void bind_typed_entity_map(const char* name)
{
    boost::python::class_<renderer::TypedEntityMap<T>, boost::python::bases<renderer::EntityMap>, boost::noncopyable>(name)
        .def("__getitem__", detail::typed_entity_map_get_item<T>, boost::python::return_value_policy<boost::python::reference_existing_object>())
        .def("get_by_uid", &renderer::TypedEntityMap<T>::get_by_uid, boost::python::return_value_policy<boost::python::reference_existing_object>())
        .def("get_by_name", &renderer::TypedEntityMap<T>::get_by_name, boost::python::return_value_policy<boost::python::reference_existing_object>())

        .def("insert", &detail::typed_entity_map_insert<T>)
        .def("remove", &detail::typed_entity_map_remove<T>)
        .def("remove_by_uid", &detail::typed_entity_map_remove_by_uid<T>)

        .def("__iter__", &detail::typed_entity_map_get_iter<T>)

        .def("keys", &detail::typed_entity_map_get_keys<T>)
        .def("values", &detail::typed_entity_map_get_values<T>);
}
