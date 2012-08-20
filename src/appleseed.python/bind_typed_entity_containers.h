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

#ifndef APPLESEED_PYTHON_BIND_TYPED_ENTITY_CONTAINERS_H
#define APPLESEED_PYTHON_BIND_TYPED_ENTITY_CONTAINERS_H

// Has to be first, to avoid redifinition warnings.
#include "Python.h"

#include <boost/python.hpp>

#include "renderer/modeling/entity/entityvector.h"
#include "renderer/modeling/entity/entitymap.h"

#include "dict2dict.h"

namespace detail
{

template<class T>
T* typed_entity_vector_get_item( renderer::TypedEntityVector<T>& vec, int index)
{
    if (index < 0)
        index = vec.size() + index;

    if (index < 0 || static_cast<size_t>(index) >= vec.size())
    {
        PyErr_SetString( PyExc_IndexError, "Invalid index in appleseed.EntityVector" );
        boost::python::throw_error_already_set();
    }

    return vec.get_by_index( index);
}

template<class T>
T* typed_entity_map_get_item( renderer::TypedEntityMap<T>& map, const std::string& key)
{
    return map.get_by_name( key.c_str());
}

} // detail

template<class T>
void bind_typed_entity_vector( const char* name)
{
    boost::python::class_<renderer::TypedEntityVector<T>, boost::python::bases<renderer::EntityVector>, boost::noncopyable>( name)
        .def( "__getitem__", detail::typed_entity_vector_get_item<T>, boost::python::return_value_policy<boost::python::reference_existing_object>())
        .def( "get_by_uid", &renderer::TypedEntityVector<T>::get_by_uid, boost::python::return_value_policy<boost::python::reference_existing_object>())
        .def( "get_by_name", &renderer::TypedEntityVector<T>::get_by_name, boost::python::return_value_policy<boost::python::reference_existing_object>())

        .def( "insert", &renderer::TypedEntityVector<T>::insert)

        .def( "__iter__", boost::python::iterator<renderer::TypedEntityVector<T> >())
        ;
}

template<class T>
void bind_typed_entity_map( const char* name)
{
    boost::python::class_<renderer::TypedEntityMap<T>, boost::python::bases<renderer::EntityMap>, boost::noncopyable>( name)
        .def( "__getitem__", detail::typed_entity_map_get_item<T>, boost::python::return_value_policy<boost::python::reference_existing_object>())

        .def( "get_by_uid", &renderer::TypedEntityMap<T>::get_by_uid, boost::python::return_value_policy<boost::python::reference_existing_object>())
        .def( "get_by_name", &renderer::TypedEntityMap<T>::get_by_name, boost::python::return_value_policy<boost::python::reference_existing_object>())

        .def( "insert", &renderer::TypedEntityMap<T>::insert)

        //.def( "__iter__", boost::python::iterator<renderer::TypedEntityMap<T> >())
        ;
}

#endif  // !APPLESEED_PYTHON_BIND_TYPED_ENTITY_CONTAINERS_H
