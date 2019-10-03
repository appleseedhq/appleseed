
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/platform/python.h"
#include "foundation/utility/iostreamop.h"

// Standard headers.
#include <cstddef>
#include <memory>

namespace bpy = boost::python;
using namespace foundation;

namespace
{
    template <typename T>
    Vector<T, 3> transform_to_local(const Basis3<T>& basis, const Vector<T, 3>& v)
    {
        return basis.transform_to_local(v);
    }

    template <typename T>
    Vector<T, 3> transform_to_parent(const Basis3<T>& basis, const Vector<T, 3>& v)
    {
        return basis.transform_to_parent(v);
    }

    template <typename T>
    void do_bind_basis(const char* class_name)
    {
        bpy::class_<Basis3<T>>(class_name)
            .def(bpy::init<>())
            .def(bpy::init<Vector<T, 3>>())
            .def(bpy::init<Vector<T, 3>, Vector<T, 3>>())
            .def(bpy::init<Vector<T, 3>, Vector<T, 3>, Vector<T, 3>>())

            .def("transform_to_local", &transform_to_local<T>)
            .def("transform_to_parent", &transform_to_parent<T>)
            .def("get_normal", &Basis3<T>::get_normal, bpy::return_value_policy<bpy::copy_const_reference>())
            .def("get_tangent_u", &Basis3<T>::get_tangent_u, bpy::return_value_policy<bpy::copy_const_reference>())
            .def("get_tangent_v", &Basis3<T>::get_tangent_v, bpy::return_value_policy<bpy::copy_const_reference>());
    }
}

void bind_basis()
{
    do_bind_basis<float>("Basis3f");
    do_bind_basis<double>("Basis3d");
}
