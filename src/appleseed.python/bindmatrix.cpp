
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
#include "unalignedmatrix44.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"

// Standard headers.
#include <cstddef>
#include <memory>

namespace bpy = boost::python;
using namespace foundation;

namespace
{
    template <typename T>
    UnalignedMatrix44<T>* construct_matrix_from_list(bpy::list l)
    {
        if (bpy::len(l) != 4 * 4)
        {
            PyErr_SetString(PyExc_RuntimeError, "Invalid list length given to appleseed.Matrix.__init__");
            bpy::throw_error_already_set();
        }

        std::unique_ptr<UnalignedMatrix44<T>> r(new UnalignedMatrix44<T>());

        for (size_t i = 0; i < 4 * 4; ++i)
        {
            bpy::extract<T> ex(l[i]);
            if (!ex.check())
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible type.");
                bpy::throw_error_already_set();
            }

            (*r)[i] = ex();
        }

        return r.release();
    }

    template <typename T>
    struct MatrixIndexer
    {
        static T get(const UnalignedMatrix44<T>& mat, bpy::tuple indices)
        {
            if (bpy::len(indices) != 2)
            {
                PyErr_SetString(PyExc_RuntimeError, "Invalid tuple length given to appleseed.Matrix.__get_item__");
                bpy::throw_error_already_set();
            }

            int i = 0, j = 0;

            bpy::extract<int> ex0(indices[0]);
            if (!ex0.check())
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible index type. Only ints.");
                bpy::throw_error_already_set();
            }
            else
                i = ex0();

            bpy::extract<int> ex1(indices[1]);
            if (!ex1.check())
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible index type. Only ints.");
                bpy::throw_error_already_set();
            }
            else
                j = ex1();

            if (i < 0)
                i = 4 + i;

            if (j < 0)
                j = 4 + j;

            if (i >= 0 && i < 4 && j >= 0 && j < 4)
                return mat(i, j);

            PyErr_SetString(PyExc_IndexError, "Out of bounds access in appleseed.Matrix.__get_item__");
            boost::python::throw_error_already_set();
            return T();
        }

        static void set(UnalignedMatrix44<T>& mat, bpy::tuple indices, const T v)
        {
            if (bpy::len(indices) != 2)
            {
                PyErr_SetString(PyExc_RuntimeError, "Invalid tuple length given to appleseed.Matrix.__set_item__");
                bpy::throw_error_already_set();
            }

            int i = 0, j = 0;

            bpy::extract<int> ex0(indices[0]);
            if (!ex0.check())
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible index type. Only ints.");
                bpy::throw_error_already_set();
            }
            else
                i = ex0();

            bpy::extract<int> ex1(indices[1]);
            if (!ex1.check())
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible index type. Only ints.");
                bpy::throw_error_already_set();
            }
            else
                j = ex1();

            if (i < 0)
                i = 4 + i;

            if (j < 0)
                j = 4 + j;

            if (i >= 0 && i < 4 && j >= 0 && j < 4)
                mat(i, j) = v;
            else
            {
                PyErr_SetString(PyExc_IndexError, "Out of bounds access in appleseed.Matrix.__set_item__");
                boost::python::throw_error_already_set();
            }
        }
    };

    template <typename T>
    UnalignedMatrix44<T> transpose_matrix(const UnalignedMatrix44<T>& mat)
    {
        return UnalignedMatrix44<T>(transpose(mat.as_foundation_matrix()));
    }

    template <typename T>
    bpy::tuple matrix_extract_euler_angles(const UnalignedMatrix44<T>& mat)
    {
        T yaw, pitch, roll;
        mat.as_foundation_matrix().extract_euler_angles(yaw, pitch, roll);
        return bpy::make_tuple(yaw, pitch, roll);
    }

    void bind_typed_matrix4_extra(bpy::class_<UnalignedMatrix44<float>>& X)
    {
        X.def(bpy::init<UnalignedMatrix44<double>>());
    }

    void bind_typed_matrix4_extra(bpy::class_<UnalignedMatrix44<double>>& X)
    {
        X.def(bpy::init<UnalignedMatrix44<float>>());
    }

    template <typename T>
    void bind_typed_matrix4(const char* class_name)
    {
        UnalignedMatrix44<T>(*rot1)(T, T, T) = &UnalignedMatrix44<T>::make_rotation;
        UnalignedMatrix44<T>(*rot2)(const Vector<T, 3>&, T) = &UnalignedMatrix44<T>::make_rotation;
        UnalignedMatrix44<T>(*rot3)(const Quaternion<T>&) = &UnalignedMatrix44<T>::make_rotation;

        bpy::class_<UnalignedMatrix44<T>> X(class_name);

        X.def("identity", &UnalignedMatrix44<T>::identity).staticmethod("identity")
            .def("make_translation", &UnalignedMatrix44<T>::make_translation).staticmethod("make_translation")
            .def("make_scaling", &UnalignedMatrix44<T>::make_scaling).staticmethod("make_scaling")
            .def("make_rotation_x", &UnalignedMatrix44<T>::make_rotation_x).staticmethod("make_rotation_x")
            .def("make_rotation_y", &UnalignedMatrix44<T>::make_rotation_y).staticmethod("make_rotation_y")
            .def("make_rotation_z", &UnalignedMatrix44<T>::make_rotation_z).staticmethod("make_rotation_z")
            .def("make_lookat", &UnalignedMatrix44<T>::make_lookat).staticmethod("make_lookat")
            .def("make_rotation", rot1).def("make_rotation", rot2).def("make_rotation", rot3).staticmethod("make_rotation")

            .def(bpy::init<T>())
            .def("__init__", bpy::make_constructor(&construct_matrix_from_list<T>))

            // operator[]
            .def("__getitem__", &MatrixIndexer<T>::get)
            .def("__setitem__", &MatrixIndexer<T>::set)

            .def("transpose", &transpose_matrix<T>)
            .def("inverse", &invert_matrix<T>)

            .def(bpy::self * bpy::self)
            .def(bpy::self * Vector<T, 4>())

            // Because of a bug in Boost.Python, this needs the extra self_ns qualification.
            .def(bpy::self_ns::str(bpy::self))
            .def(bpy::self_ns::repr(bpy::self))

            .def("extract_matrix3", &UnalignedMatrix44<T>::extract_matrix3)
            .def("extract_translation", &UnalignedMatrix44<T>::extract_translation);

        bind_typed_matrix4_extra(X);
    }
}

void bind_matrix()
{
    bind_typed_matrix4<float>("Matrix4f");
    bind_typed_matrix4<double>("Matrix4d");

#ifdef APPLESEED_ENABLE_IMATH_INTEROP
    bpy::implicitly_convertible<UnalignedMatrix44<float>, Imath::M44f>();
    bpy::implicitly_convertible<Imath::M44f, UnalignedMatrix44<float>>();

    bpy::implicitly_convertible<UnalignedMatrix44<double>, Imath::M44d>();
    bpy::implicitly_convertible<Imath::M44d, UnalignedMatrix44<double>>();
#endif
}
