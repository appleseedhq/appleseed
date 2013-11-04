
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
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

// Has to be first, to avoid redefinition warnings.
#include "boost/python/detail/wrap_python.hpp"

// appleseed.python headers.
#include "unaligned_matrix44.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"

namespace bpy = boost::python;
using namespace foundation;

namespace detail
{
    template <typename T>
    UnalignedMatrix44<T>* construct_matrix_from_list(bpy::list l)
    {
        if (bpy::len(l) != 4 * 4)
        {
            PyErr_SetString(PyExc_RuntimeError, "Invalid list length given to appleseed.Matrix.__init__");
            bpy::throw_error_already_set();
        }

        UnalignedMatrix44<T>* r = new UnalignedMatrix44<T>();

        for (unsigned i = 0; i < 4 * 4; ++i)
        {
            bpy::extract<T> ex(l[i]);
            if (!ex.check())
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible type. Only floats.");
                bpy::throw_error_already_set();
            }

            (*r)[i] = ex();
        }

        return r;
    }

    template <typename T>
    struct matrix_indexer
    {
        static T get(const UnalignedMatrix44<T>& mat, bpy::tuple indices)
        {
            if (bpy::len(indices) != 2)
            {
                PyErr_SetString(PyExc_RuntimeError, "Invalid tuple length given to appleseed.Matrix.__get_item__");
                bpy::throw_error_already_set();
            }

            int i, j;

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

            int i, j;

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

    template <class T>
    UnalignedMatrix44<T> transpose_matrix(const UnalignedMatrix44<T>& mat)
    {
        return UnalignedMatrix44<T>(transpose(mat.as_foundation_matrix()));
    }

    template <class T>
    bpy::tuple matrix_extract_euler_angles(const UnalignedMatrix44<T>& mat)
    {
        T yaw, pitch, roll;
        mat.as_foundation_matrix().extract_euler_angles(yaw, pitch, roll);
        return bpy::make_tuple(yaw, pitch, roll);
    }

    void bind_typed_matrix4_extra(bpy::class_<UnalignedMatrix44<float> >& X)
    {
        X.def(bpy::init<UnalignedMatrix44<double> >());
    }

    void bind_typed_matrix4_extra(bpy::class_<UnalignedMatrix44<double> >& X)
    {
        X.def(bpy::init<UnalignedMatrix44<float> >());
    }

    template <class T>
    void bind_typed_matrix4(const char* class_name)
    {
        UnalignedMatrix44<T> (*rot1)(T, T, T) = &UnalignedMatrix44<T>::rotation;
        UnalignedMatrix44<T> (*rot2)(const Vector<T,3>&, T) = &UnalignedMatrix44<T>::rotation;
        UnalignedMatrix44<T> (*rot3)(const Quaternion<T>&) = &UnalignedMatrix44<T>::rotation;

        bpy::class_<UnalignedMatrix44<T> > X(class_name);

        X.def("identity", &UnalignedMatrix44<T>::identity).staticmethod("identity")
         .def("translation", &UnalignedMatrix44<T>::translation).staticmethod("translation")
         .def("scaling", &UnalignedMatrix44<T>::scaling).staticmethod("scaling")
         .def("rotation_x", &UnalignedMatrix44<T>::rotation_x).staticmethod("rotation_x")
         .def("rotation_y", &UnalignedMatrix44<T>::rotation_y).staticmethod("rotation_y")
         .def("rotation_z", &UnalignedMatrix44<T>::rotation_z).staticmethod("rotation_z")
         .def("lookat", &UnalignedMatrix44<T>::lookat).staticmethod("lookat")
         .def("rotation", rot1).def("rotation", rot2).def("rotation", rot3).staticmethod("rotation")

         .def(bpy::init<T>())
         .def("__init__", bpy::make_constructor(&construct_matrix_from_list<T>))

         // operator[]
         .def("__getitem__", &matrix_indexer<T>::get)
         .def("__setitem__", &matrix_indexer<T>::set)

         .def("transpose", &transpose_matrix<T>)
         .def("inverse", &invert_matrix<T>)

         .def(bpy::self * bpy::self)
         .def(bpy::self * Vector<T,4>())

         // a bug in boost::python, this needs the extra self_ns qualification
         .def(bpy::self_ns::str(bpy::self))
         .def(bpy::self_ns::repr(bpy::self))

         .def("extract_matrix3", &UnalignedMatrix44<T>::extract_matrix3)
         .def("extract_translation", &UnalignedMatrix44<T>::extract_translation)
         ;

        bind_typed_matrix4_extra(X);
    }
}

void bind_matrix()
{
    detail::bind_typed_matrix4<float>("Matrix4f");
    detail::bind_typed_matrix4<double>("Matrix4d");
}
