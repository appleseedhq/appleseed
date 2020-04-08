
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

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/python.h"
#include "foundation/utility/iostreamop.h"

// Standard headers.
#include <cstddef>
#include <memory>
#include <type_traits>

namespace bpy = boost::python;
using namespace foundation;

namespace
{
    template <typename T, size_t N>
    Vector<T, N>* construct_vec_from_list(const bpy::list& l)
    {
        if (bpy::len(l) != N)
        {
            PyErr_SetString(PyExc_RuntimeError, "Invalid list length given to appleseed.Vector");
            bpy::throw_error_already_set();
        }

        std::unique_ptr<Vector<T, N>> r(new Vector<T, N>());

        for (size_t i = 0; i < N; ++i)
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

    template <typename T, size_t N>
    struct VectorHelper {};

    template <typename T>
    struct VectorHelper<T, 2>
    {
        typedef Vector<T, 2> VectorType;

        static VectorType* construct(const T x, const T y)
        {
            return new VectorType(x, y);
        }

        static T norm(const VectorType& v)
        {
            return foundation::norm(v);
        }

        static VectorType normalize(const VectorType& v)
        {
            return foundation::normalize(v);
        }

        static T dot(const VectorType& lhs, const VectorType& rhs)
        {
            return foundation::dot(lhs, rhs);
        }
    };

    template <typename T>
    struct VectorHelper<T, 3>
    {
        typedef Vector<T, 3> VectorType;

        static VectorType* construct(const T x, const T y, const T z)
        {
            return new VectorType(x, y, z);
        }

        static T norm(const VectorType& v)
        {
            return foundation::norm(v);
        }

        static VectorType normalize(const VectorType& v)
        {
            return foundation::normalize(v);
        }

        static T dot(const VectorType& lhs, const VectorType& rhs)
        {
            return foundation::dot(lhs, rhs);
        }

        static VectorType cross(const VectorType& lhs, const VectorType& rhs)
        {
            return foundation::cross(lhs, rhs);
        }
    };

    template <typename T>
    struct VectorHelper<T, 4>
    {
        typedef Vector<T, 4> VectorType;

        static VectorType* construct(const T x, const T y, const T z, const T w)
        {
            return new VectorType(x, y, z, w);
        }

        static T norm(const VectorType& v)
        {
            return foundation::norm(v);
        }

        static VectorType normalize(const VectorType& v)
        {
            return foundation::normalize(v);
        }

        static T dot(const VectorType& lhs, const VectorType& rhs)
        {
            return foundation::dot(lhs, rhs);
        }
    };

    template <typename T, size_t N>
    struct vector_indexer
    {
        static T get(const Vector<T, N>& x, int i)
        {
            if (i < 0)
                i = N + i;

            if (i >= 0 && i < N)
                return x[i];
            else
            {
                PyErr_SetString(PyExc_IndexError, "Invalid index in appleseed.Vector");
                boost::python::throw_error_already_set();
            }

            return T();
        }

        static void set(Vector<T, N>& x, int i, const T& v)
        {
            if (i < 0)
                i = N + i;

            if (i >= 0 && i < N)
                x[i] = v;
            else
            {
                PyErr_SetString(PyExc_IndexError, "Invalid index in appleseed.Vector");
                boost::python::throw_error_already_set();
            }
        }
    };

    struct Signed {};
    struct Unsigned {};

    // Expose operators that work on both vectors with signed and unsigned components.
    template <typename T, size_t N>
    bpy::class_<Vector<T, N>> do_bind_vector(const char* class_name, const Unsigned&)
    {
        bpy::def("dot", &VectorHelper<T, N>::dot);

        return
            bpy::class_<Vector<T, N>>(class_name)
                .def(bpy::init<>())
                .def(bpy::init<T>())
                .def("__init__", bpy::make_constructor(&VectorHelper<T, N>::construct))
                .def("__init__", bpy::make_constructor(&construct_vec_from_list<T, N>))

                // operator[]
                .def("__getitem__", &vector_indexer<T, N>::get)
                .def("__setitem__", &vector_indexer<T, N>::set)

                // Operators.
                .def(bpy::self += bpy::self)
                .def(bpy::self + bpy::self)
#if defined(__clang__) && !defined(__APPLE__)
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wself-assign-overloaded"
#endif
                .def(bpy::self -= bpy::self)
#if defined(__clang__) && !defined(__APPLE__)
    #pragma clang diagnostic pop
#endif
                .def(bpy::self - bpy::self)

                .def(bpy::self *= T())
                .def(bpy::self * T())
                .def(T() * bpy::self)

                .def(bpy::self /= T())
                .def(bpy::self / bpy::self)
                .def(bpy::self / T())
                .def(bpy::self == bpy::self)
                .def(bpy::self != bpy::self)

                // Because of a bug in Boost.Python, this needs the extra self_ns qualification.
                .def(bpy::self_ns::str(bpy::self))
                .def(bpy::self_ns::repr(bpy::self));
    }

    // Expose operators that only work on vectors with signed components.
    template <typename T, size_t N>
    void do_bind_vector(const char* class_name, const Signed&)
    {
        do_bind_vector<T, N>(class_name, Unsigned())
            .def(-bpy::self);
    }
}

void bind_vector()
{
    do_bind_vector<int, 2>("Vector2i", Signed());
    do_bind_vector<size_t, 2>("Vector2u", Unsigned());
    do_bind_vector<float, 2>("Vector2f", Signed());
    do_bind_vector<double, 2>("Vector2d", Signed());

    do_bind_vector<int, 3>("Vector3i", Signed());
    do_bind_vector<size_t, 3>("Vector3u", Unsigned());
    do_bind_vector<float, 3>("Vector3f", Signed());
    do_bind_vector<double, 3>("Vector3d", Signed());

    do_bind_vector<int, 4>("Vector4i", Signed());
    do_bind_vector<size_t, 4>("Vector4u", Unsigned());
    do_bind_vector<float, 4>("Vector4f", Signed());
    do_bind_vector<double, 4>("Vector4d", Signed());

    bpy::def("norm", &VectorHelper<float,  2>::norm);
    bpy::def("norm", &VectorHelper<double, 2>::norm);
    bpy::def("norm", &VectorHelper<float,  3>::norm);
    bpy::def("norm", &VectorHelper<double, 3>::norm);
    bpy::def("norm", &VectorHelper<float,  4>::norm);
    bpy::def("norm", &VectorHelper<double, 4>::norm);

    bpy::def("normalize", &VectorHelper<float,  2>::normalize);
    bpy::def("normalize", &VectorHelper<double, 2>::normalize);
    bpy::def("normalize", &VectorHelper<float,  3>::normalize);
    bpy::def("normalize", &VectorHelper<double, 3>::normalize);
    bpy::def("normalize", &VectorHelper<float,  4>::normalize);
    bpy::def("normalize", &VectorHelper<double, 4>::normalize);

    bpy::def("cross", &VectorHelper<float, 3>::cross);
    bpy::def("cross", &VectorHelper<double, 3>::cross);

#ifdef APPLESEED_ENABLE_IMATH_INTEROP
    bpy::implicitly_convertible<Vector2i, Imath::V2i>();
    bpy::implicitly_convertible<Imath::V2i, Vector2i>();

    bpy::implicitly_convertible<Vector2f, Imath::V2f>();
    bpy::implicitly_convertible<Imath::V2f, Vector2f>();

    bpy::implicitly_convertible<Vector2d, Imath::V2d>();
    bpy::implicitly_convertible<Imath::V2d, Vector2d>();

    bpy::implicitly_convertible<Vector3i, Imath::V3i>();
    bpy::implicitly_convertible<Imath::V3i, Vector3i>();

    bpy::implicitly_convertible<Vector3f, Imath::V3f>();
    bpy::implicitly_convertible<Imath::V3f, Vector3f>();

    bpy::implicitly_convertible<Vector3d, Imath::V3d>();
    bpy::implicitly_convertible<Imath::V3d, Vector3d>();
#endif
}
