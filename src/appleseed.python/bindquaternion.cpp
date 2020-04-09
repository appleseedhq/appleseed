
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
#include "foundation/math/quaternion.h"
#include "foundation/platform/python.h"
#include "foundation/utility/iostreamop.h"

namespace bpy = boost::python;
using namespace foundation;

namespace
{
    template <typename T>
    T quat_dot_prod(const Quaternion<T>& a, const Quaternion<T>& b)
    {
        return dot(a, b);
    }

    template <typename T>
    bpy::tuple quat_extract_axis_angle(const Quaternion<T>& q)
    {
        Vector<T, 3> axis;
        T angle;

        q.extract_axis_angle(axis, angle);
        return bpy::make_tuple(axis, angle);
    }

    template <typename T>
    Quaternion<T> quat_conjugate(const Quaternion<T>& q)
    {
        return conjugate(q);
    }

    template <typename T>
    Quaternion<T> quat_inverse(const Quaternion<T>& q)
    {
        return inverse(q);
    }

    template <typename T>
    T quat_square_norm(const Quaternion<T>& q)
    {
        return square_norm(q);
    }

    template <typename T>
    T quat_norm(const Quaternion<T>& q)
    {
        return norm(q);
    }

    template <typename T>
    Quaternion<T> quat_normalize(const Quaternion<T>& q)
    {
        return normalize(q);
    }

    template <typename T>
    bool quat_is_normalized(const Quaternion<T>& q)
    {
        return is_normalized(q);
    }

    template <typename T>
    bool quat_is_normalized_with_eps(const Quaternion<T>& q, const T eps)
    {
        return is_normalized(q, eps);
    }

    template <typename T>
    Quaternion<T> quat_slerp(const Quaternion<T>& p, const Quaternion<T>& q, const T t)
    {
        return slerp(p, q, t);
    }

    template <typename T>
    void do_bind_quaternion(const char* class_name)
    {
        Quaternion<T>(*rot1)(const Vector<T, 3>&, T) = &Quaternion<T>::make_rotation;
        Quaternion<T>(*rot2)(const Vector<T, 3>&, const Vector<T, 3>&) = &Quaternion<T>::make_rotation;

        bpy::class_<Quaternion<T>>(class_name)
            .def("make_identity", &Quaternion<T>::make_identity).staticmethod("make_identity")
            .def("make_rotation", rot1).def("make_rotation", rot2).staticmethod("make_rotation")

            .def(bpy::init<>())
            .def(bpy::init<T, Vector<T, 3>>())

            .def_readwrite("s", &Quaternion<T>::s)
            .def_readwrite("v", &Quaternion<T>::v)

            // Operators.
            .def(bpy::self + bpy::self)
            .def(bpy::self - bpy::self)
            .def(-bpy::self)
            .def(bpy::self * T())
            .def(T() * bpy::self)
            .def(bpy::self / T())
            .def(bpy::self += bpy::self)
#if defined(__clang__) && !defined(__APPLE__)
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wself-assign-overloaded"
#endif
            .def(bpy::self -= bpy::self)
#if defined(__clang__) && !defined(__APPLE__)
    #pragma clang diagnostic pop
#endif
            .def(bpy::self *= T())
            .def(bpy::self /= T())
            .def(bpy::self * bpy::self)
            .def(bpy::self *= bpy::self)

            // Because of a bug in Boost.Python, this needs the extra self_ns qualification.
            .def(bpy::self_ns::str(bpy::self))
            .def(bpy::self_ns::repr(bpy::self))

            .def("dot", &quat_dot_prod<T>)
            .def("extract_axis_angle", &quat_extract_axis_angle<T>)

            .def("conjugate", &quat_conjugate<T>)
            .def("inverse", &quat_inverse<T>)
            .def("square_norm", &quat_square_norm<T>)
            .def("norm", &quat_norm<T>)
            .def("normalize", &quat_normalize<T>)
            .def("is_normalized", &quat_is_normalized<T>)
            .def("is_normalized", &quat_is_normalized_with_eps<T>)
            .def("slerp", &quat_slerp<T>);
    }
}

void bind_quaternion()
{
    do_bind_quaternion<float>("Quaternionf");
    do_bind_quaternion<double>("Quaterniond");

#ifdef APPLESEED_ENABLE_IMATH_INTEROP
    bpy::implicitly_convertible<Quaternionf, Imath::Quatf>();
    bpy::implicitly_convertible<Imath::Quatf, Quaternionf>();

    bpy::implicitly_convertible<Quaterniond, Imath::Quatd>();
    bpy::implicitly_convertible<Imath::Quatd, Quaterniond>();
#endif
}
