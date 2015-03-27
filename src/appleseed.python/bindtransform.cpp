
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "pyseed.h" // has to be first, to avoid redefinition warnings
#include "unalignedtransformd44.h"

// appleseed.renderer headers.
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/utility/iostreamop.h"

// Standard headers.
#include <cstddef>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    void transform_seq_set_transform(
        TransformSequence*              seq,
        const double                    time,
        const UnalignedTransformd44&    transform)
    {
        const Transformd xform(
            transform.get_local_to_parent().as_foundation_matrix(),
            transform.get_parent_to_local().as_foundation_matrix());

        seq->set_transform(time, xform);
    }

    bpy::tuple transform_seq_get_transform(
        const TransformSequence*        seq,
        const size_t                    index)
    {
        double time;
        Transformd xform;
        seq->get_transform(index, time, xform);
        return bpy::make_tuple(time, UnalignedTransformd44(xform));
    }

    bpy::list transform_seq_as_list(const TransformSequence* seq)
    {
        bpy::list result;

        for (size_t i = 0, e = seq->size(); i < e; ++i)
        {
            double time;
            Transformd xform;
            seq->get_transform(i, time, xform);
            bpy::tuple t = bpy::make_tuple(time, UnalignedTransformd44(xform));
            result.append(t);
        }

        return result;
    }

    UnalignedTransformd44 transform_seq_get_earliest(const TransformSequence* seq)
    {
        const Transformd xform(seq->get_earliest_transform());
        return UnalignedTransformd44(xform);
    }
}

void bind_transform()
{
    Vector<float, 3> (UnalignedTransformd44::*point_to_localf)(const Vector<float, 3>&) const = &UnalignedTransformd44::point_to_local;
    Vector<double, 3> (UnalignedTransformd44::*point_to_locald)(const Vector<double, 3>&) const = &UnalignedTransformd44::point_to_local;

    Vector<float, 3> (UnalignedTransformd44::*point_to_parentf)(const Vector<float, 3>&) const= &UnalignedTransformd44::point_to_parent;
    Vector<double, 3> (UnalignedTransformd44::*point_to_parentd)(const Vector<double, 3>&) const= &UnalignedTransformd44::point_to_parent;

    Vector<float, 3> (UnalignedTransformd44::*vector_to_localf)(const Vector<float, 3>&) const= &UnalignedTransformd44::vector_to_local;
    Vector<double, 3> (UnalignedTransformd44::*vector_to_locald)(const Vector<double, 3>&) const= &UnalignedTransformd44::vector_to_local;

    Vector<float, 3> (UnalignedTransformd44::*vector_to_parentf)(const Vector<float, 3>&) const = &UnalignedTransformd44::vector_to_parent;
    Vector<double, 3> (UnalignedTransformd44::*vector_to_parentd)(const Vector<double, 3>&) const = &UnalignedTransformd44::vector_to_parent;

    Vector<float, 3> (UnalignedTransformd44::*normal_to_localf)(const Vector<float, 3>&) const = &UnalignedTransformd44::normal_to_local;
    Vector<double, 3> (UnalignedTransformd44::*normal_to_locald)(const Vector<double, 3>&) const = &UnalignedTransformd44::normal_to_local;

    Vector<float, 3> (UnalignedTransformd44::*normal_to_parentf)(const Vector<float, 3>&) const = &UnalignedTransformd44::normal_to_parent;
    Vector<double, 3> (UnalignedTransformd44::*normal_to_parentd)(const Vector<double, 3>&) const = &UnalignedTransformd44::normal_to_parent;

    bpy::class_<UnalignedTransformd44>("Transformd")
        .def(bpy::init<const UnalignedMatrix44<double>&>())
        .def(bpy::init<const UnalignedMatrix44<double>&, const UnalignedMatrix44<double>&>())

        .def(bpy::init<const UnalignedMatrix44<float>&>())
        .def(bpy::init<const UnalignedMatrix44<float>&, const UnalignedMatrix44<float>&>())

        .def("identity", &UnalignedTransformd44::identity).staticmethod("identity")

        .def("get_local_to_parent", &UnalignedTransformd44::get_local_to_parent, bpy::return_value_policy<bpy::copy_const_reference>())
        .def("get_parent_to_local", &UnalignedTransformd44::get_parent_to_local, bpy::return_value_policy<bpy::copy_const_reference>())

        .def(bpy::self * bpy::self)

        .def("point_to_local", point_to_localf)
        .def("point_to_local", point_to_locald)

        .def("point_to_parent", point_to_parentf)
        .def("point_to_parent", point_to_parentd)

        .def("vector_to_local", vector_to_localf)
        .def("vector_to_local", vector_to_locald)

        .def("vector_to_parent", vector_to_parentf)
        .def("vector_to_parent", vector_to_parentd)

        .def("normal_to_local", normal_to_localf)
        .def("normal_to_local", normal_to_locald)

        .def("normal_to_parent", normal_to_parentf)
        .def("normal_to_parent", normal_to_parentd)

        // Because of a bug in Boost.Python, this needs the extra self_ns qualification.
        .def(bpy::self_ns::str(bpy::self))
        .def(bpy::self_ns::repr(bpy::self))
        ;

    bpy::class_<TransformSequence>("TransformSequence")
        .def(bpy::init<>())
        .def("set_transform", &transform_seq_set_transform)
        .def("get_transform", &transform_seq_get_transform)
        .def("get_earliest_transform", &transform_seq_get_earliest)

        .def("empty", &TransformSequence::empty)
        .def("size", &TransformSequence::size)
        .def("clear", &TransformSequence::clear)

        .def("transforms", &transform_seq_as_list)

        .def(bpy::self * bpy::self)
        ;
}
