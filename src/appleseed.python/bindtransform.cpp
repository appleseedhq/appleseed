
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
#include "unalignedtransform.h"

// appleseed.renderer headers.
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"
#include "foundation/utility/iostreamop.h"

// Standard headers.
#include <cstddef>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    void bind_typed_transform_extra(bpy::class_<UnalignedTransformf>& x)
    {
        x.def(bpy::init<const UnalignedTransformd&>());
    }

    void bind_typed_transform_extra(bpy::class_<UnalignedTransformd>& x)
    {
        x.def(bpy::init<const UnalignedTransformf&>());
    }

    template <typename T>
    void bind_typed_transform(const char* class_name)
    {
        bpy::class_<UnalignedTransform<T>> x(class_name);
        x.def(bpy::init<const UnalignedMatrix44<T>&>())
            .def(bpy::init<const UnalignedMatrix44<T>&>())
            .def(bpy::init<const UnalignedMatrix44<T>&, const UnalignedMatrix44<T>&>())

            .def("identity", &UnalignedTransform<T>::identity).staticmethod("identity")

            .def("get_local_to_parent", &UnalignedTransform<T>::get_local_to_parent, bpy::return_value_policy<bpy::copy_const_reference>())
            .def("get_parent_to_local", &UnalignedTransform<T>::get_parent_to_local, bpy::return_value_policy<bpy::copy_const_reference>())

            .def(bpy::self * bpy::self)

            .def("point_to_local",   &UnalignedTransform<T>::point_to_local)
            .def("point_to_parent",  &UnalignedTransform<T>::point_to_parent)
            .def("vector_to_local",  &UnalignedTransform<T>::vector_to_local)
            .def("vector_to_parent", &UnalignedTransform<T>::vector_to_parent)
            .def("normal_to_local" , &UnalignedTransform<T>::normal_to_local)
            .def("normal_to_parent", &UnalignedTransform<T>::normal_to_parent)

            // Because of a bug in Boost.Python, this needs the extra self_ns qualification.
            .def(bpy::self_ns::str(bpy::self))
            .def(bpy::self_ns::repr(bpy::self));

        bind_typed_transform_extra(x);
    }

    void transform_seq_set_transform(
        TransformSequence*              seq,
        const float                     time,
        const UnalignedTransformd&      transform)
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
        float time;
        Transformd xform;
        seq->get_transform(index, time, xform);
        return bpy::make_tuple(time, UnalignedTransformd(xform));
    }

    bpy::list transform_seq_as_list(const TransformSequence* seq)
    {
        bpy::list result;

        for (size_t i = 0, e = seq->size(); i < e; ++i)
        {
            float time;
            Transformd xform;
            seq->get_transform(i, time, xform);
            bpy::tuple t = bpy::make_tuple(time, UnalignedTransformd(xform));
            result.append(t);
        }

        return result;
    }

    UnalignedTransformd transform_seq_get_earliest(const TransformSequence* seq)
    {
        const Transformd xform(seq->get_earliest_transform());
        return UnalignedTransformd(xform);
    }
}

void bind_transform()
{
    bind_typed_transform<float>("Transformf");
    bind_typed_transform<double>("Transformd");

    bpy::class_<TransformSequence>("TransformSequence")
        .def(bpy::init<>())
        .def("set_transform", &transform_seq_set_transform)
        .def("get_transform", &transform_seq_get_transform)
        .def("get_earliest_transform", &transform_seq_get_earliest)

        .def("empty", &TransformSequence::empty)
        .def("size", &TransformSequence::size)
        .def("clear", &TransformSequence::clear)

        .def("optimize", &TransformSequence::optimize)

        .def("transforms", &transform_seq_as_list)

        .def(bpy::self * bpy::self);
}
