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

// Has to be first, to avoid redifinition warnings.
#include "bind_auto_release_ptr.h"

#include "renderer/utility/transformsequence.h"
#include "foundation/utility/iostreamop.h"

#include "unaligned_transform44.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace detail
{


template<class T>
void bind_typed_transform( const char* class_name)
{
    bpy::class_<UnalignedTransform44<T> >( class_name)
        .def( bpy::init<const UnalignedMatrix44<T>&>())
        .def( bpy::init<const UnalignedMatrix44<T>&, const UnalignedMatrix44<T>&>())
        .def( "identity", &UnalignedTransform44<T>::identity).staticmethod( "identity")

        .def( "get_local_to_parent", &UnalignedTransform44<T>::get_local_to_parent, bpy::return_value_policy<bpy::copy_const_reference>())
        .def( "get_parent_to_local", &UnalignedTransform44<T>::get_parent_to_local, bpy::return_value_policy<bpy::copy_const_reference>())

        .def( bpy::self * bpy::self)

        // the extra self_ns qualification
        .def( bpy::self_ns::str( bpy::self))
        .def( bpy::self_ns::repr( bpy::self))
        ;
}

void transform_seq_set_transform( TransformSequence* seq, const double time, const UnalignedTransform44<float>& transform)
{
    Transformd xform( transform.get_local_to_parent().as_foundation_matrix(),
                        transform.get_parent_to_local().as_foundation_matrix());

    seq->set_transform( time, xform);
}

bpy::tuple transform_seq_get_transform( const TransformSequence* seq, std::size_t index)
{
    double time;
    Transformd xform;
    seq->get_transform( index, time, xform);
    return bpy::make_tuple( time, UnalignedTransform44<float>(xform));
}

UnalignedTransform44<float> transform_seq_get_earliest( const TransformSequence* seq)
{
    Transformd xform( seq->earliest_transform());
    return UnalignedTransform44<float>( xform);
}

} // detail

void bind_transform()
{
    detail::bind_typed_transform<float>( "Transformf");
    detail::bind_typed_transform<double>( "Transformd");

    bpy::class_<TransformSequence, boost::noncopyable>( "TransformSequence", bpy::no_init)
        .def( "set_transform", &detail::transform_seq_set_transform)
        .def( "get_transform", &detail::transform_seq_get_transform)
        .def( "earliest_transform", &detail::transform_seq_get_earliest)

        .def( "empty", &TransformSequence::empty)
        .def( "size", &TransformSequence::size)
        .def( "clear", &TransformSequence::clear)
        ;
}
