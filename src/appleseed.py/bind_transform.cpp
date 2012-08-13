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

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{

template<class T>
void bind_typed_transform( const char *class_name)
{
    typedef typename Transform<T>::MatrixType MatrixType;

    bpy::class_<Transform<T> >( class_name)
        .def( bpy::init<const MatrixType&>())
        .def( bpy::init<const MatrixType&, const MatrixType&>())

        .def( "identity", &Transform<T>::identity).staticmethod( "identity")

        .def( "get_local_to_parent", &Transform<T>::get_local_to_parent, bpy::return_value_policy<bpy::copy_const_reference>())
        .def( "get_parent_to_local", &Transform<T>::get_parent_to_local, bpy::return_value_policy<bpy::copy_const_reference>())

        .def( bpy::self * bpy::self)
        ;
}

bpy::tuple transform_seq_get_transform( const TransformSequence& seq, std::size_t index)
{
    double time;
    Transformd xform;
    seq.get_transform( index, time, xform);
    return bpy::make_tuple( time, xform);
}

Transformd transform_seq_get_earliest( const TransformSequence& seq)
{
    return seq.earliest_transform();
}

} // unnamed

void bind_transform()
{
    bind_typed_transform<float>( "Transformf");
    bind_typed_transform<double>( "Transformd");

    bpy::class_<TransformSequence, boost::noncopyable>( "TransformSequence", bpy::no_init)
        .def( "clear", &TransformSequence::clear)
        .def( "set_transform", &TransformSequence::set_transform)
        .def( "get_transform", &transform_seq_get_transform)
        .def( "earliest_transform", &transform_seq_get_earliest)
        .def( "empty", &TransformSequence::empty)
        .def( "size", &TransformSequence::size)
        .def( "prepare", &TransformSequence::prepare)
        .def( "evaluate", &TransformSequence::evaluate)
        ;
}
