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

#include "unaligned_matrix44.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace detail
{

template<class T>
Transform<T>* create_from_matrix( const UnalignedMatrix44<T>& m)
{
    return new Transform<T>( m.as_foundation_matrix());
}

template<class T>
Transform<T>* create_from_matrix_with_inv( const UnalignedMatrix44<T>& m, const UnalignedMatrix44<T>& minv)
{
    Matrix<T,4,4> aligned_m = m.as_foundation_matrix();
    Matrix<T,4,4> aligned_minv = minv.as_foundation_matrix();

    // check that m * minv == identity, if not, throw an exception before the assert fires.
    if ( !feq( aligned_m * aligned_minv, Matrix<T,4,4>::identity(), make_eps<T>(1.0e-6f, 1.0e-9)))
    {
        PyErr_SetString( PyExc_RuntimeError, "Matrices passed to appleseed.Transform are not inverses" );
        bpy::throw_error_already_set();
        return new Transform<T>();
    }

    return new Transform<T>( aligned_m, aligned_minv);
}

template<class T>
UnalignedMatrix44<T> xform_get_local_to_parent( const Transform<T>* xform)
{
    return UnalignedMatrix44<T>( xform->get_local_to_parent());
}

template<class T>
UnalignedMatrix44<T> xform_get_parent_to_local( const Transform<T>* xform)
{
    return UnalignedMatrix44<T>( xform->get_parent_to_local());
}

template<class T>
void bind_typed_transform( const char* class_name)
{
    bpy::class_<Transform<T> >( class_name)
        .def( "__init__", bpy::make_constructor( create_from_matrix<T>))
        .def( "__init__", bpy::make_constructor( create_from_matrix_with_inv<T>))

        .def( "identity", &Transform<T>::identity).staticmethod( "identity")

        .def( "get_local_to_parent", &xform_get_local_to_parent<T>)
        .def( "get_parent_to_local", &xform_get_parent_to_local<T>)

        .def( bpy::self * bpy::self)

        // a bug in boost::python, this needs
        // the extra self_ns qualification
        .def( bpy::self_ns::str( bpy::self))
        .def( bpy::self_ns::repr( bpy::self))
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

} // detail

void bind_transform()
{
    detail::bind_typed_transform<float>( "Transformf");
    detail::bind_typed_transform<double>( "Transformd");

    bpy::class_<TransformSequence, boost::noncopyable>( "TransformSequence", bpy::no_init)
        .def( "clear", &TransformSequence::clear)
        .def( "set_transform", &TransformSequence::set_transform)
        .def( "get_transform", &detail::transform_seq_get_transform)
        .def( "earliest_transform", &detail::transform_seq_get_earliest)
        .def( "empty", &TransformSequence::empty)
        .def( "size", &TransformSequence::size)
        .def( "prepare", &TransformSequence::prepare)
        .def( "evaluate", &TransformSequence::evaluate)
        ;
}
