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
#include "Python.h"

#include <boost/python.hpp>

#include "foundation/math/aabb.h"
#include "foundation/utility/iostreamop.h"

namespace bpy = boost::python;
using namespace foundation;

namespace
{

template<class T>
void bind_aabb3( const char *class_name)
{
    bpy::class_<AABB<T,3> >( class_name)
        .def_readwrite( "min", &AABB<T,3>::min)
        .def_readwrite( "max", &AABB<T,3>::max)

        // a bug in boost::python, this needs
        // the extra self_ns qualification
        .def( bpy::self_ns::str( bpy::self))
        .def( bpy::self_ns::repr( bpy::self))
        ;
}

} // unnamed

void bind_bbox()
{
    bind_aabb3<float>( "AABB3f");
    bind_aabb3<double>( "AABB3d");
}
