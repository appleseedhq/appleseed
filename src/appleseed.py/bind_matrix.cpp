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

#include "foundation/math/matrix.h"

namespace bpy = boost::python;
using namespace foundation;

namespace
{

// gcc 4.5 does not compile this if name is a const char *
// probably a bug.
template<class T>
void bind_typed_matrix4( const std::string& class_name)
{
    bpy::class_<Matrix<T,4,4> >( class_name.c_str())
        ;
}

} // unnamed

void bind_matrix()
{
    //bpy::class_<Matrix<float,4,4> >( "Matrix4f")
    //    ;

    //bpy::class_<Matrix<double,4,4> >( "Matrix4d")
    //    ;

    bind_typed_matrix4<float>( "Matrix4f");
    bind_typed_matrix4<double>( "Matrix4d");
}
