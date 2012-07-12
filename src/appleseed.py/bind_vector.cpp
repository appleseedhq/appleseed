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

#include "foundation/math/vector.h"

namespace bpy = boost::python;
using namespace foundation;

namespace
{

template<typename T, size_t N>
Vector<T,N> *construct_from_list( bpy::list l)
{
	if( bpy::len( l) != N)
	{
		//throw InvalidArgumentException( std::string( "Invalid list length given to IECore." ) + typeName<V>() + " constructor" );
		// TODO.
	}

	Vector<T,N> *r = new Vector<T,N>();

	for( unsigned i = 0; i < N; ++i)
	{
		bpy::extract<T> ex( l[i]);
		if( !ex.check())
		{
			//throw InvalidArgumentException( std::string( "Invalid list element given to IECore." ) + typeName<V>() + " constructor" );
			// TODO.
		}

		(*r)[i] = ex();
	}

	return r;
}

template<typename T, size_t N>
struct vector_indexer
{
	static T get( const Vector<T,N>& x, int i)
	{
		if ( i >= 0 && i < N )
			return x[i];
		else
			throw std::out_of_range("");
	}

	static void set( Vector<T,N>& x, int i, const T& v)
	{
		if ( i >= 0 && i < N )
			x[i] = v;
		else
			throw std::out_of_range("");
	}
};

template<typename T, size_t N>
void do_bind_vector( const char *class_name)
{
    bpy::class_<Vector<T,N> >( class_name)
        .def( bpy::init<>())
        .def( bpy::init<T>())

        .def( "__init__", bpy::make_constructor( &construct_from_list<T,N>))
		.def("__getitem__", &vector_indexer<T,N>::get)
		.def("__setitem__", &vector_indexer<T,N>::set)

		// operators
		.def(bpy::self += bpy::self)
		.def(bpy::self + bpy::self)
		.def(bpy::self -= bpy::self)
		.def(bpy::self - bpy::self)

		.def(bpy::self *= T())
		.def(bpy::self * T())
		.def(T() * bpy::self)

		.def(bpy::self /= T())
		.def(bpy::self / bpy::self)
		.def(bpy::self / T())

		.def(-bpy::self)

		.def(bpy::self == bpy::self)
		.def(bpy::self != bpy::self)
        ;
}

} // unnamed

void bind_vector()
{
    do_bind_vector<int,2>( "Vector2i");
    do_bind_vector<float,2>( "Vector2f");
    do_bind_vector<double,2>( "Vector2d");

    do_bind_vector<int,3>( "Vector3i");
    do_bind_vector<float,3>( "Vector3f");
    do_bind_vector<double,3>( "Vector3d");

    do_bind_vector<int,4>( "Vector4i");
}
