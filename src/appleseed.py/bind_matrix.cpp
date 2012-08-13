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

template<typename T, std::size_t M, std::size_t N>
Matrix<T,M,N> *construct_from_list( bpy::list l)
{
	if( bpy::len( l) != M * N)
	{
        PyErr_SetString( PyExc_RuntimeError, "Invalid list length given to appleseed.Matrix.__init__" );
        bpy::throw_error_already_set();
	}

	Matrix<T,M,N> *r = new Matrix<T,M,N>();

	for( unsigned i = 0; i < M * N; ++i)
	{
		bpy::extract<T> ex( l[i]);
		if( !ex.check())
		{
            PyErr_SetString( PyExc_TypeError, "Incompatible type. Only floats." );
            bpy::throw_error_already_set();
		}

		(*r)[i] = ex();
	}

	return r;
}

template<typename T, std::size_t M, std::size_t N>
struct matrix_indexer
{
	static T get( const Matrix<T,M,N>& mat, bpy::tuple indices)
	{
        if( bpy::len( indices) != 2)
        {
            PyErr_SetString( PyExc_RuntimeError, "Invalid tuple length given to appleseed.Matrix.__get_item__" );
            bpy::throw_error_already_set();
        }

        int i, j;

		bpy::extract<int> ex0( indices[0]);
		if( !ex0.check())
		{
            PyErr_SetString( PyExc_TypeError, "Incompatible index type. Only ints." );
            bpy::throw_error_already_set();
		}
		else
            i = ex0();

		bpy::extract<int> ex1( indices[1]);
		if( !ex1.check())
		{
            PyErr_SetString( PyExc_TypeError, "Incompatible index type. Only ints." );
            bpy::throw_error_already_set();
		}
		else
            j = ex1();

        if( i < 0)
            i = M - i;

        if( j < 0)
            j = N - j;

        if( i >= 0 && i < M && j >= 0 && j < N)
            return mat( i, j);

        PyErr_SetString( PyExc_IndexError, "Out of bounds access in appleseed.Matrix.__get_item__" );
        boost::python::throw_error_already_set();
    }

	static void set( Matrix<T,M,N>& mat, bpy::tuple indices, const T& v)
	{
        if( bpy::len( indices) != 2)
        {
            PyErr_SetString( PyExc_RuntimeError, "Invalid tuple length given to appleseed.Matrix.__set_item__" );
            bpy::throw_error_already_set();
        }

        int i, j;

		bpy::extract<int> ex0( indices[0]);
		if( !ex0.check())
		{
            PyErr_SetString( PyExc_TypeError, "Incompatible index type. Only ints." );
            bpy::throw_error_already_set();
		}
		else
            i = ex0();

		bpy::extract<int> ex1( indices[1]);
		if( !ex1.check())
		{
            PyErr_SetString( PyExc_TypeError, "Incompatible index type. Only ints." );
            bpy::throw_error_already_set();
		}
		else
            j = ex1();

        if( i >= 0 && i < M && j >= 0 && j < N)
            mat( i, j) = v;

        if( i < 0)
            i = M - i;

        if( j < 0)
            j = N - j;

        PyErr_SetString( PyExc_IndexError, "Out of bounds access in appleseed.Matrix.__set_item__" );
        boost::python::throw_error_already_set();
	}
};

template<class T, std::size_t N>
Matrix<T,N,N> invert_matrix( const Matrix<T,N,N>& mat)
{
    try
    {
        return inverse( mat);
    } catch( ExceptionSingularMatrix& e)
    {
        PyErr_SetString( PyExc_RuntimeError, "Singular matrix in appleseed.Matrix.inverse" );
        boost::python::throw_error_already_set();
    }
}

template<class T>
bpy::tuple matrix_extract_euler_angles( const Matrix<T,4,4>& mat)
{
    T yaw, pitch, roll;
    mat.extract_euler_angles( yaw, pitch, roll);
    return bpy::make_tuple( yaw, pitch, roll);
}

// gcc 4.5 does not compile this if name is a const char *
// probably a bug.
template<class T>
void bind_typed_matrix4( const std::string& class_name)
{

    Matrix<T,4,4> (*rot1)( T, T, T) = &Matrix<T,4,4>::rotation;
    Matrix<T,4,4> (*rot2)( const Vector<T,3>&, T) = &Matrix<T,4,4>::rotation;
    Matrix<T,4,4> (*rot3)( const Quaternion<T>&) = &Matrix<T,4,4>::rotation;

    bpy::class_<Matrix<T,4,4> >( class_name.c_str())
        // statics
        .def( "identity", &Matrix<T,4,4>::identity).staticmethod( "identity")
        .def( "translation", &Matrix<T,4,4>::translation).staticmethod( "translation")
        .def( "scaling", &Matrix<T,4,4>::scaling).staticmethod( "scaling")
        .def( "rotation_x", &Matrix<T,4,4>::rotation_x).staticmethod( "rotation_x")
        .def( "rotation_y", &Matrix<T,4,4>::rotation_y).staticmethod( "rotation_y")
        .def( "rotation_z", &Matrix<T,4,4>::rotation_z).staticmethod( "rotation_z")
        .def( "lookat", &Matrix<T,4,4>::lookat).staticmethod( "lookat")

        .def( "rotation", rot1).def( "rotation", rot2).def( "rotation", rot3).staticmethod( "rotation")

        .def( bpy::init<T>())
        .def( "__init__", bpy::make_constructor( &construct_from_list<T,4,4>))

        // operator[]
		.def( "__getitem__", &matrix_indexer<T,4,4>::get)
		.def( "__setitem__", &matrix_indexer<T,4,4>::set)

        .def( "transpose", &transpose<T,4,4>)
        .def( "inverse", &invert_matrix<T,4>)

		.def( bpy::self * bpy::self)
		.def( bpy::self * Vector<T,4>())

		.def( "extract_translation", &Matrix<T,4,4>::extract_translation)
        .def( "extract_unit_quaternion", &Matrix<T,4,4>::extract_unit_quaternion)
        .def( "extract_euler_angles", &matrix_extract_euler_angles<T>)
        ;
}

} // unnamed

void bind_matrix()
{
    bind_typed_matrix4<float>( "Matrix4f");
    bind_typed_matrix4<double>( "Matrix4d");
}
