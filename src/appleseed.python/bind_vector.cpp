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
#include "foundation/utility/iostreamop.h"

namespace bpy = boost::python;
using namespace foundation;

namespace detail
{

template<typename T, std::size_t N>
Vector<T,N>* construct_vec_from_list(bpy::list l)
{
	if (bpy::len(l) != N)
	{
        PyErr_SetString(PyExc_RuntimeError, "Invalid list length given to appleseed.Vector" );
        bpy::throw_error_already_set();
	}

	Vector<T,N>* r = new Vector<T,N>();

	for (unsigned i = 0; i < N; ++i)
	{
		bpy::extract<T> ex(l[i]);
		if (!ex.check())
		{
            PyErr_SetString(PyExc_TypeError, "Incompatible type type. Only floats." );
            bpy::throw_error_already_set();
		}

		(*r)[i] = ex();
	}

	return r;
}

template<class T, std::size_t N>
struct vector_constructor {};

template<class T>
struct vector_constructor<T,2>
{
    static Vector<T,2>* construct(T x, T y)
    {
        Vector<T,2>* r = new Vector<T,2>();
        (*r)[0] = x;
        (*r)[1] = y;
        return r;
    }
};

template<class T>
struct vector_constructor<T,3>
{
    static Vector<T,3>* construct(T x, T y, T z)
    {
        Vector<T,3>* r = new Vector<T,3>();
        (*r)[0] = x;
        (*r)[1] = y;
        (*r)[2] = z;
        return r;
    }
};

template<class T>
struct vector_constructor<T,4>
{
    static Vector<T,4>* construct(T x, T y, T z, T w)
    {
        Vector<T,4>* r = new Vector<T,4>();
        (*r)[0] = x;
        (*r)[1] = y;
        (*r)[2] = z;
        (*r)[3] = w;
        return r;
    }
};

template<typename T, std::size_t N>
struct vector_indexer
{
	static T get(const Vector<T,N>& x, int i)
	{
	    if (i < 0)
            i = N + i;

		if (i >= 0 && i < N)
			return x[i];
		else
		{
            PyErr_SetString(PyExc_IndexError, "Invalid index in appleseed.Vector" );
            boost::python::throw_error_already_set();
		}

		return T();
	}

	static void set(Vector<T,N>& x, int i, const T& v)
	{
	    if (i < 0)
            i = N + i;

		if (i >= 0 && i < N)
			x[i] = v;
		else
		{
            PyErr_SetString(PyExc_IndexError, "Invalid index in appleseed.Vector" );
            boost::python::throw_error_already_set();
        }
	}
};

template<typename T, std::size_t N>
void do_bind_vector(const char* class_name)
{
    bpy::class_<Vector<T,N> >(class_name)
        .def(bpy::init<>())
        .def(bpy::init<T>())
        .def("__init__", bpy::make_constructor(&vector_constructor<T,N>::construct))
        .def("__init__", bpy::make_constructor(&construct_vec_from_list<T,N>))

        // operator[]
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

		// a bug in boost::python, this needs
		// the extra self_ns qualification
		.def(bpy::self_ns::str(bpy::self))
		.def(bpy::self_ns::repr(bpy::self))
        ;
}

} // detail

void bind_vector()
{
    detail::do_bind_vector<int,2>("Vector2i");
    detail::do_bind_vector<float,2>("Vector2f");
    detail::do_bind_vector<double,2>("Vector2d");

    detail::do_bind_vector<int,3>("Vector3i");
    detail::do_bind_vector<float,3>("Vector3f");
    detail::do_bind_vector<double,3>("Vector3d");

    detail::do_bind_vector<int,4>("Vector4i");
}
