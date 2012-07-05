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
#include <Python.h>

#include <boost/python.hpp>

#include "foundation/utility/autoreleaseptr.h"

// register autorelease_ptr
namespace boost
{
namespace python
{

template <class T> struct pointee< foundation::auto_release_ptr<T> >
{
    typedef T type;
};

} // python
} // boost

template<typename T>
struct auto_release_ptr_to_python
{
	// Constructor registers the conversion with boost::python.
	auto_release_ptr_to_python()
	{
    	boost::python::to_python_converter<typename foundation::auto_release_ptr<T>, auto_release_ptr_to_python<T> >();
	}

	static PyObject *convert( const foundation::auto_release_ptr<T> &x )
	{
        using namespace boost::python::objects;

        if (!x)
        {
            Py_INCREF( Py_None );
            return Py_None;
        }

        PyObject *converted = class_value_wrapper<
                typename foundation::auto_release_ptr<T>, make_ptr_instance<
                    T,
                    pointer_holder<foundation::auto_release_ptr<T>, T>
                    >
                >::convert(x);

        assert(converted);
        return converted;
	}
};

template<typename T>
struct auto_release_ptr_from_python
{
	// Constructor registers the conversion with boost::python.
	auto_release_ptr_from_python()
	{
    	boost::python::converter::registry::push_back( &convertible, &construct,
                                                        boost::python::type_id<foundation::auto_release_ptr<T> >() );
	}

	static void *convertible( PyObject *p )
	{
        if( p == Py_None )
        {
            return p;
        }

        return boost::python::converter::get_lvalue_from_python( p, boost::python::converter::registered<T>::converters );
	}

	static void construct( PyObject *source, boost::python::converter::rvalue_from_python_stage1_data *data )
	{
        void *storage = ((boost::python::converter::rvalue_from_python_storage<foundation::auto_release_ptr<T> >*)data)->storage.bytes;

        if( data->convertible == source )
        {
            // Py_None case
            new (storage) typename foundation::auto_release_ptr<T>();
        }
        else
        {
            new (storage) typename foundation::auto_release_ptr<T>( static_cast<T*>( data->convertible ) );
        }
        data->convertible = storage;
	}
};

// main python module
BOOST_PYTHON_MODULE( _appleseed)
{
}
