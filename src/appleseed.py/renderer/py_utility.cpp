
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012 Esteban Tovagliari
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

#include "renderer/py_utility.hpp"

namespace bpy = boost::python;

namespace renderer
{

ParamArray bpy_dict_to_param_array( const bpy::dict& d)
{
    ParamArray result;

    bpy::list values = d.values();
    bpy::list keys = d.keys();

    for( int i = 0; i < keys.attr("__len__")(); ++i)
    {
        bpy::object key( keys[i] );
        bpy::object value( values[i] );

        bpy::extract<const char*> key_check( key );
        if( !key_check.check() )
        {
            PyErr_SetString( PyExc_TypeError, "Incompatible key type. Only strings accepted." );
            bpy::throw_error_already_set();
        }

        // string
        {
            bpy::extract<const char*> extractor( value );
            if( extractor.check())
            {
                result.insert( key_check(), extractor());
                continue;
            }
        }

        // int
        {
            bpy::extract<int> extractor( value );
            if( extractor.check())
            {
                result.insert( key_check(), extractor());
                continue;
            }
        }

        // double
        {
            bpy::extract<double> extractor( value );
            if( extractor.check())
            {
                result.insert( key_check(), extractor());
                continue;
            }
        }

        // ...
        // TODO: check more types here... (est.)

        // dict
        {
            bpy::extract<bpy::dict> extractor( value );
            if( extractor.check())
            {
                // recurse
                result.push( key_check()) = bpy_dict_to_param_array( extractor());
                continue;
            }
        }

        PyErr_SetString( PyExc_TypeError, "Incompatible value type - must be XXX." );
        bpy::throw_error_already_set();
    }

    return result;
}

boost::python::dict param_array_to_bpy_dict( const ParamArray& array)
{
    // TODO: implment this.
    return bpy::dict();
}

}       // namespace renderer
