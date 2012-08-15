
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

#include "dict2dict.hpp"

#include "foundation/math/vector.h"
#include "foundation/utility/string.h"
#include "foundation/utility/iostreamop.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

ParamArray& push_dict( ParamArray& dict, const char *name)
{
    return dict.push( name);
}

Dictionary& push_dict( Dictionary& dict, const char *name)
{
    if( !dict.dictionaries().exist( name))
        dict.dictionaries().insert( name, Dictionary());

    return dict.dictionaries().get( name);
}

template<class Dict>
Dict convert_from_bpy_dict( const bpy::dict& d)
{
    Dict result;

    bpy::list values = d.values();
    bpy::list keys = d.keys();

    for( int i = 0, e = bpy::len( d); i < e; ++i)
    {
        bpy::object key( keys[i] );
        bpy::object value( values[i] );

        // keys
        bpy::extract<const char*> key_extractor( key );
        if( !key_extractor.check() )
        {
            PyErr_SetString( PyExc_TypeError, "Incompatible key type. Only strings accepted." );
            bpy::throw_error_already_set();
        }

        // string
        {
            bpy::extract<const char*> extractor( value );
            if( extractor.check())
            {
                result.insert( key_extractor(), extractor());
                continue;
            }
        }

        // int
        {
            bpy::extract<int> extractor( value );
            if( extractor.check())
            {
                result.insert( key_extractor(), extractor());
                continue;
            }
        }

        // double
        {
            bpy::extract<double> extractor( value );
            if( extractor.check())
            {
                result.insert( key_extractor(), extractor());
                continue;
            }
        }

        // Vector2i
        {
            bpy::extract<Vector2i> extractor( value );
            if( extractor.check())
            {
                result.insert( key_extractor(), extractor());
                continue;
            }
        }

        // Vector2f
        {
            bpy::extract<Vector2f> extractor( value );
            if( extractor.check())
            {
                result.insert( key_extractor(), extractor());
                continue;
            }
        }

        // Vector2d
        {
            bpy::extract<Vector2d> extractor( value );
            if( extractor.check())
            {
                result.insert( key_extractor(), extractor());
                continue;
            }
        }

        // TODO: add conversions from bpy::tuple to Vector<T,N>.
        // ...

        // TODO: check more types here if needed... (est.)

        // dict
        {
            bpy::extract<bpy::dict> extractor( value );
            if( extractor.check())
            {
                // recurse
                push_dict( result, key_extractor()) = convert_from_bpy_dict<Dict>( extractor());
                continue;
            }
        }

        PyErr_SetString( PyExc_TypeError, "Incompatible value type - must be string, int, double or dictionary." );
        bpy::throw_error_already_set();
    }

    return result;
}

Dictionary bpy_dict_to_dict( const bpy::dict& d)
{
    return convert_from_bpy_dict<Dictionary>( d);
}

ParamArray bpy_dict_to_param_array( const bpy::dict& d)
{
    return convert_from_bpy_dict<ParamArray>( d);
}

bpy::object obj_from_string( const std::string& str)
{
    // guess the type of the value represented by str.
    try
    {
        // bool
        bool b = from_string<bool>( str);
        return bpy::object( b);
    }
    catch( ExceptionStringConversionError&) {}

    try
    {
        // int / double
        double d = from_string<double>( str);
        return bpy::object( d);
    }
    catch( ExceptionStringConversionError&) {}

    try
    {
        // Vector2i
        Vector2i v = from_string<Vector2i>( str);
        return bpy::object( v);
    }
    catch( ExceptionStringConversionError&) {}

    try
    {
        // Vector2f
        Vector2f v = from_string<Vector2f>( str);
        return bpy::object( v);
    }
    catch( ExceptionStringConversionError&) {}

    try
    {
        // Vector2d
        Vector2d v = from_string<Vector2d>( str);
        return bpy::object( v);
    }
    catch( ExceptionStringConversionError&) {}

    // TODO: add conversions from bpy::tuple to Vector<T,N>.

    // TODO: check more types here if needed... (est.)

    // as a fallback, return a string
    return bpy::object( str);
}

bpy::dict dictionary_to_bpy_dict( const Dictionary& dict)
{
    bpy::dict result;

    for( StringDictionary::const_iterator it( dict.strings().begin()), e( dict.strings().end()); it != e; ++it)
        result[it.name()] = obj_from_string( it.value());

    for( DictionaryDictionary::const_iterator it( dict.dictionaries().begin()), e( dict.dictionaries().end()); it != e; ++it)
    {
        // recurse
        result[it.name()] = dictionary_to_bpy_dict( it.value());
    }

    return result;
}

// Keep this for a bit, in case ParamArray needs special handling.
bpy::dict param_array_to_bpy_dict( const ParamArray& array)
{
    return dictionary_to_bpy_dict( array);
}
