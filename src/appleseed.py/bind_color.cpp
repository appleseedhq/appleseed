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

#include "bind_auto_release_ptr.h"

#include "foundation/image/colorspace.h"
#include "renderer/api/color.h"

#include "dict2dict.hpp"
#include "bind_typed_entity_containers.hpp"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{

ColorValueArray color_value_array_from_bpy_list( const bpy::list& vals)
{
    ColorValueArray result;

    unsigned int size = bpy::len( vals);
    result.reserve( size);

    for( int i = 0; i < size; ++i)
    {
		bpy::extract<float> ex( vals[i]);
		if( ex.check())
            result.push_back( ex());
		else
		{
            PyErr_SetString( PyExc_TypeError, "Incompatible type type. Only floats." );
            bpy::throw_error_already_set();
		}
    }

    return result;
}

bpy::list bpy_list_from_color_value_array( const ColorValueArray& vals)
{
    bpy::list result;

    for( int i = 0, e = vals.size(); i < e; ++i)
        result.append( vals[i]);

    return result;
}

auto_release_ptr<ColorEntity> create_color_entity( const std::string& name, const bpy::dict& params)
{
    return ColorEntityFactory::create( name.c_str(), bpy_dict_to_param_array( params));
}

auto_release_ptr<ColorEntity> create_color_entity_vals( const std::string& name,
                                                        const bpy::dict& params,
                                                        const bpy::list& values)
{
    return ColorEntityFactory::create( name.c_str(), bpy_dict_to_param_array( params),
                                        color_value_array_from_bpy_list( values));
}

auto_release_ptr<ColorEntity> create_color_entity_vals_alpha( const std::string& name,
                                                                const bpy::dict& params,
                                                                const bpy::list& values,
                                                                const bpy::list& alpha)
{
    return ColorEntityFactory::create( name.c_str(), bpy_dict_to_param_array( params),
                                        color_value_array_from_bpy_list( values),
                                        color_value_array_from_bpy_list( alpha));
}

bpy::list color_entity_get_vals( const ColorEntity *col)
{
    return bpy_list_from_color_value_array( col->get_values());
}

bpy::list color_entity_get_alpha( const ColorEntity *col)
{
    return bpy_list_from_color_value_array( col->get_alpha());
}

} // unnamed

void bind_color()
{
    bpy::enum_<ColorSpace>( "ColorSpace")
        .value( "ColorSpaceLinearRGB"   , ColorSpaceLinearRGB)
        .value( "ColorSpaceSRGB"        , ColorSpaceSRGB)
        .value( "ColorSpaceCIEXYZ"      , ColorSpaceCIEXYZ)
        .value( "ColorSpaceSpectral"    , ColorSpaceSpectral)
        ;

    bpy::class_<ColorEntity, auto_release_ptr<ColorEntity>, bpy::bases<Entity>, boost::noncopyable >( "ColorEntity", bpy::no_init)
        .def( "__init__", bpy::make_constructor( create_color_entity))
        .def( "__init__", bpy::make_constructor( create_color_entity_vals))
        .def( "__init__", bpy::make_constructor( create_color_entity_vals_alpha))
        .def( "get_values", color_entity_get_vals)
        .def( "get_alpha", color_entity_get_alpha)
        .def( "get_color_space", &ColorEntity::get_color_space)
        .def( "get_wavelength_range", &ColorEntity::get_wavelength_range, bpy::return_value_policy<bpy::copy_const_reference>())
        .def( "get_multiplier", &ColorEntity::get_multiplier)
        ;

    bind_typed_entity_vector<ColorEntity>( "ColorContainer");
}
