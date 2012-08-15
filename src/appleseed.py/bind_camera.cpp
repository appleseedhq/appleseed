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

#include "renderer/api/camera.h"

#include "dict2dict.hpp"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{

auto_release_ptr<Camera> create_camera( const std::string& camera_type, const std::string& name, const bpy::dict& params)
{
    CameraFactoryRegistrar factories;
    const ICameraFactory *f = factories.lookup( camera_type.c_str());

    if( !f)
    {
        std::string error = "Camera type ";
        error += camera_type;
        error += " not found";
        PyErr_SetString( PyExc_RuntimeError, error.c_str() );
        bpy::throw_error_already_set();
    }

    return f->create( name.c_str(), bpy_dict_to_param_array( params));
}

TransformSequence *camera_get_transform_sequence( Camera *cam)
{
    return &( cam->transform_sequence());
}

} // unnamed

void bind_camera()
{
    bpy::class_<Camera, auto_release_ptr<Camera>, bpy::bases<Entity>, boost::noncopyable>( "Camera", bpy::no_init)
        .def( "__init__", bpy::make_constructor( create_camera))
        .def( "get_model", &Camera::get_model)
        .def( "transform_sequence", camera_get_transform_sequence, bpy::return_value_policy<bpy::reference_existing_object>())
        .def( "get_film_dimensions", &Camera::get_film_dimensions, bpy::return_value_policy<bpy::copy_const_reference>())
        .def( "get_focal_length", &Camera::get_focal_length)
        .def( "get_shutter_open_time", &Camera::get_shutter_open_time)
        .def( "get_shutter_close_time", &Camera::get_shutter_close_time)
        .def( "get_shutter_middle_time", &Camera::get_shutter_middle_time)
        ;
}
