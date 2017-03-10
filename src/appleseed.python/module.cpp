
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.python headers.
#include "pyseed.h" // has to be first, to avoid redefinition warnings

// appleseed.renderer headers.
#include "renderer/api/version.h"

// Forward declarations.
void bind_aov();
void bind_assembly();
void bind_basis();
void bind_bbox();
void bind_bsdf();
void bind_bssrdf();
void bind_camera();
void bind_color();
void bind_display();
void bind_edf();
void bind_entity();
void bind_environment();
void bind_frame();
void bind_image();
void bind_light();
void bind_logger();
void bind_master_renderer();
void bind_material();
void bind_matrix();
void bind_mesh_object();
void bind_object();
void bind_project();
void bind_quaternion();
void bind_renderer_controller();
void bind_scene();
void bind_shader_group();
void bind_surface_shader();
void bind_texture();
void bind_tile_callback();
void bind_transform();
void bind_utility();
void bind_vector();

BOOST_PYTHON_MODULE(_appleseedpython)
{
    boost::python::scope().attr("APPLESEED_VERSION") = APPLESEED_VERSION;
    boost::python::scope().attr("APPLESEED_VERSION_MAJOR") = APPLESEED_VERSION_MAJOR;
    boost::python::scope().attr("APPLESEED_VERSION_MINOR") = APPLESEED_VERSION_MINOR;
    boost::python::scope().attr("APPLESEED_VERSION_PATCH") = APPLESEED_VERSION_PATCH;
    boost::python::scope().attr("APPLESEED_VERSION_MATURITY") = APPLESEED_VERSION_MATURITY;
    boost::python::scope().attr("APPLESEED_VERSION_STRING") = APPLESEED_VERSION_STRING;

    bind_utility();
    bind_logger();

    bind_vector();
    bind_basis();
    bind_quaternion();
    bind_bbox();
    bind_matrix();
    bind_transform();

    bind_entity();

    bind_color();
    bind_texture();
    bind_bsdf();
    bind_bssrdf();
    bind_edf();
    bind_shader_group();

    bind_surface_shader();
    bind_material();
    bind_light();
    bind_object();
    bind_mesh_object();
    bind_assembly();

    bind_camera();
    bind_environment();
    bind_scene();

    bind_image();
    bind_aov();
    bind_frame();
    bind_display();
    bind_project();

    bind_renderer_controller();
    bind_tile_callback();
    bind_master_renderer();
}
