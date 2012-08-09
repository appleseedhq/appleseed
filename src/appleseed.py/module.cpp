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

// Prototypes
void bind_assembly();
void bind_bsdf();
void bind_camera();
void bind_entity();
void bind_frame();
void bind_master_renderer();
void bind_matrix();
void bind_project();
void bind_renderer_controller();
void bind_scene();
void bind_transform();
void bind_utility();
void bind_vector();

// appleseed python module
BOOST_PYTHON_MODULE( _appleseed)
{
    bind_entity();

    bind_assembly();
    bind_bsdf();
    bind_camera();
    bind_frame();
    bind_master_renderer();
	bind_matrix();
	bind_project();
	bind_renderer_controller();
    bind_scene();
    bind_transform();
	bind_utility();
    bind_vector();
}
