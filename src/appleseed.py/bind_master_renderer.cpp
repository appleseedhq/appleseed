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

#include <memory>

#include <boost/python.hpp>

#include "renderer/api/rendering.h"
#include "renderer/api/project.h"

#include "dict2dict.hpp"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace detail
{

std::auto_ptr<MasterRenderer> create_master_renderer( Project* project,
                                                        const bpy::dict& params,
                                                        IRendererController* render_controller)
{
    return std::auto_ptr<MasterRenderer>( new MasterRenderer( *project, bpy_dict_to_param_array( params), render_controller));
}

std::auto_ptr<MasterRenderer> create_master_renderer_with_tile_callback( Project* project,
                                                                         const bpy::dict& params,
                                                                         IRendererController* render_controller,
                                                                         ITileCallbackFactory *tile_callback)
{
    return std::auto_ptr<MasterRenderer>( new MasterRenderer( *project,
                                                                bpy_dict_to_param_array( params),
                                                                render_controller,
                                                                tile_callback));
}

bpy::dict master_renderer_get_parameters( const MasterRenderer *m)
{
    return param_array_to_bpy_dict( m->get_parameters());
}

void master_renderer_set_parameters( MasterRenderer *m, const bpy::dict& params)
{
    m->get_parameters() = bpy_dict_to_param_array( params);
}

} // detail

void bind_master_renderer()
{
    bpy::class_<MasterRenderer, std::auto_ptr<MasterRenderer>, boost::noncopyable>( "MasterRenderer", bpy::no_init)
        .def( "__init__", bpy::make_constructor( detail::create_master_renderer))
        .def( "__init__", bpy::make_constructor( detail::create_master_renderer_with_tile_callback))
        .def( "get_parameters", detail::master_renderer_get_parameters)
        .def( "set_parameters", detail::master_renderer_set_parameters)
        .def( "render", &MasterRenderer::render)
        ;
}
