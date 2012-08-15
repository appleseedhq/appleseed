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

#include "renderer/api/frame.h"

#include "dict2dict.hpp"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{

auto_release_ptr<Frame> create_frame( const std::string& name, const bpy::dict& params)
{
    return FrameFactory::create( name.c_str(), bpy_dict_to_param_array( params));
}

bpy::object archive( const Frame *f, const char *directory)
{
    char *output = 0;

    if( f->archive( directory, &output))
    {
        bpy::str path( output);
        foundation::free_string( output);
        return path;
    }

    // return None
    return bpy::object();
}

} // unnamed

void bind_frame()
{
    bpy::class_<Frame, auto_release_ptr<Frame>, bpy::bases<Entity>, boost::noncopyable>( "Frame", bpy::no_init)
        .def( "__init__", bpy::make_constructor( create_frame))

        .def( "write", &Frame::write)
        .def( "archive", archive)
        ;
}
