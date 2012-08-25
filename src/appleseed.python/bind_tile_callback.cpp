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

#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/api/frame.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace detail
{

class ITileCallbackWrapper : public ITileCallback, public bpy::wrapper<ITileCallback>
{
public:

    virtual void release() {delete this;}

    virtual void pre_render(const size_t x, const size_t y, const size_t width, const size_t height)
    {
        this->get_override("pre_render")(x, y, width, height);
    }

    virtual void post_render_tile(const Frame* frame, const size_t tile_x, const size_t tile_y)
    {
        this->get_override("post_render_tile")(frame, tile_x, tile_y);
    }

    virtual void post_render(const Frame* frame)
    {
        this->get_override("post_render")(frame);
    }
};

} // detail

void bind_tile_callback()
{
    bpy::class_<detail::ITileCallbackWrapper, boost::noncopyable>("ITileCallback")
        .def("pre_render", bpy::pure_virtual(&ITileCallback::pre_render))
        .def("post_render_tile", bpy::pure_virtual(&ITileCallback::post_render_tile))
        .def("post_render", bpy::pure_virtual(&ITileCallback::post_render))
        ;
}
