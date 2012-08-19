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

#include "tile_callback_factory.hpp"

#include <boost/thread/locks.hpp>

#include "renderer/modeling/frame/frame.h"

#include <iostream>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{

struct scoped_gil_lock
{
    scoped_gil_lock()
    {
        state_ = PyGILState_Ensure();
    }

    ~scoped_gil_lock()
    {
        PyGILState_Release( state_);
    }

private:

    PyGILState_STATE state_;
};

}

PyTileCallback::PyTileCallback(boost::python::object callback, boost::mutex *mutex)
{
    callback_ = callback;
    mutex_ = mutex;
}

PyTileCallback::~PyTileCallback()
{
    std::cout << "PyTileCallback deleted" << std::endl;
}

void PyTileCallback::release() { delete this;}

void PyTileCallback::pre_render( const size_t x, const size_t y, const size_t width, const size_t height)
{
    boost::lock_guard<boost::mutex> lock( *mutex_);
    scoped_gil_lock gil_lock();

    std::cout << "PyTileCallback::pre_render" << std::endl;
    std::cout << "Ref count = " << callback_.ptr()->ob_refcnt << std::endl;
    callback_.attr( "pre_render")( x, y, width, height);
}

void PyTileCallback::post_render( const Frame& frame, const size_t tile_x, const size_t tile_y)
{
    boost::lock_guard<boost::mutex> lock( *mutex_);
    scoped_gil_lock gil_lock();

    //std::cout << "PyTileCallback::post_render_tile" << std::endl;
    callback_.attr( "post_render_tile")( bpy::ptr( &frame), tile_x, tile_y);
}

void PyTileCallback::post_render( const Frame& frame)
{
    boost::lock_guard<boost::mutex> lock( *mutex_);
    scoped_gil_lock gil_lock();

    //std::cout << "PyTileCallback::post_render" << std::endl;
    callback_.attr( "post_render")( bpy::ptr( &frame));
}

PyTileCallbackFactory::PyTileCallbackFactory( bpy::object callback)
{
    callback_ = callback;
}

void PyTileCallbackFactory::release() { delete this;}

ITileCallback *PyTileCallbackFactory::create()
{
    boost::lock_guard<boost::mutex> lock( mutex_);
    scoped_gil_lock gil_lock();

    std::cout << "PyTileCallbackFactory::create" << std::endl;
    ITileCallback *result = new PyTileCallback( callback_, &mutex_);
    std::cout << "Ref count = " << callback_.ptr()->ob_refcnt << std::endl;
    return result;
}

void bind_tile_callback()
{
    bpy::class_<ITileCallbackFactory, boost::noncopyable>( "ITileCallbackFactory", bpy::no_init)
        ;

    bpy::class_<PyTileCallbackFactory, bpy::bases<ITileCallbackFactory>, boost::noncopyable>( "TileCallbackFactory", bpy::no_init)
        .def( bpy::init<const bpy::object&>())
        ;
}
