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

#include "renderer/kernel/rendering/irenderercontroller.h"

#include "py_utility.hpp"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{

class IRendererControllerWrapper : public IRendererController, public bpy::wrapper<IRendererController>
{
public:

    virtual void on_rendering_begin()
    {
        this->get_override( "on_rendering_begin")();
    }

    virtual void on_rendering_success()
    {
        this->get_override( "on_rendering_success")();
    }

    virtual void on_rendering_abort()
    {
        this->get_override( "on_rendering_abort")();
    }

    virtual void on_frame_begin()
    {
        this->get_override( "on_frame_begin")();
    }

    virtual void on_frame_end()
    {
        this->get_override( "on_frame_end")();
    }

    virtual Status on_progress()
    {
        return this->get_override( "on_progress")();
    }
};

} // unnamed

void bind_renderer_controller()
{
    bpy::enum_<IRendererController::Status>( "IRenderControllerStatus")
        .value( "ContinueRendering", IRendererController::ContinueRendering)
        .value( "TerminateRendering", IRendererController::TerminateRendering)
        .value( "AbortRendering", IRendererController::AbortRendering)
        .value( "RestartRendering", IRendererController::RestartRendering)
        .value( "ReinitializeRendering", IRendererController::ReinitializeRendering)
        ;

    bpy::class_<IRendererControllerWrapper, boost::noncopyable>( "IRendererController")
        .def( "on_rendering_begin", bpy::pure_virtual( &IRendererController::on_rendering_begin))
        .def( "on_rendering_success", bpy::pure_virtual( &IRendererController::on_rendering_success))
        .def( "on_rendering_abort", bpy::pure_virtual( &IRendererController::on_rendering_abort))
        .def( "on_frame_begin", bpy::pure_virtual( &IRendererController::on_frame_begin))
        .def( "on_frame_end", bpy::pure_virtual( &IRendererController::on_frame_end))
        ;
}
