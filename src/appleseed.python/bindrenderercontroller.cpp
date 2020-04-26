
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "gillocks.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/defaultrenderercontroller.h"
#include "renderer/kernel/rendering/irenderercontroller.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/python.h"

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    class IRendererControllerWrapper
      : public IRendererController
      , public bpy::wrapper<IRendererController>
    {
      public:
        void on_rendering_begin() override
        {
            // Lock Python's global interpreter lock (GIL),
            // it was released in MasterRenderer.render.
            ScopedGILLock lock;

            try
            {
                get_override("on_rendering_begin")();
            }
            catch (const bpy::error_already_set&)
            {
                PyErr_Print();
            }
        }

        void on_rendering_success() override
        {
            // Lock Python's global interpreter lock (GIL),
            // it was released in MasterRenderer.render.
            ScopedGILLock lock;

            try
            {
                get_override("on_rendering_success")();
            }
            catch (const bpy::error_already_set&)
            {
                PyErr_Print();
            }
        }

        void on_rendering_abort() override
        {
            // Lock Python's global interpreter lock (GIL),
            // it was released in MasterRenderer.render.
            ScopedGILLock lock;

            try
            {
                get_override("on_rendering_abort")();
            }
            catch (const bpy::error_already_set&)
            {
                PyErr_Print();
            }
        }

        void on_rendering_pause() override
        {
            // Lock Python's global interpreter lock (GIL),
            // it was released in MasterRenderer.render.
            ScopedGILLock lock;

            try
            {
                get_override("on_rendering_pause")();
            }
            catch (const bpy::error_already_set&)
            {
                PyErr_Print();
            }
        }

        void on_rendering_resume() override
        {
            // Lock Python's global interpreter lock (GIL),
            // it was released in MasterRenderer.render.
            ScopedGILLock lock;

            try
            {
                get_override("on_rendering_resume")();
            }
            catch (const bpy::error_already_set&)
            {
                PyErr_Print();
            }
        }

        void on_frame_begin() override
        {
            // Lock Python's global interpreter lock (GIL),
            // it was released in MasterRenderer.render.
            ScopedGILLock lock;

            try
            {
                get_override("on_frame_begin")();
            }
            catch (const bpy::error_already_set&)
            {
                PyErr_Print();
            }
        }

        void on_frame_end() override
        {
            // Lock Python's global interpreter lock (GIL),
            // it was released in MasterRenderer.render.
            ScopedGILLock lock;

            try
            {
                get_override("on_frame_end")();
            }
            catch (const bpy::error_already_set&)
            {
                PyErr_Print();
            }
        }

        void on_progress() override
        {
            // Lock Python's global interpreter lock (GIL),
            // it was released in MasterRenderer.render.
            ScopedGILLock lock;

            try
            {
                get_override("on_progress")();
            }
            catch (const bpy::error_already_set&)
            {
                PyErr_Print();
            }
        }

        Status get_status() const override
        {
            // Lock Python's global interpreter lock (GIL),
            // it was released in MasterRenderer.render.
            ScopedGILLock lock;

            try
            {
                return get_override("get_status")();
            }
            catch (const bpy::error_already_set&)
            {
                PyErr_Print();
                return AbortRendering;
            }
        }
    };
}

void bind_renderer_controller()
{
    bpy::enum_<IRendererController::Status>("IRenderControllerStatus")
        .value("ContinueRendering", IRendererController::ContinueRendering)
        .value("PauseRendering", IRendererController::PauseRendering)
        .value("TerminateRendering", IRendererController::TerminateRendering)
        .value("AbortRendering", IRendererController::AbortRendering)
        .value("RestartRendering", IRendererController::RestartRendering)
        .value("ReinitializeRendering", IRendererController::ReinitializeRendering);

    bpy::class_<IRendererControllerWrapper, boost::noncopyable>("IRendererController")
        .def("on_rendering_begin", bpy::pure_virtual(&IRendererController::on_rendering_begin))
        .def("on_rendering_success", bpy::pure_virtual(&IRendererController::on_rendering_success))
        .def("on_rendering_abort", bpy::pure_virtual(&IRendererController::on_rendering_abort))
        .def("on_frame_begin", bpy::pure_virtual(&IRendererController::on_frame_begin))
        .def("on_frame_end", bpy::pure_virtual(&IRendererController::on_frame_end))
        .def("on_progress", bpy::pure_virtual(&IRendererController::on_progress))
        .def("get_status", bpy::pure_virtual(&IRendererController::get_status));

    bpy::class_<DefaultRendererController, boost::noncopyable>("DefaultRendererController");
}
