
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

// Interface header.
#include "renderdevicebase.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"

// Standard headers.

using namespace foundation;

namespace renderer
{

RenderDeviceBase::RenderDeviceBase(
    Project&                project,
    const ParamArray&       params,
    IRendererController*    renderer_controller)
  : m_project(project)
  , m_params(params)
  , m_renderer_controller(renderer_controller)
{
}

RenderDeviceBase::~RenderDeviceBase()
{
}

IRendererController::Status RenderDeviceBase::wait_for_event(
    IFrameRenderer&         frame_renderer) const
{
    bool is_paused = false;

    while (true)
    {
        if (!frame_renderer.is_rendering())
            return IRendererController::TerminateRendering;

        const IRendererController::Status status = m_renderer_controller->get_status();

        switch (status)
        {
          case IRendererController::ContinueRendering:
            if (is_paused)
            {
                frame_renderer.resume_rendering();
                is_paused = false;
            }
            break;

          case IRendererController::PauseRendering:
            if (!is_paused)
            {
                frame_renderer.pause_rendering();
                is_paused = true;
            }
            break;

          default:
            return status;
        }

        m_renderer_controller->on_progress();

        foundation::sleep(1);   // namespace qualifer required
    }
}

void RenderDeviceBase::copy_param(
    ParamArray&             dest,
    const ParamArray&       source,
    const char*             param_name)
{
    if (source.strings().exist(param_name))
        dest.strings().insert(param_name, source.strings().get(param_name));
}

ParamArray RenderDeviceBase::get_child_and_inherit_globals(
    const ParamArray&       source,
    const char*             name)
{
    ParamArray child = source.child(name);
    copy_param(child, source, "passes");
    copy_param(child, source, "spectrum_mode");
    copy_param(child, source, "sampling_mode");
    copy_param(child, source, "rendering_threads");
    return child;
}

}
