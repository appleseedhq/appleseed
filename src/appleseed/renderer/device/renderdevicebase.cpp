
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include <string>

namespace renderer
{

RenderDeviceBase::RenderDeviceBase(
    Project&            project,
    const ParamArray&   params)
  : m_project(project)
  , m_params(params)
{
}

const Project& RenderDeviceBase::get_project() const
{
    return m_project;
}

Project& RenderDeviceBase::get_project()
{
    return m_project;
}

const ParamArray& RenderDeviceBase::get_params() const
{
    return m_params;
}

bool RenderDeviceBase::is_progressive_render() const
{
    return get_params().get<std::string>("frame_renderer") == "progressive";
}

IRendererController* RenderDeviceBase::get_frame_renderer_controller()
{
    return nullptr;
}

IRendererController::Status RenderDeviceBase::wait_for_event(
    IFrameRenderer&         frame_renderer,
    IRendererController&    renderer_controller)
{
    bool is_paused = false;

    while (true)
    {
        if (!frame_renderer.is_rendering())
            return IRendererController::TerminateRendering;

        const IRendererController::Status status = renderer_controller.get_status();

        switch (status)
        {
          case IRendererController::ContinueRendering:
            if (is_paused)
            {
                frame_renderer.resume_rendering();
                renderer_controller.on_rendering_resume();
                is_paused = false;
            }
            break;

          case IRendererController::PauseRendering:
            if (!is_paused)
            {
                renderer_controller.on_rendering_pause();
                frame_renderer.pause_rendering();
                is_paused = true;
            }
            break;

          default:
            return status;
        }

        renderer_controller.on_progress();
        foundation::sleep(1);   // namespace qualifer required
    }
}

}
