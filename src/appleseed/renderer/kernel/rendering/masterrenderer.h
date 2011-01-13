
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_MASTERRENDERER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_MASTERRENDERER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/rendering/irenderercontroller.h"

// Forward declarations.
namespace renderer      { class IFrameRenderer; }
namespace renderer      { class ITileCallbackFactory; }
namespace renderer      { class Project; }

namespace renderer
{

//
// Master renderer, handles rendering of a given project.
//

class RENDERERDLL MasterRenderer
  : public foundation::NonCopyable
{
  public:
    // Rendering modes.
    enum Mode
    {
        RenderOnce = 0,
        RenderContinuously
    };

    // Constructor.
    MasterRenderer(
        Project&                project,
        const ParamArray&       params,
        const Mode              mode,
        IRendererController*    renderer_controller,
        ITileCallbackFactory*   tile_callback_factory = 0);

    // Return the parameters of the master renderer.
    ParamArray& get_parameters();
    const ParamArray& get_parameters() const;

    // Render the project.
    void render();

  private:
    Project&                    m_project;
    ParamArray                  m_params;
    const Mode                  m_mode;
    IRendererController*        m_renderer_controller;
    ITileCallbackFactory*       m_tile_callback_factory;

    // Continuously render from scratch.
    void do_render();

    // Initialize the rendering components and render until completed or aborted.
    IRendererController::Status render_from_scratch();

    // Bind all scene entities inputs. Return true on success, false otherwise.
    bool bind_inputs() const;

    // Render (one or many frames) until rendering is completed or aborted.
    IRendererController::Status render_until_completed_or_aborted(IFrameRenderer* frame_renderer);

    // Wait until the frame being rendered is complete, or rendering was aborted.
    IRendererController::Status wait_until_frame_complete(IFrameRenderer* frame_renderer);

    // Perform pre-frame rendering actions.
    void on_frame_begin() const;

    // Perform post-frame rendering actions.
    void on_frame_end() const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_MASTERRENDERER_H
