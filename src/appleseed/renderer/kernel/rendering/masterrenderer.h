
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/rendering/baserenderer.h"
#include "renderer/kernel/rendering/irenderercontroller.h"
#include "renderer/utility/paramarray.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class IFrameRenderer; }
namespace renderer      { class ITileCallback; }
namespace renderer      { class ITileCallbackFactory; }
namespace renderer      { class Project; }
namespace renderer      { class RendererComponents; }

namespace renderer
{

//
// Master renderer, handles rendering of a given project.
//

class APPLESEED_DLLSYMBOL MasterRenderer
  : public BaseRenderer
{
  public:
    // Constructor.
    MasterRenderer(
        Project&                    project,
        const ParamArray&           params,
        IRendererController*        renderer_controller,
        ITileCallbackFactory*       tile_callback_factory = nullptr);

    // Constructor for serial tile callbacks.
    MasterRenderer(
        Project&                    project,
        const ParamArray&           params,
        IRendererController*        renderer_controller,
        ITileCallback*              tile_callback);

    // Destructor.
    ~MasterRenderer();

    struct RenderingResult
    {
        enum Status { Succeeded, Aborted, Failed };

        Status  m_status;
        double  m_render_time;
    };

    // Render the project.
    RenderingResult render();

  private:
    struct Impl;
    Impl* impl;

    // Render a frame until completed or aborted and handle reinitialization events.
    RenderingResult::Status do_render();

    // Initialize rendering components and render a frame.
    IRendererController::Status initialize_and_render_frame();

    // Render a frame until completed or aborted and handle restart events.
    IRendererController::Status render_frame(
        RendererComponents&         components,
        foundation::IAbortSwitch&   abort_switch);

    // Wait until the the frame is completed or rendering is aborted.
    IRendererController::Status wait_for_event(
        IFrameRenderer&             frame_renderer) const;

    // Return true if the scene passes basic integrity checks.
    bool check_scene() const;

    // Bind all scene entities inputs. Return true on success, false otherwise.
    bool bind_scene_entities_inputs() const;

    void add_render_stamp(const double render_time);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_MASTERRENDERER_H
