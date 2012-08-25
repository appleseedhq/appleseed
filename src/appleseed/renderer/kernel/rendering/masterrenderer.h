
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

// appleseed.foundation headers
#include "foundation/utility/autoreleaseptr.h"

// Forward declarations.
namespace renderer      { class IFrameRenderer; }
namespace renderer      { class ITileCallbackFactory; }
namespace renderer      { class ITileCallback; }
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
    // Constructor.
    MasterRenderer(
        Project&                project,
        const ParamArray&       params,
        IRendererController*    renderer_controller,
        ITileCallbackFactory*   tile_callback_factory = 0);

    // Constructor for serial tile callbacks.
    MasterRenderer(
        Project&                project,
        const ParamArray&       params,
        IRendererController*    renderer_controller,
        ITileCallback*          tile_callback);

    ~MasterRenderer();

    // Return the parameters of the master renderer.
    ParamArray& get_parameters();
    const ParamArray& get_parameters() const;

    // Render the project. Return true on success, false otherwise.
    bool render() const;

  private:
    Project&                    m_project;
    ParamArray                  m_params;
    IRendererController*        m_renderer_controller;
    ITileCallbackFactory*       m_tile_callback_factory;

    // storage for serial tile callbacks
    IRendererController*                                m_serial_renderer_controller;
    foundation::auto_release_ptr<ITileCallbackFactory>  m_serial_tile_callback_factory;

    // Render frame sequences, each time reinitializing the rendering components.
    void do_render() const;

    // Initialize the rendering components and render a frame sequence.
    IRendererController::Status initialize_and_render_frame_sequence() const;

    // Render a frame sequence until the sequence is completed or rendering is aborted.
    IRendererController::Status render_frame_sequence(IFrameRenderer* frame_renderer) const;

    // Render a frame until until the frame is completed or rendering is aborted.
    IRendererController::Status render_frame(IFrameRenderer* frame_renderer) const;

    // Bind all scene entities inputs. Return true on success, false otherwise.
    bool bind_scene_entities_inputs() const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_MASTERRENDERER_H
