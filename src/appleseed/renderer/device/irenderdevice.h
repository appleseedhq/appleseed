
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

#pragma once

// appleseed.renderer headers.
#include "renderer/kernel/rendering/irenderercontroller.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation { class IAbortSwitch; }
namespace foundation { class SearchPaths; }
namespace renderer   { class Frame; }
namespace renderer   { class IRenderContext; }
namespace renderer   { class ITileCallbackFactory; }
namespace renderer   { class OnFrameBeginRecorder; }
namespace renderer   { class OnRenderBeginRecorder; }

namespace renderer
{

class IRenderDevice
  : public foundation::NonCopyable
{
  public:
    // Destructor.
    virtual ~IRenderDevice() = default;

    // Initialize the render device.
    virtual bool initialize(
        const foundation::SearchPaths&  resource_search_paths,
        ITileCallbackFactory*           tile_callback_factory,
        foundation::IAbortSwitch&       abort_switch)  = 0;

    // Build or update ray tracing acceleration structures.
    virtual bool build_or_update_scene() = 0;

    // Load checkpoint.
    virtual bool load_checkpoint(Frame& frame, const size_t pass_count) = 0;

    // Return the frame renderer controller.
    virtual IRendererController* get_frame_renderer_controller() = 0;

    // Please refer to the documentation of Entity::on_render_begin().
    virtual bool on_render_begin(
        OnRenderBeginRecorder&          recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr) = 0;

    // Please refer to the documentation of Entity::on_frame_begin().
    virtual bool on_frame_begin(
        OnFrameBeginRecorder&           recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr) = 0;

    // Return the render context for this device.
    virtual const IRenderContext& get_render_context() const = 0;

    // Render a frame.
    virtual IRendererController::Status render_frame(
        ITileCallbackFactory*           tile_callback_factory,
        IRendererController&            renderer_controller,
        foundation::IAbortSwitch&       abort_switch) = 0;

    // Print render device settings.
    virtual void print_settings() const = 0;
};

}       // namespace renderer
