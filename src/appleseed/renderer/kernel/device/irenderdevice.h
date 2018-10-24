
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

#ifndef APPLESEED_RENDERER_KERNEL_DEVICE_IRENDERDEVICE_H
#define APPLESEED_RENDERER_KERNEL_DEVICE_IRENDERDEVICE_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/irenderercontroller.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Forward declarations.
namespace foundation { class IAbortSwitch; }
namespace renderer   { class ITileCallbackFactory; }
namespace renderer   { class OnFrameBeginRecorder; }
namespace renderer   { class Project; }
namespace renderer   { class TextureStore; }

namespace renderer
{

class IRenderDevice
  : public foundation::NonCopyable
{
  public:
    // Destructor.
    virtual ~IRenderDevice()
    {
    }

    // Initialize the render device.
    virtual bool initialize(
        TextureStore&               texture_store,
        foundation::IAbortSwitch&   abort_switch)  = 0;

    // Build or update ray tracing acceleration structures.
    virtual bool build_or_update_bvh() = 0;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) = 0;

    // Render a frame.
    virtual IRendererController::Status render_frame(
        ITileCallbackFactory*       tile_callback_factory,
        TextureStore&               texture_store,
        foundation::IAbortSwitch&   abort_switch) = 0;

    // Print render device settings.
    virtual void print_settings() const = 0;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_DEVICE_IRENDERDEVICE_H
