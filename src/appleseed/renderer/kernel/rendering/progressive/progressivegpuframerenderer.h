
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_PROGRESSIVEGPUFRAMERENDERER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_PROGRESSIVEGPUFRAMERENDERER_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class IGPULightingEngineFactory; }
namespace renderer      { class IRendererController; }
namespace renderer      { class ITileCallbackFactory; }
namespace renderer      { class Project; }

namespace renderer
{

//
// Progressive GPU frame renderer factory.
//

class ProgressiveGPUFrameRendererFactory
  : public IFrameRendererFactory
{
  public:
    // Constructor.
    ProgressiveGPUFrameRendererFactory(
        const Project&              project,
        IRendererController*        renderer_controller,
        IGPULightingEngineFactory*  lighting_engine_factory,
        ITileCallbackFactory*       callback_factory,       // may be 0
        const ParamArray&           params);

    // Delete this instance.
    void release() override;

    // Return a new progressive frame renderer instance.
    IFrameRenderer* create() override;

    // Return a new progressive frame renderer instance.
    static IFrameRenderer* create(
        const Project&              project,
        IRendererController*        renderer_controller,
        IGPULightingEngineFactory*  lighting_engine_factory,
        ITileCallbackFactory*       callback_factory,       // may be 0
        const ParamArray&           params);

    // Return the metadata of the progressive frame renderer parameters.
    static foundation::Dictionary get_params_metadata();

  private:
    const Project&                  m_project;
    IRendererController*            m_renderer_controller;
    IGPULightingEngineFactory*      m_lighting_engine_factory;
    ITileCallbackFactory*           m_callback_factory;     // may be 0
    ParamArray                      m_params;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_PROGRESSIVEGPUFRAMERENDERER_H
