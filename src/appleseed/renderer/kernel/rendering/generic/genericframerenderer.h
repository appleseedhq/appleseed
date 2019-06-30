
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class Frame; }
namespace renderer      { class IPassCallback; }
namespace renderer      { class IShadingResultFrameBufferFactory; }
namespace renderer      { class ITileCallbackFactory; }
namespace renderer      { class ITileRendererFactory; }
namespace renderer      { class IRendererController; }

namespace renderer
{

//
// Generic frame renderer factory.
//

class GenericFrameRendererFactory
  : public IFrameRendererFactory
{
  public:
    // Return parameters metadata.
    static foundation::Dictionary get_params_metadata();

    // Constructor.
    GenericFrameRendererFactory(
        const Frame&                        frame,
        IShadingResultFrameBufferFactory*   framebuffer_factory,
        ITileRendererFactory*               tile_renderer_factory,
        ITileCallbackFactory*               tile_callback_factory,      // may be nullptr
        IPassCallback*                      pass_callback,              // may be nullptr
        const ParamArray&                   params);

    // Delete this instance.
    void release() override;

    // Return a new generic frame renderer instance.
    IFrameRenderer* create() override;

    // Return a new generic frame renderer instance.
    static IFrameRenderer* create(
        const Frame&                        frame,
        IShadingResultFrameBufferFactory*   framebuffer_factory,
        ITileRendererFactory*               tile_renderer_factory,
        ITileCallbackFactory*               tile_callback_factory,      // may be nullptr
        IPassCallback*                      pass_callback,              // may be nullptr
        const ParamArray&                   params);

  private:
    const Frame&                            m_frame;
    IShadingResultFrameBufferFactory*       m_framebuffer_factory;
    ITileRendererFactory*                   m_tile_renderer_factory;
    ITileCallbackFactory*                   m_tile_callback_factory;    // may be nullptr
    IPassCallback*                          m_pass_callback;            // may be nullptr
    const ParamArray                        m_params;
};

}   // namespace renderer
