
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_GENERICFRAMERENDERER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_GENERICFRAMERENDERER_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class Frame; }
namespace renderer      { class IPassCallback; }
namespace renderer      { class ITileCallbackFactory; }
namespace renderer      { class ITileRendererFactory; }

namespace renderer
{

//
// Generic frame renderer factory.
//

class GenericFrameRendererFactory
  : public IFrameRendererFactory
{
  public:
    // Constructor.
    GenericFrameRendererFactory(
        const Frame&            frame,
        ITileRendererFactory*   tile_renderer_factory,
        ITileCallbackFactory*   tile_callback_factory,      // may be 0
        IPassCallback*          pass_callback,              // may be 0
        const ParamArray&       params);

    // Delete this instance.
    virtual void release() APPLESEED_OVERRIDE;

    // Return a new generic frame renderer instance.
    virtual IFrameRenderer* create() APPLESEED_OVERRIDE;

    // Return a new generic frame renderer instance.
    static IFrameRenderer* create(
        const Frame&            frame,
        ITileRendererFactory*   tile_renderer_factory,
        ITileCallbackFactory*   tile_callback_factory,      // may be 0
        IPassCallback*          pass_callback,              // may be 0
        const ParamArray&       params);

    // Get the metadata dictionary describing the generic frame renderer params.
    static foundation::Dictionary get_params_metadata();

  private:
    const Frame&                m_frame;
    ITileRendererFactory*       m_tile_renderer_factory;
    ITileCallbackFactory*       m_tile_callback_factory;    // may be 0
    IPassCallback*              m_pass_callback;            // may be 0
    const ParamArray            m_params;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_GENERICFRAMERENDERER_H
