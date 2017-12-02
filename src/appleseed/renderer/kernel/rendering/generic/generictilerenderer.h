
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_GENERICTILERENDERER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_GENERICTILERENDERER_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class Frame; }
namespace renderer  { class IPixelRendererFactory; }
namespace renderer  { class IShadingResultFrameBufferFactory; }

namespace renderer
{

//
// Generic tile renderer factory.
//

class GenericTileRendererFactory
  : public ITileRendererFactory
{
  public:
    // Constructor.
    GenericTileRendererFactory(
        const Frame&                        frame,
        IPixelRendererFactory*              pixel_renderer_factory,
        IShadingResultFrameBufferFactory*   framebuffer_factory,
        const ParamArray&                   params);

    // Delete this instance.
    void release() override;

    // Return a new generic tile renderer instance.
    ITileRenderer* create(
        const size_t                        thread_index) override;

  private:
    const Frame&                            m_frame;
    IPixelRendererFactory*                  m_pixel_renderer_factory;
    IShadingResultFrameBufferFactory*       m_framebuffer_factory;
    const ParamArray                        m_params;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_GENERICTILERENDERER_H
