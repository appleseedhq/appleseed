
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_PROGRESSIVEFRAMERENDERER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_PROGRESSIVEFRAMERENDERER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/rendering/iframerenderer.h"

// Forward declarations.
namespace renderer  { class Frame; }
namespace renderer  { class ISampleGeneratorFactory; }
namespace renderer  { class ITileCallbackFactory; }

namespace renderer
{

//
// Progressive frame renderer factory.
//

class RENDERERDLL ProgressiveFrameRendererFactory
  : public IFrameRendererFactory
{
  public:
    // Constructor.
    ProgressiveFrameRendererFactory(
        Frame&                      frame,
        ISampleGeneratorFactory*    generator_factory,
        ITileCallbackFactory*       callback_factory,       // may be 0
        const ParamArray&           params);

    // Delete this instance.
    virtual void release();

    // Return a new progressive frame renderer instance.
    virtual IFrameRenderer* create();

    // Return a new progressive frame renderer instance.
    static IFrameRenderer* create(
        Frame&                      frame,
        ISampleGeneratorFactory*    generator_factory,
        ITileCallbackFactory*       callback_factory,       // may be 0
        const ParamArray&           params);

  private:
    Frame&                          m_frame;
    ISampleGeneratorFactory*        m_generator_factory;
    ITileCallbackFactory*           m_callback_factory;     // may be 0
    ParamArray                      m_params;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_PROGRESSIVEFRAMERENDERER_H
