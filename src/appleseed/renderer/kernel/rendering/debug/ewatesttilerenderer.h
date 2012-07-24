
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_DEBUG_EWATESTTILERENDERER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_DEBUG_EWATESTTILERENDERER_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Forward declarations.
namespace renderer  { class Scene; }
namespace renderer  { class TextureStore; }
namespace renderer  { class TraceContext; }

namespace renderer
{

class EWATestTileRendererFactory
  : public ITileRendererFactory
{
  public:
    // Constructor.
    EWATestTileRendererFactory(
        const Scene&        scene,
        const TraceContext& trace_context,
        TextureStore&       texture_store,
        const ParamArray&   params);

    // Delete this instance.
    virtual void release() override;

    // Return a new blank tile renderer instance.
    virtual ITileRenderer* create() override;

  private:
    const Scene&            m_scene;
    const TraceContext&     m_trace_context;
    TextureStore&           m_texture_store;
    ParamArray              m_params;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_DEBUG_EWATESTTILERENDERER_H
