
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
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Forward declarations.
namespace renderer  { class Frame; }
namespace renderer  { class ILightingEngineFactory; }
namespace renderer  { class OIIOTextureSystem; }
namespace renderer  { class OSLShadingSystem; }
namespace renderer  { class Scene; }
namespace renderer  { class ShadingEngine; }
namespace renderer  { class TextureStore; }
namespace renderer  { class TraceContext; }

namespace renderer
{

//
// Generic sample renderer factory.
//

class GenericSampleRendererFactory
  : public ISampleRendererFactory
{
  public:
    // Constructor.
    GenericSampleRendererFactory(
        const Scene&            scene,
        const Frame&            frame,
        const TraceContext&     trace_context,
        TextureStore&           texture_store,
        ILightingEngineFactory* lighting_engine_factory,
        ShadingEngine&          shading_engine,
        OIIOTextureSystem&      oiio_texture_system,
        OSLShadingSystem&       shading_system,
        const ParamArray&       params);

    // Delete this instance.
    void release() override;

    // Return a new sample renderer instance.
    ISampleRenderer* create(
        const size_t            thread_index) override;

  private:
    const Scene&                m_scene;
    const Frame&                m_frame;
    const TraceContext&         m_trace_context;
    TextureStore&               m_texture_store;
    ILightingEngineFactory*     m_lighting_engine_factory;
    ShadingEngine&              m_shading_engine;
    OIIOTextureSystem&          m_oiio_texture_system;
    OSLShadingSystem&           m_shading_system;
    const ParamArray            m_params;
};

}   // namespace renderer
