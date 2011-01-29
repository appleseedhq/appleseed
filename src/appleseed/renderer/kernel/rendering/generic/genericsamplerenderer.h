
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_GENERICSAMPLERENDERER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_GENERICSAMPLERENDERER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/rendering/isamplerenderer.h"

// Forward declarations.
namespace renderer  { class ILightingEngineFactory; }
namespace renderer  { class Scene; }
namespace renderer  { class ShadingEngine; }
namespace renderer  { class TraceContext; }

namespace renderer
{

//
// Generic sample renderer factory.
//

class RENDERERDLL GenericSampleRendererFactory
  : public ISampleRendererFactory
{
  public:
    // Constructor.
    GenericSampleRendererFactory(
        const Scene&            scene,
        const TraceContext&     trace_context,
        ILightingEngineFactory* lighting_engine_factory,
        ShadingEngine&          shading_engine,
        const ParamArray&       params);

    // Delete this instance.
    virtual void release();

    // Return a new generic sample renderer instance.
    virtual ISampleRenderer* create();

    // Return a new generic sample renderer instance.
    static ISampleRenderer* create(
        const Scene&            scene,
        const TraceContext&     trace_context,
        ILightingEngineFactory* lighting_engine_factory,
        ShadingEngine&          shading_engine,
        const ParamArray&       params);

  private:
    const Scene&                m_scene;
    const TraceContext&         m_trace_context;
    ILightingEngineFactory*     m_lighting_engine_factory;
    ShadingEngine&              m_shading_engine;
    const ParamArray            m_params;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_GENERICSAMPLERENDERER_H
