
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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
#include "renderer/kernel/lighting/backwardlightsampler.h"
#include "renderer/kernel/lighting/forwardlightsampler.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/ipasscallback.h"
#include "renderer/kernel/rendering/ipixelrenderer.h"
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/ishadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/kernel/shading/shadingengine.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"

// Standard headers.
#include <memory>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Frame; }
namespace renderer      { class IFrameRenderer; }
namespace renderer      { class ITileCallbackFactory; }
namespace renderer      { class OIIOTextureSystem; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class OnRenderBeginRecorder; }
namespace renderer      { class OSLShadingSystem; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class Scene; }
namespace renderer      { class TextureStore; }
namespace renderer      { class TraceContext; }

namespace renderer
{

//
// The RendererComponents creates, connects together and owns the various components
// of the rendering pipeline.
//

class RendererComponents
{
  public:
    // Constructor.
    RendererComponents(
        const Project&              project,
        const ParamArray&           params,
        ITileCallbackFactory*       tile_callback_factory,
        TextureStore&               texture_store,
        OIIOTextureSystem&          texture_system,
        OSLShadingSystem&           shading_system);

    // Create all components as specified by the parameters passed at construction.
    bool create();

    // Ask every component to print its settings.
    void print_settings() const;

    // Please refer to the documentation of Entity::on_render_begin().
    bool on_render_begin(
        OnRenderBeginRecorder&      recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr);

    // Please refer to the documentation of Entity::on_frame_begin().
    bool on_frame_begin(
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr);

    // Retrieve individual components.
    const TraceContext& get_trace_context() const;
    BackwardLightSampler* get_backward_light_sampler() const;
    ShadingEngine& get_shading_engine();
    TextureStore& get_texture_store() const;
    OIIOTextureSystem& get_oiio_texture_system();
    OSLShadingSystem& get_osl_shading_system();
    IShadingResultFrameBufferFactory& get_shading_result_framebuffer_factory() const;
    IFrameRenderer& get_frame_renderer() const;

  private:
    const Project&                                      m_project;
    const ParamArray&                                   m_params;
    ITileCallbackFactory*                               m_tile_callback_factory;
    const Scene&                                        m_scene;
    const Frame&                                        m_frame;
    const TraceContext&                                 m_trace_context;
    std::unique_ptr<ForwardLightSampler>                m_forward_light_sampler;
    std::unique_ptr<BackwardLightSampler>               m_backward_light_sampler;
    ShadingEngine                                       m_shading_engine;
    TextureStore&                                       m_texture_store;
    OIIOTextureSystem&                                  m_oiio_texture_system;
    OSLShadingSystem&                                   m_osl_shading_system;

    std::unique_ptr<IShadingResultFrameBufferFactory>   m_shading_result_framebuffer_factory;
    std::unique_ptr<ILightingEngineFactory>             m_lighting_engine_factory;
    std::unique_ptr<ISampleRendererFactory>             m_sample_renderer_factory;
    std::unique_ptr<ISampleGeneratorFactory>            m_sample_generator_factory;
    std::unique_ptr<IPixelRendererFactory>              m_pixel_renderer_factory;
    std::unique_ptr<ITileRendererFactory>               m_tile_renderer_factory;
    std::unique_ptr<IPassCallback>                      m_pass_callback;
    foundation::auto_release_ptr<IFrameRenderer>        m_frame_renderer;

    bool create_lighting_engine_factory();
    bool create_sample_renderer_factory();
    bool create_sample_generator_factory();
    bool create_pixel_renderer_factory();
    bool create_shading_result_framebuffer_factory();
    bool create_tile_renderer_factory();
    bool create_frame_renderer();
};


//
// RendererComponents class implementation.
//

inline const TraceContext& RendererComponents::get_trace_context() const
{
    return m_trace_context;
}

inline BackwardLightSampler* RendererComponents::get_backward_light_sampler() const
{
    return m_backward_light_sampler.get();
}

inline ShadingEngine& RendererComponents::get_shading_engine()
{
    return m_shading_engine;
}

inline TextureStore& RendererComponents::get_texture_store() const
{
    return m_texture_store;
}

inline OIIOTextureSystem& RendererComponents::get_oiio_texture_system()
{
    return m_oiio_texture_system;
}

inline OSLShadingSystem& RendererComponents::get_osl_shading_system()
{
    return m_osl_shading_system;
}

inline IFrameRenderer& RendererComponents::get_frame_renderer() const
{
    return *m_frame_renderer.get();
}

inline IShadingResultFrameBufferFactory& RendererComponents::get_shading_result_framebuffer_factory() const
{
    return *m_shading_result_framebuffer_factory.get();
}

}       // namespace renderer
