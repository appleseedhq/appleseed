
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
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/lighting/backwardlightsampler.h"
#include "renderer/kernel/lighting/forwardlightsampler.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/ipasscallback.h"
#include "renderer/kernel/rendering/ipixelrenderer.h"
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/ishadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/kernel/shading/shadingengine.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"

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

    // This method is called before rendering begins, and whenever rendering is reinitialized
    // (i.e. because an entity has been edited). At this point, all entities inputs are bound.
    // Returns true on success, or false if an error occurred or if the abort switch was triggered.
    bool on_render_begin(
        OnRenderBeginRecorder&      recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr);

    // This method is called before rendering a frame begins, and whenever rendering is restarted
    // (i.e. because the camera has been moved). At this point, all entities inputs are bound.
    // Returns true on success, or false if an error occurred or if the abort switch was triggered.
    bool on_frame_begin(
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr);

    // Retrieve individual components.
    ShadingEngine& get_shading_engine();
    IShadingResultFrameBufferFactory& get_shading_result_framebuffer_factory();
    IFrameRenderer& get_frame_renderer();

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
    OIIOTextureSystem&                                  m_texture_system;
    OSLShadingSystem&                                   m_shading_system;

    std::unique_ptr<ILightingEngineFactory>             m_lighting_engine_factory;
    std::unique_ptr<ISampleRendererFactory>             m_sample_renderer_factory;
    std::unique_ptr<ISampleGeneratorFactory>            m_sample_generator_factory;
    std::unique_ptr<IPixelRendererFactory>              m_pixel_renderer_factory;
    std::unique_ptr<IShadingResultFrameBufferFactory>   m_shading_result_framebuffer_factory;
    std::unique_ptr<ITileRendererFactory>               m_tile_renderer_factory;
    std::unique_ptr<IPassCallback>                      m_pass_callback;
    foundation::auto_release_ptr<IFrameRenderer>        m_frame_renderer;

    bool create_lighting_engine_factory();
    bool create_sample_renderer_factory();
    bool create_sample_generator_factory();
    bool create_pixel_renderer_factory();
    bool create_shading_result_framebuffer_factory();
    bool create_tile_renderer_factory();
    bool create_frame_renderer_factory();
};


//
// RendererComponents class implementation.
//

inline ShadingEngine& RendererComponents::get_shading_engine()
{
    return m_shading_engine;
}

inline IFrameRenderer& RendererComponents::get_frame_renderer()
{
    return *m_frame_renderer.get();
}

inline IShadingResultFrameBufferFactory& RendererComponents::get_shading_result_framebuffer_factory()
{
    return *m_shading_result_framebuffer_factory.get();
}

}       // namespace renderer
