
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_RENDERERCOMPONENTS_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_RENDERERCOMPONENTS_H

// appleseed.renderer headers.
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/lighting/lightsampler.h"
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

// OpenImageIO headers.
#include "foundation/platform/oiioheaderguards.h"
BEGIN_OIIO_INCLUDES
#include "OpenImageIO/texture.h"
END_OIIO_INCLUDES

// OSL headers.
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/oslexec.h"
END_OSL_INCLUDES

// Standard headers.
#include <memory>

// Forward declarations.
namespace renderer  { class Frame; }
namespace renderer  { class IFrameRenderer; }
namespace renderer  { class ITileCallbackFactory; }
namespace renderer  { class ParamArray; }
namespace renderer  { class Project; }
namespace renderer  { class Scene; }
namespace renderer  { class TextureStore; }
namespace renderer  { class TraceContext; }

namespace renderer
{

//
// The RendererComponents creates, connects together and owns the various components
// of the rendering pipeline.
//

class RendererComponents
{
  public:
    RendererComponents(
        const Project&          project,
        const ParamArray&       params,
        ITileCallbackFactory*   tile_callback_factory,
        TextureStore&           texture_store,
        OIIO::TextureSystem&    texture_system,
        OSL::ShadingSystem&     shading_system);

    bool create();

    ShadingEngine& get_shading_engine();

    IFrameRenderer& get_frame_renderer();

  private:
    const Project&              m_project;
    const ParamArray&           m_params;
    ITileCallbackFactory*       m_tile_callback_factory;
    const Scene&                m_scene;
    const Frame&                m_frame;
    const TraceContext&         m_trace_context;
    LightSampler                m_light_sampler;
    ShadingEngine               m_shading_engine;
    TextureStore&               m_texture_store;
    OIIO::TextureSystem&        m_texture_system;
    OSL::ShadingSystem&         m_shading_system;

    std::auto_ptr<ILightingEngineFactory>               m_lighting_engine_factory;
    std::auto_ptr<ISampleRendererFactory>               m_sample_renderer_factory;
    std::auto_ptr<ISampleGeneratorFactory>              m_sample_generator_factory;
    std::auto_ptr<IPixelRendererFactory>                m_pixel_renderer_factory;
    std::auto_ptr<IShadingResultFrameBufferFactory>     m_shading_result_framebuffer_factory;
    std::auto_ptr<ITileRendererFactory>                 m_tile_renderer_factory;
    std::auto_ptr<IPassCallback>                        m_pass_callback;
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

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_RENDERERCOMPONENTS_H
