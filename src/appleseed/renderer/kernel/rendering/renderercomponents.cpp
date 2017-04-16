
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

// Interface header.
#include "renderercomponents.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/lighting/drt/drtlightingengine.h"
#include "renderer/kernel/lighting/lighttracing/lighttracingsamplegenerator.h"
#include "renderer/kernel/lighting/pt/ptlightingengine.h"
#include "renderer/kernel/lighting/sppm/sppmlightingengine.h"
#include "renderer/kernel/lighting/sppm/sppmparameters.h"
#include "renderer/kernel/lighting/sppm/sppmpasscallback.h"
#include "renderer/kernel/rendering/debug/blanksamplerenderer.h"
#include "renderer/kernel/rendering/debug/blanktilerenderer.h"
#include "renderer/kernel/rendering/debug/debugsamplerenderer.h"
#include "renderer/kernel/rendering/debug/debugtilerenderer.h"
#include "renderer/kernel/rendering/ephemeralshadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/final/adaptivepixelrenderer.h"
#include "renderer/kernel/rendering/final/uniformpixelrenderer.h"
#include "renderer/kernel/rendering/generic/genericframerenderer.h"
#include "renderer/kernel/rendering/generic/genericsamplegenerator.h"
#include "renderer/kernel/rendering/generic/genericsamplerenderer.h"
#include "renderer/kernel/rendering/generic/generictilerenderer.h"
#include "renderer/kernel/rendering/permanentshadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/progressive/progressiveframerenderer.h"
#include "renderer/modeling/project/project.h"
#include "renderer/utility/paramarray.h"

// Standard headers.
#include <string>

using namespace std;

namespace renderer
{

//
// RendererComponents class implementation.
//

namespace
{
    void copy_param(
        ParamArray&         dest,
        const ParamArray&   source,
        const char*         param_name)
    {
        if (source.strings().exist(param_name))
            dest.strings().insert(param_name, source.strings().get(param_name));
    }

    ParamArray get_child_and_inherit_globals(
        const ParamArray&   source,
        const char*         name)
    {
        ParamArray child = source.child(name);
        copy_param(child, source, "sampling_mode");
        copy_param(child, source, "rendering_threads");
        return child;
    }
}

RendererComponents::RendererComponents(
    const Project&          project,
    const ParamArray&       params,
    ITileCallbackFactory*   tile_callback_factory,
    TextureStore&           texture_store,
    OIIO::TextureSystem&    texture_system,
    OSL::ShadingSystem&     shading_system)
  : m_project(project)
  , m_params(params)
  , m_tile_callback_factory(tile_callback_factory)
  , m_scene(*project.get_scene())
  , m_frame(*project.get_frame())
  , m_trace_context(project.get_trace_context())
  , m_light_sampler(m_scene, get_child_and_inherit_globals(params, "light_sampler"))
  , m_shading_engine(get_child_and_inherit_globals(params, "shading_engine"))
  , m_texture_store(texture_store)
  , m_texture_system(texture_system)
  , m_shading_system(shading_system)
{
}

bool RendererComponents::create()
{
    if (!create_lighting_engine_factory())
        return false;

    if (!create_sample_renderer_factory())
        return false;

    if (!create_sample_generator_factory())
        return false;

    if (!create_pixel_renderer_factory())
        return false;

    if (!create_shading_result_framebuffer_factory())
        return false;

    if (!create_tile_renderer_factory())
        return false;

    if (!create_frame_renderer_factory())
        return false;

    return true;
}

bool RendererComponents::create_lighting_engine_factory()
{
    const string name = m_params.get_required<string>("lighting_engine", "pt");

    if (name.empty())
    {
        return true;
    }
    else if (name == "drt")
    {
        m_lighting_engine_factory.reset(
            new DRTLightingEngineFactory(
                m_light_sampler,
                get_child_and_inherit_globals(m_params, "drt")));   // todo: change to "drt_lighting_engine"?
        return true;
    }
    else if (name == "pt")
    {
        m_lighting_engine_factory.reset(
            new PTLightingEngineFactory(
                m_light_sampler,
                get_child_and_inherit_globals(m_params, "pt")));    // todo: change to "pt_lighting_engine"?
        return true;
    }
    else if (name == "sppm")
    {
        const SPPMParameters sppm_params(
            get_child_and_inherit_globals(m_params, "sppm"));

        SPPMPassCallback* sppm_pass_callback =
            new SPPMPassCallback(
                m_scene,
                m_light_sampler,
                m_trace_context,
                m_texture_store,
                m_texture_system,
                m_shading_system,
                sppm_params);

        m_pass_callback.reset(sppm_pass_callback);

        m_lighting_engine_factory.reset(
            new SPPMLightingEngineFactory(
                *sppm_pass_callback,
                m_light_sampler,
                sppm_params));

        return true;
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"lighting_engine\" parameter: \"%s\".",
            name.c_str());
        return false;
    }
}

bool RendererComponents::create_sample_renderer_factory()
{
    const string name = m_params.get_required<string>("sample_renderer", "generic");

    if (name.empty())
    {
        return true;
    }
    else if (name == "generic")
    {
        m_sample_renderer_factory.reset(
            new GenericSampleRendererFactory(
                m_scene,
                m_frame,
                m_trace_context,
                m_texture_store,
                m_lighting_engine_factory.get(),
                m_shading_engine,
                m_texture_system,
                m_shading_system,
                get_child_and_inherit_globals(m_params, "generic_sample_renderer")));
        return true;
    }
    else if (name == "blank")
    {
        m_sample_renderer_factory.reset(new BlankSampleRendererFactory());
        return true;
    }
    else if (name == "debug")
    {
        m_sample_renderer_factory.reset(new DebugSampleRendererFactory());
        return true;
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"sample_renderer\" parameter: \"%s\".",
            name.c_str());
        return false;
    }
}

bool RendererComponents::create_sample_generator_factory()
{
    const string name = m_params.get_optional<string>("sample_generator", "");

    if (name.empty())
    {
        return true;
    }
    else if (name == "generic")
    {
        if (m_sample_renderer_factory.get() == 0)
        {
            RENDERER_LOG_ERROR("cannot use the generic sample generator without a sample renderer.");
            return false;
        }

        m_sample_generator_factory.reset(
            new GenericSampleGeneratorFactory(
                m_frame,
                m_sample_renderer_factory.get(),
                get_child_and_inherit_globals(m_params, "generic_sample_generator")));
        return true;
    }
    else if (name == "lighttracing")
    {
        m_sample_generator_factory.reset(
            new LightTracingSampleGeneratorFactory(
                m_project,
                m_frame,
                m_trace_context,
                m_texture_store,
                m_light_sampler,
                m_texture_system,
                m_shading_system,
                get_child_and_inherit_globals(m_params, "lighttracing_sample_generator")));
        return true;
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"sample_generator\" parameter: \"%s\".",
            name.c_str());
        return false;
    }
}

bool RendererComponents::create_pixel_renderer_factory()
{
    const string name = m_params.get_optional<string>("pixel_renderer", "");

    if (name.empty())
    {
        return true;
    }
    else if (name == "uniform")
    {
        if (m_sample_renderer_factory.get() == 0)
        {
            RENDERER_LOG_ERROR("cannot use the uniform pixel renderer without a sample renderer.");
            return false;
        }

        ParamArray params = get_child_and_inherit_globals(m_params, "uniform_pixel_renderer");
        copy_param(params, m_params, "passes");
        m_pixel_renderer_factory.reset(
            new UniformPixelRendererFactory(
                m_sample_renderer_factory.get(),
                params));
        return true;
    }
    else if (name == "adaptive")
    {
        if (m_sample_renderer_factory.get() == 0)
        {
            RENDERER_LOG_ERROR("cannot use the adaptive pixel renderer without a sample renderer.");
            return false;
        }

        m_pixel_renderer_factory.reset(
            new AdaptivePixelRendererFactory(
                m_frame,
                m_sample_renderer_factory.get(),
                get_child_and_inherit_globals(m_params, "adaptive_pixel_renderer")));
        return true;
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"pixel_renderer\" parameter: \"%s\".",
            name.c_str());
        return false;
    }
}

bool RendererComponents::create_shading_result_framebuffer_factory()
{
    const string name = m_params.get_optional<string>("shading_result_framebuffer", "ephemeral");

    if (name.empty())
    {
        return true;
    }
    else if (name == "ephemeral")
    {
        m_shading_result_framebuffer_factory.reset(
            new EphemeralShadingResultFrameBufferFactory());
        return true;
    }
    else if (name == "permanent")
    {
        m_shading_result_framebuffer_factory.reset(
            new PermanentShadingResultFrameBufferFactory(m_frame));
        return true;
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"shading_result_framebuffer\" parameter: \"%s\".",
            name.c_str());
        return false;
    }
}

bool RendererComponents::create_tile_renderer_factory()
{
    const string name = m_params.get_optional<string>("tile_renderer", "");

    if (name.empty())
    {
        return true;
    }
    else if (name == "generic")
    {
        if (m_pixel_renderer_factory.get() == 0)
        {
            RENDERER_LOG_ERROR("cannot use the generic tile renderer without a pixel renderer.");
            return false;
        }

        if (m_shading_result_framebuffer_factory.get() == 0)
        {
            RENDERER_LOG_ERROR("cannot use the generic tile renderer without a shading result framebuffer.");
            return false;
        }

        m_tile_renderer_factory.reset(
            new GenericTileRendererFactory(
                m_frame,
                m_pixel_renderer_factory.get(),
                m_shading_result_framebuffer_factory.get(),
                get_child_and_inherit_globals(m_params, "generic_tile_renderer")));
        return true;
    }
    else if (name == "blank")
    {
        m_tile_renderer_factory.reset(new BlankTileRendererFactory());
        return true;
    }
    else if (name == "debug")
    {
        m_tile_renderer_factory.reset(new DebugTileRendererFactory());
        return true;
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"tile_renderer\" parameter: \"%s\".",
            name.c_str());
        return false;
    }
}

bool RendererComponents::create_frame_renderer_factory()
{
    const string name = m_params.get_required<string>("frame_renderer", "generic");

    if (name.empty())
    {
        return true;
    }
    else if (name == "generic")
    {
        if (m_tile_renderer_factory.get() == 0)
        {
            RENDERER_LOG_ERROR("cannot use the generic frame renderer without a tile renderer.");
            return false;
        }

        m_frame_renderer.reset(
            GenericFrameRendererFactory::create(
                m_frame,
                m_tile_renderer_factory.get(),
                m_tile_callback_factory,
                m_pass_callback.get(),
                get_child_and_inherit_globals(m_params, "generic_frame_renderer")));
        return true;
    }
    else if (name == "progressive")
    {
        if (m_sample_generator_factory.get() == 0)
        {
            RENDERER_LOG_ERROR("cannot use the progressive frame renderer without a sample generator.");
            return false;
        }

        m_frame_renderer.reset(
            ProgressiveFrameRendererFactory::create(
                m_project,
                m_sample_generator_factory.get(),
                m_tile_callback_factory,
                get_child_and_inherit_globals(m_params, "progressive_frame_renderer")));
        return true;
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"frame_renderer\" parameter: \"%s\".",
            name.c_str());
        return false;
    }
}

}   // namespace renderer
