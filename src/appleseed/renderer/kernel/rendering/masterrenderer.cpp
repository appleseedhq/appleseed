
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "masterrenderer.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/drt/drtlightingengine.h"
#include "renderer/kernel/lighting/pt/ptlightingengine.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/rendering/debug/blanksamplerenderer.h"
#include "renderer/kernel/rendering/debug/blanktilerenderer.h"
#include "renderer/kernel/rendering/debug/debugsamplerenderer.h"
#include "renderer/kernel/rendering/debug/debugtilerenderer.h"
#include "renderer/kernel/rendering/debug/ewatesttilerenderer.h"
#include "renderer/kernel/rendering/final/adaptivepixelrenderer.h"
#include "renderer/kernel/rendering/final/uniformpixelrenderer.h"
#include "renderer/kernel/rendering/generic/genericframerenderer.h"
#include "renderer/kernel/rendering/generic/genericsamplegenerator.h"
#include "renderer/kernel/rendering/generic/genericsamplerenderer.h"
#include "renderer/kernel/rendering/generic/generictilerenderer.h"
#include "renderer/kernel/rendering/lighttracing/lighttracingsamplegenerator.h"
#include "renderer/kernel/rendering/progressive/progressiveframerenderer.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/ipixelrenderer.h"
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/itilerenderer.h"
#ifdef WITH_OSL
#include "renderer/kernel/rendering/rendererservices.h"
#endif
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingengine.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/stringexception.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/searchpaths.h"

// OIIO headers.
#ifdef WITH_OSL
#include "OpenImageIO/texture.h"
#endif

// boost headers.
#include "boost/shared_ptr.hpp"
#include "boost/bind.hpp"

// Standard headers.
#include <cstring>
#include <deque>
#include <exception>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// A renderer controller that can queue tile callback updates and execute them
// later in the master renderer's thread. Needed by the Python bindings.
//

class SerialRendererController
  : public IRendererController
{
  public:
    SerialRendererController(
        IRendererController*    controller,
        ITileCallback*          tile_callback)
      : m_controller(controller)
      , m_tile_callback(tile_callback)
    {
        assert(m_controller);
        assert(m_tile_callback);
    }

    virtual void on_rendering_begin() OVERRIDE
    {
        m_controller->on_rendering_begin();
    }

    virtual void on_rendering_success() OVERRIDE
    {
        m_controller->on_rendering_success();
    }

    virtual void on_rendering_abort() OVERRIDE
    {
        m_controller->on_rendering_abort();
    }

    virtual void on_frame_begin() OVERRIDE
    {
        m_controller->on_frame_begin();
    }

    virtual void on_frame_end() OVERRIDE
    {
        m_controller->on_frame_end();
    }

    virtual Status on_progress() OVERRIDE
    {
        {
            boost::mutex::scoped_lock lock(m_mutex);

            while (!m_callbacks_todo.empty())
            {
                exec_callback(m_callbacks_todo.front());
                m_callbacks_todo.pop_front();
            }
        }

        return m_controller->on_progress();
    }

    void add_pre_render_tile_callback(const size_t x, const size_t y, const size_t width, const size_t height)
    {
        boost::mutex::scoped_lock lock(m_mutex);

        PendingTileCallback callback;
        callback.m_type = PendingTileCallback::PreRender;
        callback.m_frame = 0;
        callback.m_x = x;
        callback.m_y = y;
        callback.m_width = width;
        callback.m_height = height;
        m_callbacks_todo.push_back(callback);
    }

    void add_post_render_tile_callback(const Frame* frame, const size_t tile_x, const size_t tile_y)
    {
        boost::mutex::scoped_lock lock(m_mutex);

        PendingTileCallback callback;
        callback.m_type = PendingTileCallback::PostRenderTile;
        callback.m_frame = frame;
        callback.m_x = tile_x;
        callback.m_y = tile_y;
        callback.m_width = 0;
        callback.m_height = 0;
        m_callbacks_todo.push_back(callback);
    }

    void add_post_render_tile_callback(const Frame* frame)
    {
        boost::mutex::scoped_lock lock(m_mutex);

        PendingTileCallback callback;
        callback.m_type = PendingTileCallback::PostRender;
        callback.m_frame = frame;
        callback.m_x = 0;
        callback.m_y = 0;
        callback.m_width = 0;
        callback.m_height = 0;
        m_callbacks_todo.push_back(callback);
    }

  private:
    struct PendingTileCallback
    {
        enum CallbackType
        {
            PreRender,
            PostRenderTile,
            PostRender
        };

        CallbackType    m_type;
        const Frame*    m_frame;
        size_t          m_x;
        size_t          m_y;
        size_t          m_width;
        size_t          m_height;
    };

    void exec_callback(const PendingTileCallback& call)
    {
        switch (call.m_type)
        {
            case PendingTileCallback::PreRender:
            m_tile_callback->pre_render(call.m_x, call.m_y, call.m_width, call.m_height);
            break;

            case PendingTileCallback::PostRenderTile:
            m_tile_callback->post_render_tile(call.m_frame, call.m_x, call.m_y);
            break;

            case PendingTileCallback::PostRender:
            m_tile_callback->post_render(call.m_frame);
            break;

            default:
            assert(false);
        }
    }

    IRendererController*        m_controller;
    ITileCallback*              m_tile_callback;
    boost::mutex                m_mutex;
    deque<PendingTileCallback>  m_callbacks_todo;
};


namespace
{
    //
    // A tile callback that simply serializes updates to a SerialRendererController.
    //

    class SerialTileCallback
      : public ITileCallback
    {
      public:
        explicit SerialTileCallback(SerialRendererController* controller)
          : m_controller(controller)
        {
            assert(m_controller);
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual void pre_render(const size_t x, const size_t y, const size_t width, const size_t height) OVERRIDE
        {
            m_controller->add_pre_render_tile_callback(x,y,width,height);
        }

        virtual void post_render_tile(const Frame* frame, const size_t tile_x, const size_t tile_y) OVERRIDE
        {
            m_controller->add_post_render_tile_callback(frame,tile_x,tile_y);
        }

        virtual void post_render(const Frame* frame) OVERRIDE
        {
            m_controller->add_post_render_tile_callback(frame);
        }

      private:
        SerialRendererController* m_controller;
    };

    class SerialTileCallbackFactory
      : public ITileCallbackFactory
    {
      public:
        explicit SerialTileCallbackFactory(SerialRendererController* controller)
          : m_controller(controller)
        {
            assert(m_controller);
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual ITileCallback* create() OVERRIDE
        {
            return new SerialTileCallback(m_controller);
        }

      private:
        SerialRendererController* m_controller;
    };
}


//
// MasterRenderer class implementation.
//

MasterRenderer::MasterRenderer(
    Project&                project,
    const ParamArray&       params,
    IRendererController*    renderer_controller,
    ITileCallbackFactory*   tile_callback_factory)
  : m_project(project)
  , m_params(params)
  , m_renderer_controller(renderer_controller)
  , m_tile_callback_factory(tile_callback_factory)
  , m_serial_renderer_controller(0)
  , m_serial_tile_callback_factory(0)
{
}

MasterRenderer::MasterRenderer(
    Project&                project,
    const ParamArray&       params,
    IRendererController*    renderer_controller,
    ITileCallback*          tile_callback)
  : m_project(project)
  , m_params(params)
  , m_renderer_controller(0)
  , m_tile_callback_factory(0)
  , m_serial_renderer_controller(new SerialRendererController(renderer_controller, tile_callback))
  , m_serial_tile_callback_factory(new SerialTileCallbackFactory(m_serial_renderer_controller))
{
    m_renderer_controller = m_serial_renderer_controller;
    m_tile_callback_factory = m_serial_tile_callback_factory;
}

MasterRenderer::~MasterRenderer()
{
    delete m_serial_tile_callback_factory;
    delete m_serial_renderer_controller;
}

ParamArray& MasterRenderer::get_parameters()
{
    return m_params;
}

const ParamArray& MasterRenderer::get_parameters() const
{
    return m_params;
}

bool MasterRenderer::render() const
{
    try
    {
        do_render();
        return true;
    }
    catch (const StringException& e)
    {
        m_renderer_controller->on_rendering_abort();
        if (strlen(e.string()) > 0)
            RENDERER_LOG_ERROR("rendering failed (%s: %s).", e.what(), e.string());
        else RENDERER_LOG_ERROR("rendering failed (%s).", e.what());
        return false;
    }
    catch (const bad_alloc&)
    {
        m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed (ran out of memory).");
        return false;
    }
#ifdef NDEBUG
    catch (const exception& e)
    {
        m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed (%s).", e.what());
        return false;
    }
    catch (...)
    {
        m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed (unknown exception).");
        return false;
    }
#endif
}

void MasterRenderer::do_render() const
{
    while (true)
    {
        m_renderer_controller->on_rendering_begin();

        const IRendererController::Status status = initialize_and_render_frame_sequence();

        switch (status)
        {
          case IRendererController::TerminateRendering:
            m_renderer_controller->on_rendering_success();
            return;

          case IRendererController::AbortRendering:
            m_renderer_controller->on_rendering_abort();
            return;

          case IRendererController::ReinitializeRendering:
            break;

          assert_otherwise;
        }
    }
}

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
}

IRendererController::Status MasterRenderer::initialize_and_render_frame_sequence() const
{
    assert(m_project.get_scene());
    assert(m_project.get_frame());

#ifdef WITH_OSL

    // Create our renderer services.
    RendererServices services;

    // Create the error handler
    // TODO: replace this by an appropiate error handler...
    OIIO::ErrorHandler error_handler;

    // Create the OIIO texture system.
    boost::shared_ptr<OIIO::TextureSystem> texture_system(
        OIIO::TextureSystem::create(false),
        boost::bind(&OIIO::TextureSystem::destroy, _1));

    // Set texture system mem limit.
    {
        const size_t max_size = m_params.get_optional<size_t>("texture_cache_size", 256 * 1024 * 1024);
        texture_system->attribute("max_memory_MB", static_cast<float>(max_size / 1024));
    }

    std::string osl_search_path;
    for (size_t i = 0, e = m_project.get_search_paths().size(); i < e; ++i)
    {
        osl_search_path.append(m_project.get_search_paths()[i]);

        // Do not append a colon after the last path.
        if (i != e - 1)
            osl_search_path.append(";");
    }

    // Setup search paths.
    texture_system->attribute("searchpath", osl_search_path);

    // TODO: set other texture system options here...

    // Create our OSL shading system.
    boost::shared_ptr<OSL::ShadingSystem> shading_system(
        OSL::ShadingSystem::create(
            &services,
            texture_system.get(),
            &error_handler),
        boost::bind(&OSL::ShadingSystem::destroy, _1));
    shading_system->attribute("searchpath:shader", osl_search_path);
    shading_system->attribute("lockgeom", 1);
    shading_system->attribute("colorspace", "Linear");
    shading_system->attribute("commonspace", "world");
    // TODO: set more shading system options here...
    // string[] raytypes      Array of ray type names
    // ...

    register_closures(*shading_system);

#endif

    // We start by binding entities inputs. This must be done before creating/updating the trace context.
    if (!bind_scene_entities_inputs())
        return IRendererController::AbortRendering;

    m_project.create_aov_images();
    m_project.update_trace_context();

    const Scene& scene = *m_project.get_scene();
    Frame& frame = *m_project.get_frame();

    // Create the texture store.
    TextureStore texture_store(scene, m_params.child("texture_store"));

    // Create the light sampler.
    LightSampler light_sampler(scene);

    // Create the shading engine.
    ShadingEngine shading_engine(m_params.child("shading_engine"));

    //
    // Create a lighting engine factory.
    //

    auto_ptr<ILightingEngineFactory> lighting_engine_factory;
    {
        const string lighting_engine_param =
            m_params.get_required<string>("lighting_engine", "pt");

        if (lighting_engine_param == "drt")
        {
            lighting_engine_factory.reset(
                new DRTLightingEngineFactory(
                    light_sampler,
                    m_params.child("drt")));
        }
        else if (lighting_engine_param == "pt")
        {
            lighting_engine_factory.reset(
                new PTLightingEngineFactory(
                    light_sampler,
                    m_params.child("pt")));
        }
        else
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"lighting_engine\" parameter: \"%s\".",
                lighting_engine_param.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a sample renderer factory.
    //

    auto_ptr<ISampleRendererFactory> sample_renderer_factory;
    {
        const string sample_renderer_param =
            m_params.get_required<string>("sample_renderer", "generic");

        if (sample_renderer_param == "generic")
        {
            sample_renderer_factory.reset(
                new GenericSampleRendererFactory(
                    scene,
                    frame,
                    m_project.get_trace_context(),
                    texture_store,
                    lighting_engine_factory.get(),
                    shading_engine,
                    m_params.child("generic_sample_renderer")));
        }
        else if (sample_renderer_param == "blank")
        {
            sample_renderer_factory.reset(new BlankSampleRendererFactory());
        }
        else if (sample_renderer_param == "debug")
        {
            sample_renderer_factory.reset(new DebugSampleRendererFactory());
        }
        else
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"sample_renderer\" parameter: \"%s\".",
                sample_renderer_param.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a pixel renderer factory.
    //

    auto_ptr<IPixelRendererFactory> pixel_renderer_factory;
    {
        const string pixel_renderer_param =
            m_params.get_optional<string>("pixel_renderer", "");

        if (pixel_renderer_param == "uniform")
        {
            pixel_renderer_factory.reset(
                new UniformPixelRendererFactory(
                    sample_renderer_factory.get(),
                    m_params.child("uniform_pixel_renderer")));
        }
        else if (pixel_renderer_param == "adaptive")
        {
            pixel_renderer_factory.reset(
                new AdaptivePixelRendererFactory(
                    frame,
                    sample_renderer_factory.get(),
                    m_params.child("adaptive_pixel_renderer")));
        }
        else if (!pixel_renderer_param.empty())
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"pixel_renderer\" parameter: \"%s\".",
                pixel_renderer_param.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a tile renderer factory.
    //

    auto_ptr<ITileRendererFactory> tile_renderer_factory;
    {
        const string tile_renderer_param =
            m_params.get_optional<string>("tile_renderer", "");

        if (tile_renderer_param == "generic")
        {
            tile_renderer_factory.reset(
                new GenericTileRendererFactory(
                    frame,
                    pixel_renderer_factory.get(),
                    m_params.child("generic_tile_renderer")));
        }
        else if (tile_renderer_param == "blank")
        {
            tile_renderer_factory.reset(new BlankTileRendererFactory());
        }
        else if (tile_renderer_param == "debug")
        {
            tile_renderer_factory.reset(new DebugTileRendererFactory());
        }
        else if (tile_renderer_param == "ewatest")
        {
            tile_renderer_factory.reset(
                new EWATestTileRendererFactory(
                    scene,
                    m_project.get_trace_context(),
                    texture_store,
                    m_params.child("ewatest_tile_renderer")));
        }
        else if (!tile_renderer_param.empty())
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"tile_renderer\" parameter: \"%s\".",
                tile_renderer_param.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a sample generator factory.
    //

    auto_ptr<ISampleGeneratorFactory> sample_generator_factory;
    {
        const string sample_generator_param =
            m_params.get_optional<string>("sample_generator", "");

        if (sample_generator_param == "generic")
        {
            sample_generator_factory.reset(
                new GenericSampleGeneratorFactory(
                    frame,
                    sample_renderer_factory.get()));
        }
        else if (sample_generator_param == "lighttracing")
        {
            sample_generator_factory.reset(
                new LightTracingSampleGeneratorFactory(
                    scene,
                    frame,
                    m_project.get_trace_context(),
                    texture_store,
                    light_sampler,
                    m_params.child("lighttracing_sample_generator")));
        }
        else if (!sample_generator_param.empty())
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"sample_generator\" parameter: \"%s\".",
                sample_generator_param.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a frame renderer.
    //

    auto_release_ptr<IFrameRenderer> frame_renderer;
    {
        const string frame_renderer_param =
            m_params.get_required<string>("frame_renderer", "generic");

        if (frame_renderer_param == "generic")
        {
            ParamArray params = m_params.child("generic_frame_renderer");
            copy_param(params, m_params, "rendering_threads");

            frame_renderer.reset(
                GenericFrameRendererFactory::create(
                    frame,
                    tile_renderer_factory.get(),
                    m_tile_callback_factory,
                    params));
        }
        else if (frame_renderer_param == "progressive")
        {
            ParamArray params = m_params.child("progressive_frame_renderer");
            copy_param(params, m_params, "rendering_threads");

            frame_renderer.reset(
                ProgressiveFrameRendererFactory::create(
                    m_project,
                    sample_generator_factory.get(),
                    m_tile_callback_factory,
                    params));
        }
        else
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"frame_renderer\" parameter: \"%s\".",
                frame_renderer_param.c_str());

            return IRendererController::AbortRendering;
        }
    }

    // Execute the main rendering loop.
    const IRendererController::Status status = render_frame_sequence(frame_renderer.get());

    // Print texture store performance statistics.
    RENDERER_LOG_DEBUG("%s", texture_store.get_statistics().to_string().c_str());

    return status;
}

IRendererController::Status MasterRenderer::render_frame_sequence(IFrameRenderer* frame_renderer) const
{
    while (true) 
    {
        assert(!frame_renderer->is_rendering());

        // The renderer controller might alter the scene (like the transform of the camera).
        // It needs to be called before renderer::Scene::on_frame_begin() which assumes the
        // scene is up-to-date and ready to be rendered.
        m_renderer_controller->on_frame_begin();

        if (!m_project.get_scene()->on_frame_begin(m_project))
        {
            m_renderer_controller->on_frame_end();
            return IRendererController::AbortRendering;
        }

        frame_renderer->start_rendering();

        const IRendererController::Status status = render_frame(frame_renderer);

        switch (status)
        {
          case IRendererController::TerminateRendering:
          case IRendererController::AbortRendering:
          case IRendererController::ReinitializeRendering:
            frame_renderer->terminate_rendering();
            break;

          case IRendererController::RestartRendering:
            frame_renderer->stop_rendering();
            break;

          assert_otherwise;
        }

        assert(!frame_renderer->is_rendering());

        m_project.get_scene()->on_frame_end(m_project);

        m_renderer_controller->on_frame_end();

        if (status != IRendererController::RestartRendering)
            return status;
    }
}

IRendererController::Status MasterRenderer::render_frame(IFrameRenderer* frame_renderer) const
{
    while (frame_renderer->is_rendering())
    {
        const IRendererController::Status status = m_renderer_controller->on_progress();

        if (status != IRendererController::ContinueRendering)
            return status;
    }

    return IRendererController::TerminateRendering;
}

bool MasterRenderer::bind_scene_entities_inputs() const
{
    InputBinder input_binder;
    input_binder.bind(*m_project.get_scene());
    return input_binder.get_error_count() == 0;
}

#ifdef WITH_OSL

void MasterRenderer::register_closures(OSL::ShadingSystem& shading_sys) const
{
    // OSL TODO: implement this...
}

#endif

}   // namespace renderer
