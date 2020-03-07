
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

// Interface header.
#include "genericframerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/rendering/generic/tilejob.h"
#include "renderer/kernel/rendering/generic/tilejobfactory.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/ipasscallback.h"
#include "renderer/kernel/rendering/ishadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/kernel/rendering/permanentshadingresultframebufferfactory.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/hash/hash.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/platform/thread.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/job.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

// Forward declarations.
namespace renderer { class IRendererController; }

using namespace boost;
using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Generic frame renderer.
    //

    class GenericFrameRenderer
      : public IFrameRenderer
    {
      public:
        GenericFrameRenderer(
            const Frame&                        frame,
            IShadingResultFrameBufferFactory*   framebuffer_factory,
            ITileRendererFactory*               tile_renderer_factory,
            ITileCallbackFactory*               tile_callback_factory,
            IPassCallback*                      pass_callback,
            const ParamArray&                   params)
          : m_frame(frame)
          , m_framebuffer_factory(framebuffer_factory)
          , m_params(params)
          , m_pass_callback(pass_callback)
          , m_is_rendering(false)
        {
            // We must have a renderer factory, but it's OK not to have a callback factory.
            assert(tile_renderer_factory);

            // Create and initialize job manager.
            m_job_manager.reset(
                new JobManager(
                    global_logger(),
                    m_job_queue,
                    m_params.m_thread_count,
                    JobManager::KeepRunningOnEmptyQueue));

            // Instantiate tile renderers, one per rendering thread.
            m_tile_renderers.reserve(m_params.m_thread_count);
            for (size_t i = 0; i < m_params.m_thread_count; ++i)
                m_tile_renderers.push_back(tile_renderer_factory->create(i));

            if (tile_callback_factory)
            {
                // Instantiate tile callbacks, one per rendering thread.
                m_tile_callbacks.reserve(m_params.m_thread_count);
                for (size_t i = 0; i < m_params.m_thread_count; ++i)
                    m_tile_callbacks.push_back(tile_callback_factory->create());
            }
        }

        ~GenericFrameRenderer() override
        {
            // Tell the pass manager thread to stop.
            m_abort_switch.abort();

            // Wait until the pass manager thread is terminated.
            if (m_pass_manager_thread.get() && m_pass_manager_thread->joinable())
                m_pass_manager_thread->join();

            // Delete tile callbacks.
            for (auto tile_callback : m_tile_callbacks)
                tile_callback->release();

            // Delete tile renderers.
            for (auto tile_renderer : m_tile_renderers)
                tile_renderer->release();
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
            RENDERER_LOG_INFO(
                "generic frame renderer settings:\n"
                "  spectrum mode                 %s\n"
                "  sampling mode                 %s\n"
                "  rendering threads             %s\n"
                "  tile ordering                 %s\n"
                "  passes                        %s",
                get_spectrum_mode_name(m_params.m_spectrum_mode).c_str(),
                get_sampling_context_mode_name(m_params.m_sampling_mode).c_str(),
                pretty_uint(m_params.m_thread_count).c_str(),
                m_params.m_tile_ordering == TileJobFactory::TileOrdering::LinearOrdering ? "linear" :
                m_params.m_tile_ordering == TileJobFactory::TileOrdering::SpiralOrdering ? "spiral" :
                m_params.m_tile_ordering == TileJobFactory::TileOrdering::HilbertOrdering ? "hilbert" : "random",
                pretty_uint(m_params.m_pass_count).c_str());

            m_tile_renderers.front()->print_settings();
        }

        IRendererController* get_renderer_controller() override
        {
            return nullptr;
        }

        void render() override
        {
            start_rendering();
            m_job_queue.wait_until_completion();
        }

        bool is_rendering() const override
        {
            return m_is_rendering;
        }

        void start_rendering() override
        {
            assert(!is_rendering());
            assert(!m_job_queue.has_scheduled_or_running_jobs());

            m_abort_switch.clear();

            // Start job execution.
            m_job_manager->start();

            // Create and start the pass manager thread.
            m_is_rendering = true;
            m_pass_manager_func.reset(
                new PassManagerFunc(
                    m_frame,
                    m_framebuffer_factory,
                    m_tile_renderers,
                    m_tile_callbacks,
                    m_pass_callback,
                    m_params.m_spectrum_mode,
                    m_params.m_tile_ordering,
                    m_params.m_pass_count,
                    m_job_queue,
                    m_params.m_thread_count,
                    m_abort_switch,
                    m_is_rendering));
            ThreadFunctionWrapper<PassManagerFunc> wrapper(m_pass_manager_func.get());
            m_pass_manager_thread.reset(new boost::thread(wrapper));
        }

        void stop_rendering() override
        {
            // First, delete scheduled jobs to prevent worker threads from picking them up.
            m_job_queue.clear_scheduled_jobs();

            // Tell rendering jobs and the pass manager thread to stop.
            m_abort_switch.abort();

            // Wait until the pass manager thread has stopped.
            m_pass_manager_thread->join();

            // Stop job execution.
            m_job_manager->stop();
        }

        void pause_rendering() override
        {
            m_job_manager->pause();
        }

        void resume_rendering() override
        {
            m_job_manager->resume();
        }

        void terminate_rendering() override
        {
            stop_rendering();

            print_tile_renderers_stats();
        }

      private:
        struct Parameters
        {
            const Spectrum::Mode                m_spectrum_mode;
            const SamplingContext::Mode         m_sampling_mode;
            const size_t                        m_thread_count;     // number of rendering threads
            const TileJobFactory::TileOrdering  m_tile_ordering;    // tile rendering order
            const size_t                        m_pass_count;       // number of rendering passes

            explicit Parameters(const ParamArray& params)
              : m_spectrum_mode(get_spectrum_mode(params))
              , m_sampling_mode(get_sampling_context_mode(params))
              , m_thread_count(get_rendering_thread_count(params))
              , m_tile_ordering(get_tile_ordering(params))
              , m_pass_count(params.get_optional<size_t>("passes", 1))
            {
            }

            static TileJobFactory::TileOrdering get_tile_ordering(const ParamArray& params)
            {
                const std::string tile_ordering =
                    params.get_optional<std::string>("tile_ordering", "spiral");

                if (tile_ordering == "linear")
                {
                    return TileJobFactory::LinearOrdering;
                }
                else if (tile_ordering == "spiral")
                {
                    return TileJobFactory::SpiralOrdering;
                }
                else if (tile_ordering == "hilbert")
                {
                    return TileJobFactory::HilbertOrdering;
                }
                else if (tile_ordering == "random")
                {
                    return TileJobFactory::RandomOrdering;
                }
                else
                {
                    RENDERER_LOG_ERROR(
                        "invalid value \"%s\" for parameter \"%s\", using default value \"%s\".",
                        tile_ordering.c_str(),
                        "tile_ordering",
                        "hilbert");

                    return TileJobFactory::HilbertOrdering;
                }
            }
        };

        class PassManagerFunc
          : public NonCopyable
        {
          public:
            PassManagerFunc(
                const Frame&                        frame,
                IShadingResultFrameBufferFactory*   framebuffer_factory,
                std::vector<ITileRenderer*>&        tile_renderers,
                std::vector<ITileCallback*>&        tile_callbacks,
                IPassCallback*                      pass_callback,
                const Spectrum::Mode                spectrum_mode,
                const TileJobFactory::TileOrdering  tile_ordering,
                const size_t                        pass_count,
                JobQueue&                           job_queue,
                const size_t                        thread_count,
                IAbortSwitch&                       abort_switch,
                bool&                               is_rendering)
              : m_frame(frame)
              , m_framebuffer_factory(framebuffer_factory)
              , m_tile_renderers(tile_renderers)
              , m_tile_callbacks(tile_callbacks)
              , m_pass_callback(pass_callback)
              , m_spectrum_mode(spectrum_mode)
              , m_tile_ordering(tile_ordering)
              , m_pass_count(pass_count)
              , m_job_queue(job_queue)
              , m_thread_count(thread_count)
              , m_abort_switch(abort_switch)
              , m_is_rendering(is_rendering)
            {
            }

            void operator()()
            {
                set_current_thread_name("pass_manager");

                const size_t start_pass = m_frame.get_initial_pass();

                //
                // Rendering passes.
                //

                for (size_t pass = start_pass; pass < m_pass_count; ++pass)
                {
                    // Check abort flag.
                    if (m_abort_switch.is_aborted())
                    {
                        m_is_rendering = false;
                        return;
                    }

                    if (m_pass_count > 1)
                        RENDERER_LOG_INFO("--- beginning rendering pass #%s ---", pretty_uint(pass + 1).c_str());

                    // Invoke on_pass_begin() on the pass callback if there is one.
                    if (m_pass_callback)
                    {
                        assert(!m_job_queue.has_scheduled_or_running_jobs());
                        m_pass_callback->on_pass_begin(m_frame, m_job_queue, m_abort_switch);
                        assert(!m_job_queue.has_scheduled_or_running_jobs());
                    }

                    // Invoke on_tiled_frame_begin() on tile callbacks.
                    for (auto tile_callback : m_tile_callbacks)
                        tile_callback->on_tiled_frame_begin(&m_frame);

                    // Create tile jobs.
                    const std::uint32_t pass_hash = mix_uint32(m_frame.get_noise_seed(), static_cast<std::uint32_t>(pass));
                    TileJobFactory::TileJobVector tile_jobs;
                    m_tile_job_factory.create(
                        m_frame,
                        m_tile_ordering,
                        m_tile_renderers,
                        m_tile_callbacks,
                        m_thread_count,
                        pass_hash,
                        m_spectrum_mode,
                        tile_jobs,
                        m_abort_switch);

                    // Schedule tile jobs.
                    for (const_each<TileJobFactory::TileJobVector> i = tile_jobs; i; ++i)
                        m_job_queue.schedule(*i);

                    // Wait until tile jobs have effectively stopped.
                    m_job_queue.wait_until_completion();

                    // Invoke on_tiled_frame_end() on tile callbacks.
                    for (auto tile_callback : m_tile_callbacks)
                        tile_callback->on_tiled_frame_end(&m_frame);

                    // Invoke on_pass_end() on the pass callback if there is one.
                    if (m_pass_callback)
                    {
                        assert(!m_job_queue.has_scheduled_or_running_jobs());
                        m_pass_callback->on_pass_end(m_frame, m_job_queue, m_abort_switch);
                        assert(!m_job_queue.has_scheduled_or_running_jobs());
                    }

                    // Optionally write a checkpoint file to disk.
                    m_frame.save_checkpoint(m_framebuffer_factory, pass);
                }

                // Check abort flag.
                if (m_abort_switch.is_aborted())
                {
                    m_is_rendering = false;
                    return;
                }

                // Post-process AOVs.
                m_frame.post_process_aov_images();

                //
                // Denoising pass.
                //

                if (m_frame.get_denoising_mode() == Frame::DenoisingMode::Denoise)
                {
                    if (m_pass_count > 1)
                        RENDERER_LOG_INFO("--- beginning denoising pass ---");

                    // Call on_tile_begin() on all tiles of the frame.
                    on_tile_begin_whole_frame();

                    // Denoise the frame.
                    m_frame.denoise(m_thread_count, &m_abort_switch);

                    // Call on_tile_end() on all tiles of the frame.
                    on_tile_end_whole_frame();
                }

                m_is_rendering = false;
            }

          private:
            const Frame&                            m_frame;
            IShadingResultFrameBufferFactory*       m_framebuffer_factory;
            std::vector<ITileRenderer*>&            m_tile_renderers;
            std::vector<ITileCallback*>&            m_tile_callbacks;
            IPassCallback*                          m_pass_callback;
            const Spectrum::Mode                    m_spectrum_mode;
            const TileJobFactory::TileOrdering      m_tile_ordering;
            const size_t                            m_pass_count;
            JobQueue&                               m_job_queue;
            const size_t                            m_thread_count;
            IAbortSwitch&                           m_abort_switch;
            bool&                                   m_is_rendering;
            TileJobFactory                          m_tile_job_factory;

            void on_tile_begin_whole_frame()
            {
                if (!m_tile_callbacks.empty())
                {
                    ITileCallback* tile_callback = m_tile_callbacks.front();
                    const CanvasProperties& frame_props = m_frame.image().properties();

                    for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
                    {
                        for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
                            tile_callback->on_tile_begin(&m_frame, tx, ty, 0, 1);
                    }
                }
            }

            void on_tile_end_whole_frame()
            {
                if (!m_tile_callbacks.empty())
                {
                    ITileCallback* tile_callback = m_tile_callbacks.front();
                    const CanvasProperties& frame_props = m_frame.image().properties();

                    for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
                    {
                        for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
                            tile_callback->on_tile_end(&m_frame, tx, ty);
                    }
                }
            }
        };

        const Frame&                            m_frame;            // target framebuffer
        const Parameters                        m_params;

        JobQueue                                m_job_queue;
        std::unique_ptr<JobManager>             m_job_manager;
        AbortSwitch                             m_abort_switch;

        IShadingResultFrameBufferFactory*       m_framebuffer_factory;
        std::vector<ITileRenderer*>             m_tile_renderers;   // tile renderers, one per thread
        std::vector<ITileCallback*>             m_tile_callbacks;   // tile callbacks, none or one per thread
        IPassCallback*                          m_pass_callback;

        TileJobFactory                          m_tile_job_factory;

        bool                                    m_is_rendering;
        std::unique_ptr<PassManagerFunc>        m_pass_manager_func;
        std::unique_ptr<boost::thread>          m_pass_manager_thread;

        void print_tile_renderers_stats() const
        {
            assert(!m_tile_renderers.empty());

            StatisticsVector stats;

            for (auto tile_renderer : m_tile_renderers)
                stats.merge(tile_renderer->get_statistics());

            RENDERER_LOG_DEBUG("%s", stats.to_string().c_str());
        }
    };
}


//
// GenericFrameRendererFactory class implementation.
//

Dictionary GenericFrameRendererFactory::get_params_metadata()
{
    Dictionary metadata;

    metadata.dictionaries().insert(
        "tile_ordering",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "linear|spiral|hilbert|random")
            .insert("default", "spiral")
            .insert("label", "Tile Order")
            .insert("help", "Tile rendering order")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "linear",
                        Dictionary()
                            .insert("label", "Linear")
                            .insert("help", "Linear tile ordering"))
                    .insert(
                        "spiral",
                        Dictionary()
                            .insert("label", "Spiral")
                            .insert("help", "Spiral tile ordering"))
                    .insert(
                        "hilbert",
                        Dictionary()
                            .insert("label", "Hilbert")
                            .insert("help", "Hilbert tile ordering"))
                    .insert(
                        "random",
                        Dictionary()
                            .insert("label", "Random")
                            .insert("help", "Random tile ordering"))));

    return metadata;
}

GenericFrameRendererFactory::GenericFrameRendererFactory(
    const Frame&                        frame,
    IShadingResultFrameBufferFactory*   framebuffer_factory,
    ITileRendererFactory*               tile_renderer_factory,
    ITileCallbackFactory*               tile_callback_factory,
    IPassCallback*                      pass_callback,
    const ParamArray&                   params)
  : m_frame(frame)
  , m_framebuffer_factory(framebuffer_factory)
  , m_tile_renderer_factory(tile_renderer_factory)
  , m_tile_callback_factory(tile_callback_factory)
  , m_pass_callback(pass_callback)
  , m_params(params)
{
}

void GenericFrameRendererFactory::release()
{
    delete this;
}

IFrameRenderer* GenericFrameRendererFactory::create()
{
    return
        new GenericFrameRenderer(
            m_frame,
            m_framebuffer_factory,
            m_tile_renderer_factory,
            m_tile_callback_factory,
            m_pass_callback,
            m_params);
}

IFrameRenderer* GenericFrameRendererFactory::create(
    const Frame&                        frame,
    IShadingResultFrameBufferFactory*   framebuffer_factory,
    ITileRendererFactory*               tile_renderer_factory,
    ITileCallbackFactory*               tile_callback_factory,
    IPassCallback*                      pass_callback,
    const ParamArray&                   params)
{
    return
        new GenericFrameRenderer(
            frame,
            framebuffer_factory,
            tile_renderer_factory,
            tile_callback_factory,
            pass_callback,
            params);
}

}   // namespace renderer
