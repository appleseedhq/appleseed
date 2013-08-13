
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
#include "genericframerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/generic/tilejob.h"
#include "renderer/kernel/rendering/generic/tilejobfactory.h"
#include "renderer/kernel/rendering/framerendererbase.h"
#include "renderer/kernel/rendering/ipasscallback.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/itilerenderer.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/job.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Generic frame renderer.
    //

    class GenericFrameRenderer
      : public FrameRendererBase
    {
      public:
        typedef GenericFrameRendererFactory::PassMode PassMode;

        GenericFrameRenderer(
            const Frame&            frame,
            ITileRendererFactory*   tile_renderer_factory,
            ITileCallbackFactory*   tile_callback_factory,
            IPassCallback*          pass_callback,
            const PassMode          pass_mode,
            const ParamArray&       params)
          : m_frame(frame)
          , m_pass_mode(pass_mode)
          , m_params(params)
          , m_pass_callback(pass_callback)
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
            for (size_t i = 0; i < m_params.m_thread_count; ++i)
                m_tile_renderers.push_back(tile_renderer_factory->create(i == 0));

            if (tile_callback_factory)
            {
                // Instantiate tile callbacks, one per rendering thread.
                for (size_t i = 0; i < m_params.m_thread_count; ++i)
                    m_tile_callbacks.push_back(tile_callback_factory->create());
            }

            print_rendering_thread_count(m_params.m_thread_count);
        }

        virtual ~GenericFrameRenderer()
        {
            // Delete tile callbacks.
            for (const_each<vector<ITileCallback*> > i = m_tile_callbacks; i; ++i)
                (*i)->release();

            // Delete tile renderers.
            for (const_each<vector<ITileRenderer*> > i = m_tile_renderers; i; ++i)
                (*i)->release();
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual void render() OVERRIDE
        {
            start_rendering();
            m_job_queue.wait_until_completion();
        }

        virtual void start_rendering() OVERRIDE
        {
            assert(!is_rendering());
            assert(!m_job_queue.has_scheduled_or_running_jobs());

            m_abort_switch.clear();

            if (m_pass_mode == GenericFrameRendererFactory::SinglePass)
            {
                // Create tile jobs.
                TileJobFactory::TileJobVector tile_jobs;
                m_tile_job_factory.create(
                    m_frame,
                    m_params.m_tile_ordering,
                    m_tile_renderers,
                    m_tile_callbacks,
                    tile_jobs,
                    m_abort_switch);

                // Schedule tile jobs.
                for (const_each<TileJobFactory::TileJobVector> i = tile_jobs; i; ++i)
                    m_job_queue.schedule(*i);
            }
            else
            {
                // Create and schedule the initial pass job.
                m_job_queue.schedule(
                    new PassJob(
                        m_frame,
                        m_params.m_tile_ordering,
                        m_tile_renderers,
                        m_tile_callbacks,
                        m_pass_callback,
                        m_job_queue,
                        m_abort_switch));
            }

            // Start job execution.
            m_job_manager->start();
        }

        virtual void stop_rendering() OVERRIDE
        {
            // Tell rendering jobs to stop.
            m_abort_switch.abort();

            // Stop job execution.
            m_job_manager->stop();

            // Delete all non-executed tile jobs.
            m_job_queue.clear_scheduled_jobs();
        }

        virtual void terminate_rendering() OVERRIDE
        {
            stop_rendering();

            print_tile_renderers_stats();
        }

        virtual bool is_rendering() const OVERRIDE
        {
            return m_job_queue.has_scheduled_or_running_jobs();
        }

      private:
        struct Parameters
        {
            const size_t                        m_thread_count;     // number of rendering threads
            const TileJobFactory::TileOrdering  m_tile_ordering;    // tile rendering order

            explicit Parameters(const ParamArray& params)
              : m_thread_count(FrameRendererBase::get_rendering_thread_count(params))
              , m_tile_ordering(get_tile_ordering(params))
            {
            }

            static TileJobFactory::TileOrdering get_tile_ordering(const ParamArray& params)
            {
                const string tile_ordering =
                    params.get_optional<string>("tile_ordering", "hilbert");

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

        class PassJob
          : public IJob
        {
          public:
            PassJob(
                const Frame&                        frame,
                const TileJobFactory::TileOrdering  tile_ordering,
                vector<ITileRenderer*>&             tile_renderers,
                vector<ITileCallback*>&             tile_callbacks,
                IPassCallback*                      pass_callback,
                JobQueue&                           job_queue,
                AbortSwitch&                        abort_switch)
              : m_frame(frame)
              , m_tile_ordering(tile_ordering)
              , m_tile_renderers(tile_renderers)
              , m_tile_callbacks(tile_callbacks)
              , m_pass_callback(pass_callback)
              , m_job_queue(job_queue)
              , m_abort_switch(abort_switch)
            {
            }

            virtual void execute(const size_t thread_index) OVERRIDE
            {
                // Invoke the pass callback if there is one.
                if (m_pass_callback)
                    m_pass_callback->pre_render();

                // Create tile jobs.
                TileJobFactory::TileJobVector tile_jobs;
                m_tile_job_factory.create(
                    m_frame,
                    m_tile_ordering,
                    m_tile_renderers,
                    m_tile_callbacks,
                    tile_jobs,
                    m_abort_switch);

                // Schedule tile jobs.
                for (const_each<TileJobFactory::TileJobVector> i = tile_jobs; i; ++i)
                    m_job_queue.schedule(*i);

                // This job reschedules itself automatically.
                if (!m_abort_switch.is_aborted())
                {
                    m_job_queue.schedule(
                        new PassJob(
                            m_frame,
                            m_tile_ordering,
                            m_tile_renderers,
                            m_tile_callbacks,
                            m_pass_callback,
                            m_job_queue,
                            m_abort_switch));
                }
            }

          private:
            const Frame&                            m_frame;
            const TileJobFactory::TileOrdering      m_tile_ordering;
            vector<ITileRenderer*>&                 m_tile_renderers;
            vector<ITileCallback*>&                 m_tile_callbacks;
            IPassCallback*                          m_pass_callback;
            JobQueue&                               m_job_queue;
            AbortSwitch&                            m_abort_switch;
            TileJobFactory                          m_tile_job_factory;
        };

        const Frame&                m_frame;            // target framebuffer
        const PassMode              m_pass_mode;
        const Parameters            m_params;

        JobQueue                    m_job_queue;
        auto_ptr<JobManager>        m_job_manager;
        AbortSwitch                 m_abort_switch;

        vector<ITileRenderer*>      m_tile_renderers;   // tile renderers, one per thread
        vector<ITileCallback*>      m_tile_callbacks;   // tile callbacks, none or one per thread
        IPassCallback*              m_pass_callback;

        TileJobFactory              m_tile_job_factory;

        void print_tile_renderers_stats() const
        {
            assert(!m_tile_renderers.empty());

            StatisticsVector stats;

            for (size_t i = 0; i < m_tile_renderers.size(); ++i)
                stats.merge(m_tile_renderers[i]->get_statistics());

            RENDERER_LOG_DEBUG("%s", stats.to_string().c_str());
        }
    };
}


//
// GenericFrameRendererFactory class implementation.
//

GenericFrameRendererFactory::GenericFrameRendererFactory(
    const Frame&            frame,
    ITileRendererFactory*   tile_renderer_factory,
    ITileCallbackFactory*   tile_callback_factory,
    IPassCallback*          pass_callback,
    const PassMode          pass_mode,
    const ParamArray&       params)
  : m_frame(frame)
  , m_tile_renderer_factory(tile_renderer_factory)  
  , m_tile_callback_factory(tile_callback_factory)
  , m_pass_callback(pass_callback)
  , m_pass_mode(pass_mode)
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
            m_tile_renderer_factory,
            m_tile_callback_factory,
            m_pass_callback,
            m_pass_mode,
            m_params);
}

IFrameRenderer* GenericFrameRendererFactory::create(
    const Frame&            frame,
    ITileRendererFactory*   tile_renderer_factory,
    ITileCallbackFactory*   tile_callback_factory,
    IPassCallback*          pass_callback,
    const PassMode          pass_mode,
    const ParamArray&       params)
{
    return
        new GenericFrameRenderer(
            frame,
            tile_renderer_factory,
            tile_callback_factory,
            pass_callback,
            pass_mode,
            params);
}

}   // namespace renderer
