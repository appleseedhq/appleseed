
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

// Interface header.
#include "genericframerenderer.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/generic/tilejob.h"
#include "renderer/kernel/rendering/generic/tilejobfactory.h"
#include "renderer/kernel/rendering/framerendererbase.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/job.h"

// Standard headers.
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
        GenericFrameRenderer(
            const Frame&            frame,
            ITileRendererFactory*   renderer_factory,
            ITileCallbackFactory*   callback_factory,   // may be 0
            const ParamArray&       params)
          : m_frame(frame)
          , m_params(params)
        {
            // We must have a renderer factory, but it's OK not to have a callback factory.
            assert(renderer_factory);

            // Create and initialize job manager.
            m_job_manager.reset(
                new JobManager(
                    global_logger(),
                    m_job_queue,
                    m_params.m_thread_count));

            // Instantiate tile renderers, one per rendering thread.
            for (size_t i = 0; i < m_params.m_thread_count; ++i)
                m_tile_renderers.push_back(renderer_factory->create());

            if (callback_factory)
            {
                // Instantiate tile callbacks, one per rendering thread.
                for (size_t i = 0; i < m_params.m_thread_count; ++i)
                    m_tile_callbacks.push_back(callback_factory->create());
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

        virtual void release()
        {
            delete this;
        }

        virtual void render()
        {
            start_rendering();
            m_job_queue.wait_until_completion();
        }

        virtual void start_rendering()
        {
            assert(!is_rendering());
            assert(!m_job_queue.has_scheduled_or_running_jobs());

            m_abort_switch.clear();

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

            // Start job execution.
            m_job_manager->start();
        }

        virtual void stop_rendering()
        {
            // Tell rendering jobs to stop.
            m_abort_switch.abort();

            // Stop job execution.
            m_job_manager->stop();

            // Delete all non-executed tile jobs.
            m_job_queue.clear_scheduled_jobs();
        }

        virtual void terminate_rendering()
        {
            stop_rendering();
        }

        virtual bool is_rendering() const
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

        const Frame&                m_frame;            // target framebuffer
        const Parameters            m_params;

        JobQueue                    m_job_queue;
        auto_ptr<JobManager>        m_job_manager;
        AbortSwitch                 m_abort_switch;

        vector<ITileRenderer*>      m_tile_renderers;   // tile renderers, one per thread
        vector<ITileCallback*>      m_tile_callbacks;   // tile callbacks, none or one per thread

        TileJobFactory              m_tile_job_factory;
    };
}


//
// GenericFrameRendererFactory class implementation.
//

GenericFrameRendererFactory::GenericFrameRendererFactory(
    const Frame&            frame,
    ITileRendererFactory*   renderer_factory,
    ITileCallbackFactory*   callback_factory,
    const ParamArray&       params)
  : m_frame(frame)
  , m_renderer_factory(renderer_factory)  
  , m_callback_factory(callback_factory)
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
            m_renderer_factory,
            m_callback_factory,
            m_params);
}

IFrameRenderer* GenericFrameRendererFactory::create(
    const Frame&            frame,
    ITileRendererFactory*   renderer_factory,
    ITileCallbackFactory*   callback_factory,
    const ParamArray&       params)
{
    return
        new GenericFrameRenderer(
            frame,
            renderer_factory,
            callback_factory,
            params);
}

}   // namespace renderer
