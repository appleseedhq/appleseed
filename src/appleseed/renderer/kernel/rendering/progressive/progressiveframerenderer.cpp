
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

// Interface header.
#include "progressiveframerenderer.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/progressive/progressiveframebuffer.h"
#include "renderer/kernel/rendering/progressive/samplecounter.h"
#include "renderer/kernel/rendering/progressive/samplegenerator.h"
#include "renderer/kernel/rendering/progressive/samplegeneratorjob.h"
#include "renderer/kernel/rendering/framerendererbase.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
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
    typedef vector<ISampleRenderer*> SampleRendererVector;
    typedef vector<SampleGenerator*> SampleGeneratorVector;
    typedef vector<ITileCallback*> TileCallbackVector;


    //
    // Progressive frame renderer.
    //

    class ProgressiveFrameRenderer
      : public FrameRendererBase
    {
      public:
        // Constructor.
        ProgressiveFrameRenderer(
            Frame&                  frame,
            ISampleRendererFactory* renderer_factory,
            ITileCallbackFactory*   callback_factory,   // may be 0
            const ParamArray&       params)
          : m_frame(frame)
          , m_params(params)
          , m_sample_counter(m_params.m_max_sample_count)
        {
            // We must have a renderer factory, but it's OK not to have a callback factory.
            assert(renderer_factory);

            // Create and initialize job manager.
            m_job_manager.reset(
                new JobManager(
                    global_logger(),
                    m_job_queue,
                    m_params.m_thread_count,
                    true));         // keep threads alive, even if there's no more jobs

            // Instantiate sample renderers, one per rendering thread.
            for (size_t i = 0; i < m_params.m_thread_count; ++i)
                m_sample_renderers.push_back(renderer_factory->create());

            if (callback_factory)
            {
                // Instantiate tile callbacks, one per rendering thread.
                for (size_t i = 0; i < m_params.m_thread_count; ++i)
                    m_tile_callbacks.push_back(callback_factory->create());
            }

            m_framebuffer.reset(
                new ProgressiveFrameBuffer(
                    frame.properties().m_canvas_width,
                    frame.properties().m_canvas_height));

            print_rendering_thread_count(m_params.m_thread_count);
        }

        // Destructor.
        virtual ~ProgressiveFrameRenderer()
        {
            // Delete tile callbacks.
            for (const_each<TileCallbackVector> i = m_tile_callbacks; i; ++i)
                (*i)->release();

            // Delete sample renderers.
            for (const_each<SampleRendererVector> i = m_sample_renderers; i; ++i)
                (*i)->release();
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Synchronous frame rendering.
        virtual void render()
        {
            start_rendering();
            m_job_queue.wait_until_completion();
        }

        // Start rendering.
        virtual void start_rendering()
        {
            assert(!is_rendering());
            assert(!m_job_queue.has_scheduled_or_running_jobs());

            m_abort_switch.clear();
            m_framebuffer->clear();
            m_sample_counter.clear();

            // Create sample generators, one per rendering thread.
            assert(m_sample_generators.empty());
            for (size_t i = 0; i < m_params.m_thread_count; ++i)
            {
                m_sample_generators.push_back(
                    new SampleGenerator(
                        m_frame,
                        m_sample_renderers[i],
                        i,
                        m_params.m_thread_count));
            }

            // Schedule the first batch of jobs.
            for (size_t i = 0; i < m_params.m_thread_count; ++i)
            {
                m_job_queue.schedule(
                    new SampleGeneratorJob(
                        m_frame,
                        *m_framebuffer.get(),
                        *m_sample_generators[i],
                        m_sample_counter,
                        m_tile_callbacks[i],
                        m_job_queue,
                        i,                              // job index
                        m_params.m_thread_count,        // job count
                        0,                              // pass number
                        m_abort_switch));
            }

            // Start job execution.
            m_job_manager->start();
        }

        // Stop rendering.
        virtual void stop_rendering()
        {
            m_abort_switch.abort();

            // Stop job execution.
            m_job_manager->stop();

            // Delete all non-executed jobs.
            m_job_queue.clear_scheduled_jobs();

            // Delete sample generators.
            for (const_each<SampleGeneratorVector> i = m_sample_generators; i; ++i)
                delete *i;
            m_sample_generators.clear();
        }

        // Return true if currently rendering.
        virtual bool is_rendering() const
        {
            return m_job_queue.has_scheduled_or_running_jobs();
        }

      private:
        // Parameters.
        struct Parameters
        {
            const size_t    m_thread_count;         // number of rendering threads
            const uint64    m_max_sample_count;     // maximum total number of samples to store in the framebuffer

            // Constructor, extract parameters.
            explicit Parameters(const ParamArray& params)
              : m_thread_count(FrameRendererBase::get_rendering_thread_count(params))
              , m_max_sample_count(params.get_optional<uint64>("max_samples", numeric_limits<uint64>::max()))
            {
            }
        };

        Frame&                              m_frame;
        const Parameters                    m_params;

        JobQueue                            m_job_queue;
        auto_ptr<JobManager>                m_job_manager;
        AbortSwitch                         m_abort_switch;

        SampleRendererVector                m_sample_renderers;
        SampleGeneratorVector               m_sample_generators;
        TileCallbackVector                  m_tile_callbacks;

        auto_ptr<ProgressiveFrameBuffer>    m_framebuffer;
        SampleCounter                       m_sample_counter;
    };
}


//
// ProgressiveFrameRendererFactory class implementation.
//

ProgressiveFrameRendererFactory::ProgressiveFrameRendererFactory(
    Frame&                  frame,
    ISampleRendererFactory* renderer_factory,
    ITileCallbackFactory*   callback_factory,
    const ParamArray&       params)
  : m_frame(frame)
  , m_renderer_factory(renderer_factory)  
  , m_callback_factory(callback_factory)
  , m_params(params)
{
}

void ProgressiveFrameRendererFactory::release()
{
    delete this;
}

IFrameRenderer* ProgressiveFrameRendererFactory::create()
{
    return
        new ProgressiveFrameRenderer(
            m_frame,
            m_renderer_factory,
            m_callback_factory,
            m_params);
}

IFrameRenderer* ProgressiveFrameRendererFactory::create(
    Frame&                  frame,
    ISampleRendererFactory* renderer_factory,
    ITileCallbackFactory*   callback_factory,
    const ParamArray&       params)
{
    return
        new ProgressiveFrameRenderer(
            frame,
            renderer_factory,
            callback_factory,
            params);
}

}   // namespace renderer
