
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/progressive/samplecounter.h"
#include "renderer/kernel/rendering/progressive/samplecounthistory.h"
#include "renderer/kernel/rendering/progressive/samplegeneratorjob.h"
#include "renderer/kernel/rendering/framerendererbase.h"
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/sampleaccumulationbuffer.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/analysis.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/math/aabb.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/thread.h"
#include "foundation/platform/timers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/job.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <limits>
#include <memory>
#include <string>
#include <vector>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    typedef vector<ISampleGenerator*> SampleGeneratorVector;
    typedef vector<ITileCallback*> TileCallbackVector;


    //
    // Progressive frame renderer.
    //

    class ProgressiveFrameRenderer
      : public FrameRendererBase
    {
      public:
        ProgressiveFrameRenderer(
            const Project&                  project,
            ISampleGeneratorFactory*        generator_factory,
            ITileCallbackFactory*           callback_factory,
            const ParamArray&               params)
          : m_frame(*project.get_frame())
          , m_params(params)
          , m_sample_counter(m_params.m_max_sample_count)
          , m_ref_image_avg_lum(0.0)
        {
            // We must have a generator factory, but it's OK not to have a callback factory.
            assert(generator_factory);

            // Create an accumulation buffer.
            m_buffer.reset(generator_factory->create_sample_accumulation_buffer());

            // Create and initialize the job manager.
            m_job_manager.reset(
                new JobManager(
                    global_logger(),
                    m_job_queue,
                    m_params.m_thread_count,
                    JobManager::KeepRunningOnEmptyQueue));

            // Instantiate sample generators, one per rendering thread.
            for (size_t i = 0; i < m_params.m_thread_count; ++i)
            {
                m_sample_generators.push_back(
                    generator_factory->create(i, m_params.m_thread_count));
            }

            // Instantiate tile callbacks, one per rendering thread.
            if (callback_factory)
            {
                for (size_t i = 0; i < m_params.m_thread_count; ++i)
                    m_tile_callbacks.push_back(callback_factory->create());
            }

            // Load the reference image if one is specified.
            if (!m_params.m_ref_image_path.empty())
            {
                const string ref_image_path =
                    project.search_paths().qualify(m_params.m_ref_image_path);

                RENDERER_LOG_DEBUG("loading reference image %s...", ref_image_path.c_str());

                GenericImageFileReader reader;
                m_ref_image.reset(reader.read(ref_image_path.c_str()));

                if (are_images_compatible(m_frame.image(), *m_ref_image))
                {
                    m_ref_image_avg_lum = compute_average_luminance(*m_ref_image.get());

                    RENDERER_LOG_DEBUG(
                        "reference image average luminance is %s.",
                        pretty_scalar(m_ref_image_avg_lum, 6).c_str());
                }
                else
                {
                    RENDERER_LOG_ERROR(
                        "the reference image is not compatible with the output frame "
                        "(different dimensions, tile size or number of channels).");

                    m_ref_image.reset();
                }
            }

            print_rendering_thread_count(m_params.m_thread_count);
        }

        virtual ~ProgressiveFrameRenderer()
        {
            // Tell the statistics printing thread to stop.
            m_abort_switch.abort();

            // Wait until the statistics printing thread is terminated.
            if (m_statistics_thread.get() && m_statistics_thread->joinable())
                m_statistics_thread->join();

            // Delete tile callbacks.
            for (const_each<TileCallbackVector> i = m_tile_callbacks; i; ++i)
                (*i)->release();

            // Delete sample generators.
            for (const_each<SampleGeneratorVector> i = m_sample_generators; i; ++i)
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
            m_buffer->clear();
            m_sample_counter.clear();

            // Reset sample generators.
            for (size_t i = 0; i < m_sample_generators.size(); ++i)
                m_sample_generators[i]->reset();

            // Schedule the first batch of jobs.
            for (size_t i = 0; i < m_sample_generators.size(); ++i)
            {
                m_job_queue.schedule(
                    new SampleGeneratorJob(
                        m_frame,
                        *m_buffer.get(),
                        m_sample_generators[i],
                        m_sample_counter,
                        m_tile_callbacks.empty() ? 0 : m_tile_callbacks[i],
                        m_job_queue,
                        i,                              // job index
                        m_sample_generators.size(),     // job count
                        0,                              // pass number
                        m_abort_switch));
            }

            // Start job execution.
            m_job_manager->start();

            // Create and start the statistics printing thread.
            m_statistics_func.reset(
                new StatisticsFunc(
                    m_frame,
                    *m_buffer.get(),
                    m_params.m_print_luminance_stats,
                    m_ref_image.get(),
                    m_ref_image_avg_lum,
                    m_abort_switch));
            ThreadFunctionWrapper<StatisticsFunc> wrapper(m_statistics_func.get());
            m_statistics_thread.reset(new thread(wrapper));
        }

        virtual void stop_rendering()
        {
            // First, delete scheduled jobs to prevent worker threads from picking them up.
            m_job_queue.clear_scheduled_jobs();

            // Tell rendering jobs and the statistics printing thread to stop.
            m_abort_switch.abort();

            // Wait until the statistics printing thread has stopped.
            m_statistics_thread->join();

            // Wait until rendering jobs have effectively stopped.
            m_job_queue.wait_until_completion();
        }

        virtual void terminate_rendering()
        {
            stop_rendering();

            m_job_manager->stop();

            m_statistics_func->write_rms_deviation_file();

            print_sample_generators_stats();
        }

        virtual bool is_rendering() const
        {
            return m_job_queue.has_scheduled_or_running_jobs();
        }

      private:
        struct Parameters
        {
            const size_t    m_thread_count;             // number of rendering threads
            const uint64    m_max_sample_count;         // maximum total number of samples to compute
            const bool      m_print_luminance_stats;    // compute and print luminance statistics?
            const string    m_ref_image_path;           // path to the reference image

            explicit Parameters(const ParamArray& params)
              : m_thread_count(FrameRendererBase::get_rendering_thread_count(params))
              , m_max_sample_count(params.get_optional<uint64>("max_samples", numeric_limits<uint64>::max()))
              , m_print_luminance_stats(params.get_optional<bool>("print_luminance_statistics", false))
              , m_ref_image_path(params.get_optional<string>("reference_image", ""))
            {
            }
        };

        class StatisticsFunc
          : public NonCopyable
        {
          public:
            StatisticsFunc(
                Frame&                      frame,
                SampleAccumulationBuffer&   buffer,
                const bool                  print_luminance_stats,
                const Image*                ref_image,
                const double                ref_image_avg_lum,
                IAbortSwitch&               abort_switch)
              : m_frame(frame)
              , m_buffer(buffer)
              , m_print_luminance_stats(print_luminance_stats)
              , m_ref_image(ref_image)
              , m_ref_image_avg_lum(ref_image_avg_lum)
              , m_abort_switch(abort_switch)
              , m_rcp_timer_frequency(1.0 / m_timer.frequency())
              , m_last_time(m_timer.read() * m_rcp_timer_frequency)
            {
                const Vector2u crop_window_extent = m_frame.get_crop_window().extent();
                const size_t pixel_count = (crop_window_extent.x + 1) * (crop_window_extent.y + 1);
                m_rcp_pixel_count = 1.0 / pixel_count;
            }

            void operator()()
            {
                while (!m_abort_switch.is_aborted())
                {
                    const double time = m_timer.read() * m_rcp_timer_frequency;
                    const double elapsed_seconds = time - m_last_time;

                    if (elapsed_seconds >= 1.0)
                    {
                        m_last_time = time;

                        record_and_print_perf_stats(time);

                        if (m_print_luminance_stats || m_ref_image)
                            record_and_print_convergence_stats();
                    }

                    foundation::sleep(5);   // needs full qualification
                }
            }

            void write_rms_deviation_file() const
            {
                if (!m_rmsd_history.empty())
                {
                    GnuplotFile plotfile;
                    plotfile.set_xlabel("Samples per Pixel");
                    plotfile
                        .new_plot()
                        .set_points(m_rmsd_history)
                        .set_title("RMS Deviation");
                    plotfile.write("rms_deviation.gnuplot");
                }
            }

          private:
            Frame&                          m_frame;
            SampleAccumulationBuffer&       m_buffer;
            const bool                      m_print_luminance_stats;
            const Image*                    m_ref_image;
            const double                    m_ref_image_avg_lum;
            IAbortSwitch&                   m_abort_switch;

            DefaultWallclockTimer           m_timer;
            double                          m_rcp_timer_frequency;
            double                          m_last_time;

            double                          m_rcp_pixel_count;
            SampleCountHistory<128>         m_sample_count_history;
            vector<Vector2d>                m_rmsd_history;             // RMS deviation history

            void record_and_print_perf_stats(const double time)
            {
                const uint64 samples = m_buffer.get_sample_count();
                m_sample_count_history.insert(time, samples);

                const double samples_per_pixel = samples * m_rcp_pixel_count;
                const uint64 samples_per_second = truncate<uint64>(m_sample_count_history.get_samples_per_second());

                RENDERER_LOG_INFO(
                    "%s samples, %s samples/pixel, %s samples/second",
                    pretty_uint(samples).c_str(),
                    pretty_scalar(samples_per_pixel).c_str(),
                    pretty_uint(samples_per_second).c_str());
            }

            void record_and_print_convergence_stats()
            {
                assert(m_print_luminance_stats || m_ref_image);

                string output;

                Image current_image(m_frame.image());
                m_frame.transform_to_output_color_space(current_image);

                if (m_print_luminance_stats)
                {
                    const double avg_lum = compute_average_luminance(current_image);
                    output += "average luminance " + pretty_scalar(avg_lum, 6);

                    if (m_ref_image)
                    {
                        const double avg_lum_delta = abs(m_ref_image_avg_lum - avg_lum);
                        output += " (";
                        output += pretty_percent(avg_lum_delta, m_ref_image_avg_lum, 3);
                        output += " error)";
                    }
                }

                if (m_ref_image)
                {
                    const double samples_per_pixel = m_buffer.get_sample_count() * m_rcp_pixel_count;
                    const double rmsd = compute_rms_deviation(current_image, *m_ref_image);

                    m_rmsd_history.push_back(Vector2d(samples_per_pixel, rmsd));

                    if (m_print_luminance_stats)
                        output += ", ";
                    output += "rms deviation " + pretty_scalar(rmsd, 6);
                }

                RENDERER_LOG_DEBUG("%s", output.c_str());
            }
        };

        Frame&                              m_frame;
        const Parameters                    m_params;
        SampleCounter                       m_sample_counter;

        auto_ptr<SampleAccumulationBuffer>  m_buffer;

        JobQueue                            m_job_queue;
        auto_ptr<JobManager>                m_job_manager;
        AbortSwitch                         m_abort_switch;

        SampleGeneratorVector               m_sample_generators;
        TileCallbackVector                  m_tile_callbacks;

        auto_ptr<Image>                     m_ref_image;
        double                              m_ref_image_avg_lum;

        auto_ptr<StatisticsFunc>            m_statistics_func;
        auto_ptr<thread>                    m_statistics_thread;

        void print_sample_generators_stats() const
        {
            assert(!m_sample_generators.empty());

            StatisticsVector stats;

            for (size_t i = 0; i < m_sample_generators.size(); ++i)
                stats.merge(m_sample_generators[i]->get_statistics());

            RENDERER_LOG_DEBUG("%s", stats.to_string().c_str());
        }
    };
}


//
// ProgressiveFrameRendererFactory class implementation.
//

ProgressiveFrameRendererFactory::ProgressiveFrameRendererFactory(
    const Project&              project,
    ISampleGeneratorFactory*    generator_factory,
    ITileCallbackFactory*       callback_factory,
    const ParamArray&           params)
  : m_project(project)
  , m_generator_factory(generator_factory)
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
            m_project,
            m_generator_factory,
            m_callback_factory,
            m_params);
}

IFrameRenderer* ProgressiveFrameRendererFactory::create(
    const Project&              project,
    ISampleGeneratorFactory*    generator_factory,
    ITileCallbackFactory*       callback_factory,
    const ParamArray&           params)
{
    return
        new ProgressiveFrameRenderer(
            project,
            generator_factory,
            callback_factory,
            params);
}

}   // namespace renderer
