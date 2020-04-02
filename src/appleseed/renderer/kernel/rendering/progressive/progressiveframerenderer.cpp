
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
#include "progressiveframerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/progressive/samplecounter.h"
#include "renderer/kernel/rendering/progressive/samplecounthistory.h"
#include "renderer/kernel/rendering/progressive/samplegeneratorjob.h"
#include "renderer/kernel/rendering/sampleaccumulationbuffer.h"
#include "renderer/kernel/rendering/timedrenderercontroller.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/analysis.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/aabb.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/thread.h"
#include "foundation/platform/timers.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/job.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/stopwatch.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;
namespace bf = boost::filesystem;

namespace renderer
{

namespace
{
    using SampleCountHistoryType = SampleCountHistory<128>;


    //
    // Frame display thread.
    //

    class DisplayFunc
      : public NonCopyable
    {
      public:
        DisplayFunc(
            Frame&                      frame,
            SampleAccumulationBuffer&   buffer,
            SampleCountHistoryType&     sample_count_history,
            Spinlock&                   sample_count_history_spinlock,
            ITileCallback*              tile_callback,
            const double                max_fps,
            IAbortSwitch&               abort_switch)
          : m_frame(frame)
          , m_buffer(buffer)
          , m_sample_count_history(sample_count_history)
          , m_sample_count_history_spinlock(sample_count_history_spinlock)
          , m_tile_callback(tile_callback)
          , m_min_sample_count(std::min<std::uint64_t>(frame.get_crop_window().volume(), 32 * 32 * 2))
          , m_target_elapsed(1.0 / max_fps)
          , m_abort_switch(abort_switch)
          , m_rcp_timer_freq(1.0 / m_timer.frequency())
        {
            const Vector2u crop_window_extent = frame.get_crop_window().extent();
            const size_t pixel_count = crop_window_extent.x * crop_window_extent.y;
            m_rcp_pixel_count = 1.0 / pixel_count;
        }

        void pause()
        {
            m_pause_flag.set();
        }

        void resume()
        {
            m_pause_flag.clear();
        }

        void operator()()
        {
            set_current_thread_name("display");

            m_start_time = m_timer.read();

            std::uint64_t last_time = m_start_time;

#ifdef PRINT_DISPLAY_THREAD_PERFS
            m_stopwatch.start();
#endif

            while (!m_abort_switch.is_aborted())
            {
                // Compute time elapsed since last call to display().
                const std::uint64_t time = m_timer.read();
                const double elapsed = (time - last_time) * m_rcp_timer_freq;
                last_time = time;

                if (m_pause_flag.is_clear())
                {
                    // It's time to display but the sample accumulation buffer doesn't contain
                    // enough samples yet. Giving up would lead to noticeable jerkiness, so we
                    // wait until enough samples are available.
                    while (!m_abort_switch.is_aborted() &&
                           m_buffer.get_sample_count() < m_min_sample_count)
                        yield();

                    // Merge the samples and display the final frame.
                    develop_and_display();
                }

                // Limit display rate.
                if (elapsed < m_target_elapsed)
                {
                    const double ms = std::ceil(1000.0 * (m_target_elapsed - elapsed));
                    sleep(truncate<std::uint32_t>(ms), m_abort_switch);
                }
            }
        }

        void develop_and_display()
        {
#ifdef PRINT_DISPLAY_THREAD_PERFS
            m_stopwatch.measure();
            const double t1 = m_stopwatch.get_seconds();
#endif

            // Develop the accumulation buffer to the frame.
            m_buffer.develop_to_frame(m_frame, m_abort_switch);

#ifdef PRINT_DISPLAY_THREAD_PERFS
            m_stopwatch.measure();
            const double t2 = m_stopwatch.get_seconds();
#endif

            // Make sure we don't present incomplete frames.
            if (m_abort_switch.is_aborted())
                return;

            // Compute current rendering time.
            const double time = (m_timer.read() - m_start_time) * m_rcp_timer_freq;

            // Compute sampling statistics.
            const std::uint64_t samples = m_buffer.get_sample_count();
            const double samples_per_pixel = samples * m_rcp_pixel_count;
            m_sample_count_history_spinlock.lock();
            const std::uint64_t samples_per_second =
                truncate<std::uint64_t>(m_sample_count_history.get_samples_per_second());
            m_sample_count_history_spinlock.unlock();

            // Present the frame.
            m_tile_callback->on_progressive_frame_update(
                m_frame,
                time,
                samples,
                samples_per_pixel,
                samples_per_second);

#ifdef PRINT_DISPLAY_THREAD_PERFS
            m_stopwatch.measure();
            const double t3 = m_stopwatch.get_seconds();

            RENDERER_LOG_DEBUG(
                "display thread:\n"
                "  buffer to frame               %s\n"
                "  frame to widget               %s\n"
                "  total                         %s (%s fps)",
                pretty_time(t2 - t1).c_str(),
                pretty_time(t3 - t2).c_str(),
                pretty_time(t3 - t1).c_str(),
                pretty_ratio(1.0, t3 - t1).c_str());
#endif
        }

      private:
        Frame&                              m_frame;
        SampleAccumulationBuffer&           m_buffer;
        SampleCountHistoryType&             m_sample_count_history;
        Spinlock&                           m_sample_count_history_spinlock;
        ITileCallback*                      m_tile_callback;
        const std::uint64_t                 m_min_sample_count;
        const double                        m_target_elapsed;
        IAbortSwitch&                       m_abort_switch;
        ThreadFlag                          m_pause_flag;
        Stopwatch<DefaultWallclockTimer>    m_stopwatch;
        double                              m_rcp_pixel_count;
        DefaultWallclockTimer               m_timer;
        const double                        m_rcp_timer_freq;
        std::uint64_t                       m_start_time;
    };


    //
    // Statistics gathering and printing thread.
    //

    class StatisticsFunc
      : public NonCopyable
    {
      public:
        StatisticsFunc(
            const Project&              project,
            SampleAccumulationBuffer&   buffer,
            SampleCountHistoryType&     sample_count_history,
            Spinlock&                   sample_count_history_spinlock,
            const bool                  perf_stats,
            const bool                  luminance_stats,
            const Image*                ref_image,
            const double                ref_image_avg_lum,
            IAbortSwitch&               abort_switch)
          : m_project(project)
          , m_buffer(buffer)
          , m_sample_count_history(sample_count_history)
          , m_sample_count_history_spinlock(sample_count_history_spinlock)
          , m_perf_stats(perf_stats)
          , m_luminance_stats(luminance_stats)
          , m_ref_image(ref_image)
          , m_ref_image_avg_lum(ref_image_avg_lum)
          , m_abort_switch(abort_switch)
          , m_rcp_timer_frequency(1.0 / m_timer.frequency())
          , m_timer_start_value(m_timer.read())
        {
            const Vector2u crop_window_extent = m_project.get_frame()->get_crop_window().extent();
            const size_t pixel_count = crop_window_extent.x * crop_window_extent.y;
            m_rcp_pixel_count = 1.0 / pixel_count;
        }

        ~StatisticsFunc()
        {
            if (!m_sample_count_records.empty())
            {
                const std::string filepath =
                    (bf::path(m_project.search_paths().get_root_path().c_str()) / "sample_count.gnuplot").string();
                RENDERER_LOG_DEBUG("writing %s...", filepath.c_str());

                GnuplotFile plotfile;
                plotfile.set_xlabel("Time");
                plotfile.set_ylabel("Samples");
                plotfile
                    .new_plot()
                    .set_points(m_sample_count_records)
                    .set_title("Total Sample Count Over Time");
                plotfile.write(filepath);
            }

            if (!m_rmsd_records.empty())
            {
                const std::string filepath =
                    (bf::path(m_project.search_paths().get_root_path().c_str()) / "rms_deviation.gnuplot").string();
                RENDERER_LOG_DEBUG("writing %s...", filepath.c_str());

                GnuplotFile plotfile;
                plotfile.set_xlabel("Samples per Pixel");
                plotfile.set_ylabel("RMS Deviation");
                plotfile
                    .new_plot()
                    .set_points(m_rmsd_records)
                    .set_title("RMS Deviation Over Time");
                plotfile.write(filepath);
            }
        }

        void pause()
        {
            m_pause_flag.set();
        }

        void resume()
        {
            m_pause_flag.clear();
        }

        void operator()()
        {
            set_current_thread_name("statistics");

            while (!m_abort_switch.is_aborted())
            {
                if (m_pause_flag.is_clear())
                {
                    const double time = (m_timer.read() - m_timer_start_value) * m_rcp_timer_frequency;
                    record_and_print_perf_stats(time);

                    if (m_luminance_stats || m_ref_image)
                        record_and_print_convergence_stats();
                }

                sleep(1000, m_abort_switch);
            }
        }

      private:
        const Project&                  m_project;
        SampleAccumulationBuffer&       m_buffer;
        SampleCountHistoryType&         m_sample_count_history;
        Spinlock&                       m_sample_count_history_spinlock;
        const bool                      m_perf_stats;
        const bool                      m_luminance_stats;
        const Image*                    m_ref_image;
        const double                    m_ref_image_avg_lum;
        IAbortSwitch&                   m_abort_switch;
        ThreadFlag                      m_pause_flag;

        DefaultWallclockTimer           m_timer;
        double                          m_rcp_timer_frequency;
        std::uint64_t                   m_timer_start_value;

        double                          m_rcp_pixel_count;
        std::vector<Vector2d>           m_sample_count_records;     // total sample count over time
        std::vector<Vector2d>           m_rmsd_records;             // RMS deviation over time

        void record_and_print_perf_stats(const double time)
        {
            const std::uint64_t samples = m_buffer.get_sample_count();
            const double samples_per_pixel = samples * m_rcp_pixel_count;

            m_sample_count_history_spinlock.lock();
            m_sample_count_history.insert(time, samples);
            const std::uint64_t samples_per_second =
                truncate<std::uint64_t>(m_sample_count_history.get_samples_per_second());
            m_sample_count_history_spinlock.unlock();

            RENDERER_LOG_INFO(
                "%s samples, %s samples/pixel, %s samples/second",
                pretty_uint(samples).c_str(),
                pretty_scalar(samples_per_pixel).c_str(),
                pretty_uint(samples_per_second).c_str());

            if (m_perf_stats)
                m_sample_count_records.emplace_back(time, static_cast<double>(samples));
        }

        void record_and_print_convergence_stats()
        {
            assert(m_luminance_stats || m_ref_image);

            // Develop the accumulation buffer to the frame.
            m_buffer.develop_to_frame(*m_project.get_frame(), m_abort_switch);

            std::string output;

            if (m_luminance_stats)
            {
                const double avg_lum = compute_average_luminance(m_project.get_frame()->image());
                output += "average luminance " + pretty_scalar(avg_lum, 6);

                if (m_ref_image)
                {
                    const double avg_lum_delta = std::abs(m_ref_image_avg_lum - avg_lum);
                    output += " (";
                    output += pretty_percent(avg_lum_delta, m_ref_image_avg_lum, 3);
                    output += " error)";
                }
            }

            if (m_ref_image)
            {
                const double samples_per_pixel = m_buffer.get_sample_count() * m_rcp_pixel_count;
                const double rmsd = compute_rms_deviation(m_project.get_frame()->image(), *m_ref_image);
                m_rmsd_records.emplace_back(samples_per_pixel, rmsd);

                if (m_luminance_stats)
                    output += ", ";
                output += "rms deviation " + pretty_scalar(rmsd, 6);
            }

            RENDERER_LOG_DEBUG("%s", output.c_str());
        }
    };


    //
    // Progressive frame renderer.
    //

//#define PRINT_DISPLAY_THREAD_PERFS

    class ProgressiveFrameRenderer
      : public IFrameRenderer
    {
      public:
        ProgressiveFrameRenderer(
            const Project&                  project,
            ISampleGeneratorFactory*        generator_factory,
            ITileCallbackFactory*           callback_factory,
            const ParamArray&               params)
          : m_project(project)
          , m_params(params)
          , m_sample_counter(
                m_params.m_max_average_spp < std::numeric_limits<std::uint64_t>::max()
                    ? m_params.m_max_average_spp * project.get_frame()->get_crop_window().volume()
                    : m_params.m_max_average_spp)
          , m_ref_image_avg_lum(0.0)
          , m_renderer_controller(m_params.m_time_limit)
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
            m_sample_generators.reserve(m_params.m_thread_count);
            for (size_t i = 0; i < m_params.m_thread_count; ++i)
            {
                m_sample_generators.push_back(
                    generator_factory->create(i, m_params.m_thread_count));
            }

            // Create rendering jobs, one per rendering thread.
            m_sample_generator_jobs.reserve(m_params.m_thread_count);
            for (size_t i = 0; i < m_params.m_thread_count; ++i)
            {
                m_sample_generator_jobs.push_back(
                    new SampleGeneratorJob(
                        *m_buffer.get(),
                        m_sample_generators[i],
                        m_sample_counter,
                        m_params.m_sampling_profile,
                        m_params.m_spectrum_mode,
                        m_job_queue,
                        i,                              // job index
                        m_abort_switch));
            }

            // Instantiate a single tile callback.
            if (callback_factory)
                m_tile_callback.reset(callback_factory->create());

            if (m_project.get_frame()->has_valid_ref_image())
            {
                m_ref_image_avg_lum = compute_average_luminance(*m_project.get_frame()->ref_image());

                RENDERER_LOG_DEBUG(
                    "reference image average luminance is %s.",
                    pretty_scalar(m_ref_image_avg_lum, 6).c_str());
            }
        }

        ~ProgressiveFrameRenderer() override
        {
            // Stop the statistics thread.
            m_abort_switch.abort();
            if (m_statistics_thread.get() && m_statistics_thread->joinable())
                m_statistics_thread->join();

            // Stop the display thread.
            m_display_thread_abort_switch.abort();
            if (m_display_thread.get() && m_display_thread->joinable())
                m_display_thread->join();

            // Delete the tile callback.
            m_tile_callback.reset();

            // Delete rendering jobs.
            for (auto sample_generator_job : m_sample_generator_jobs)
                delete sample_generator_job;

            // Delete sample generators.
            for (auto sample_generator : m_sample_generators)
                sample_generator->release();
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
            RENDERER_LOG_INFO(
                "progressive frame renderer settings:\n"
                "  spectrum mode                 %s\n"
                "  sampling mode                 %s\n"
                "  rendering threads             %s\n"
                "  max average samples per pixel %s\n"
                "  time limit                    %s\n"
                "  max fps                       %f\n"
                "  collect performance stats     %s\n"
                "  collect luminance stats       %s",
                get_spectrum_mode_name(m_params.m_spectrum_mode).c_str(),
                get_sampling_context_mode_name(m_params.m_sampling_mode).c_str(),
                pretty_uint(m_params.m_thread_count).c_str(),
                m_params.m_max_average_spp == std::numeric_limits<std::uint64_t>::max()
                    ? "unlimited"
                    : pretty_uint(m_params.m_max_average_spp).c_str(),
                m_params.m_time_limit == std::numeric_limits<double>::max()
                    ? "unlimited"
                    : pretty_time(m_params.m_time_limit).c_str(),
                m_params.m_max_fps,
                m_params.m_perf_stats ? "on" : "off",
                m_params.m_luminance_stats ? "on" : "off");

            m_sample_generators.front()->print_settings();
        }

        IRendererController* get_renderer_controller() override
        {
            return &m_renderer_controller;
        }

        void render() override
        {
            start_rendering();

            m_job_queue.wait_until_completion();
        }

        bool is_rendering() const override
        {
            return m_job_queue.has_scheduled_or_running_jobs();
        }

        void start_rendering() override
        {
            assert(!is_rendering());
            assert(!m_job_queue.has_scheduled_or_running_jobs());

            m_abort_switch.clear();
            m_buffer->clear();
            m_sample_counter.clear();

            m_sample_count_history_spinlock.lock();
            m_sample_count_history.clear();
            m_sample_count_history_spinlock.unlock();

            // Reset sample generators.
            for (auto sample_generator : m_sample_generators)
                sample_generator->reset();

            // Schedule rendering jobs.
            for (auto sample_generator_job : m_sample_generator_jobs)
            {
                m_job_queue.schedule(
                    sample_generator_job,
                    false);     // don't transfer ownership of the job to the queue
            }

            // Start job execution.
            m_job_manager->start();

            // Create and start the statistics thread.
            m_statistics_func.reset(
                new StatisticsFunc(
                    m_project,
                    *m_buffer,
                    m_sample_count_history,
                    m_sample_count_history_spinlock,
                    m_params.m_perf_stats,
                    m_params.m_luminance_stats,
                    m_project.get_frame()->ref_image(),
                    m_ref_image_avg_lum,
                    m_abort_switch));
            m_statistics_thread.reset(
                new boost::thread(
                    ThreadFunctionWrapper<StatisticsFunc>(m_statistics_func.get())));

            // Create and start the display thread.
            if (m_tile_callback.get() != nullptr && m_display_thread.get() == nullptr)
            {
                m_display_func.reset(
                    new DisplayFunc(
                        *m_project.get_frame(),
                        *m_buffer,
                        m_sample_count_history,
                        m_sample_count_history_spinlock,
                        m_tile_callback.get(),
                        m_params.m_max_fps,
                        m_display_thread_abort_switch));
                m_display_thread.reset(
                    new boost::thread(
                        ThreadFunctionWrapper<DisplayFunc>(m_display_func.get())));
            }

            // Resume rendering if it was paused.
            resume_rendering();
        }

        void stop_rendering() override
        {
            // First, delete scheduled jobs to prevent worker threads from picking them up.
            m_job_queue.clear_scheduled_jobs();

            // Tell rendering jobs and the statistics thread to stop.
            m_abort_switch.abort();

            // Wait until rendering jobs have effectively stopped.
            m_job_queue.wait_until_completion();

            // Wait until the statistics thread has stopped.
            m_statistics_thread->join();
        }

        void pause_rendering() override
        {
            m_job_manager->pause();

            if (m_display_func.get())
                m_display_func->pause();

            m_statistics_func->pause();
        }

        void resume_rendering() override
        {
            m_statistics_func->resume();

            if (m_display_func.get())
                m_display_func->resume();

            m_job_manager->resume();
        }

        void terminate_rendering() override
        {
            // Completely stop rendering.
            stop_rendering();
            m_job_manager->stop();

            // The statistics thread has already been joined in stop_rendering().
            m_statistics_thread.reset();
            m_statistics_func.reset();

            // Join and delete the display thread.
            if (m_display_thread.get())
            {
                m_display_thread_abort_switch.abort();
                m_display_thread->join();
                m_display_thread.reset();
            }

            // Make sure the remaining calls of this method don't get interrupted.
            m_abort_switch.clear();
            m_display_thread_abort_switch.clear();

            if (m_display_func.get())
            {
                // Merge the last samples and display the final frame.
                m_display_func->develop_and_display();
                m_display_func.reset();
            }
            else
            {
                // Just merge the last samples into the frame.
                m_buffer->develop_to_frame(*m_project.get_frame(), m_abort_switch);
            }

            // Merge and print sample generator statistics.
            print_sample_generators_stats();
        }

      private:
        struct Parameters
        {
            const Spectrum::Mode                    m_spectrum_mode;
            const SamplingContext::Mode             m_sampling_mode;
            const size_t                            m_thread_count;       // number of rendering threads
            const std::uint64_t                     m_max_average_spp;    // maximum average number of samples to compute per pixel
            const double                            m_time_limit;         // maximum rendering time in seconds
            const double                            m_max_fps;            // maximum display frequency in frames/second
            const bool                              m_perf_stats;         // collect and print performance statistics?
            const bool                              m_luminance_stats;    // collect and print luminance statistics?
            SampleGeneratorJob::SamplingProfile     m_sampling_profile;

            explicit Parameters(const ParamArray& params)
              : m_spectrum_mode(get_spectrum_mode(params))
              , m_sampling_mode(get_sampling_context_mode(params))
              , m_thread_count(get_rendering_thread_count(params))
              , m_max_average_spp(params.get_optional<std::uint64_t>("max_average_spp", std::numeric_limits<std::uint64_t>::max()))
              , m_time_limit(params.get_optional<double>("time_limit", std::numeric_limits<double>::max()))
              , m_max_fps(params.get_optional<double>("max_fps", 30.0))
              , m_perf_stats(params.get_optional<bool>("performance_statistics", false))
              , m_luminance_stats(params.get_optional<bool>("luminance_statistics", false))
            {
                const SampleGeneratorJob::SamplingProfile default_sampling_profile;
                m_sampling_profile.m_samples_in_uninterruptible_phase =
                    params.get_optional<std::uint64_t>("samples_in_uninterruptible_phase", default_sampling_profile.m_samples_in_uninterruptible_phase);
                m_sampling_profile.m_samples_in_linear_phase =
                    params.get_optional<std::uint64_t>("samples_in_linear_phase", default_sampling_profile.m_samples_in_linear_phase);
                m_sampling_profile.m_samples_per_job_in_linear_phase =
                    params.get_optional<std::uint64_t>("samples_per_job_in_linear_phase", default_sampling_profile.m_samples_per_job_in_linear_phase);
                m_sampling_profile.m_max_samples_per_job_in_exponential_phase =
                    params.get_optional<std::uint64_t>("max_samples_per_job_in_exponential_phase", default_sampling_profile.m_max_samples_per_job_in_exponential_phase);
                m_sampling_profile.m_curve_exponent_in_exponential_phase =
                    params.get_optional<double>("curve_exponent_in_exponential_phase", default_sampling_profile.m_curve_exponent_in_exponential_phase);
            }
        };

        using SampleGeneratorVector = std::vector<ISampleGenerator*>;
        using SampleGeneratorJobVector = std::vector<SampleGeneratorJob*>;

        const Project&                              m_project;
        const Parameters                            m_params;
        SampleCounter                               m_sample_counter;
        SampleCountHistoryType                      m_sample_count_history;
        Spinlock                                    m_sample_count_history_spinlock;

        std::unique_ptr<SampleAccumulationBuffer>   m_buffer;

        JobQueue                                    m_job_queue;
        std::unique_ptr<JobManager>                 m_job_manager;
        AbortSwitch                                 m_abort_switch;

        SampleGeneratorVector                       m_sample_generators;
        SampleGeneratorJobVector                    m_sample_generator_jobs;

        auto_release_ptr<ITileCallback>             m_tile_callback;

        std::unique_ptr<DisplayFunc>                m_display_func;
        std::unique_ptr<boost::thread>              m_display_thread;
        AbortSwitch                                 m_display_thread_abort_switch;

        double                                      m_ref_image_avg_lum;
        std::unique_ptr<StatisticsFunc>             m_statistics_func;
        std::unique_ptr<boost::thread>              m_statistics_thread;

        TimedRendererController                     m_renderer_controller;

        void print_sample_generators_stats() const
        {
            assert(!m_sample_generators.empty());

            StatisticsVector stats;

            for (auto sample_generator : m_sample_generators)
                stats.merge(sample_generator->get_statistics());

            RENDERER_LOG_DEBUG("%s", stats.to_string().c_str());
        }
    };
}


//
// ProgressiveFrameRendererFactory class implementation.
//

Dictionary ProgressiveFrameRendererFactory::get_params_metadata()
{
    Dictionary metadata;

    metadata.dictionaries().insert(
        "max_fps",
        Dictionary()
            .insert("type", "float")
            .insert("default", "30.0")
            .insert("label", "Max FPS")
            .insert("help", "Maximum progressive rendering update rate in frames per second"));

    metadata.dictionaries().insert(
        "max_average_spp",
        Dictionary()
            .insert("type", "int")
            .insert("default", "64")
            .insert("unlimited", "true")
            .insert("label", "Max Average Samples Per Pixel")
            .insert("help", "Maximum number of average samples per pixel"));

    metadata.dictionaries().insert(
        "time_limit",
        Dictionary()
            .insert("type", "int")
            .insert("default", "60")
            .insert("unlimited", "true")
            .insert("label", "Time Limit:")
            .insert("help", "Maximum rendering time"));

    return metadata;
}

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
