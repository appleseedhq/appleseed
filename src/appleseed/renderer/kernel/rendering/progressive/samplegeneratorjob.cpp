
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
#include "samplegeneratorjob.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/kernel/rendering/progressive/samplecounter.h"
#include "renderer/kernel/rendering/sampleaccumulationbuffer.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cmath>

using namespace foundation;

namespace renderer
{

//
// SampleGeneratorJob::SamplingProfile class implementation.
//

std::uint64_t SampleGeneratorJob::SamplingProfile::get_job_sample_count(const std::uint64_t samples) const
{
    if (samples < m_samples_in_linear_phase)
        return m_samples_per_job_in_linear_phase;

    const double x = (samples - m_samples_in_linear_phase) * 0.001;
    const std::uint64_t y =
        m_samples_per_job_in_linear_phase +
        truncate<std::uint64_t>(std::pow(x, m_curve_exponent_in_exponential_phase));

    return std::min(y, m_max_samples_per_job_in_exponential_phase);
}


//
// SampleGeneratorJob class implementation.
//

//#define PRINT_DETAILED_PROGRESS

SampleGeneratorJob::SampleGeneratorJob(
    SampleAccumulationBuffer&   buffer,
    ISampleGenerator*           sample_generator,
    SampleCounter&              sample_counter,
    const SamplingProfile&      sampling_profile,
    const Spectrum::Mode        spectrum_mode,
    JobQueue&                   job_queue,
    const size_t                job_index,
    IAbortSwitch&               abort_switch)
  : m_buffer(buffer)
  , m_sample_generator(sample_generator)
  , m_sample_counter(sample_counter)
  , m_sampling_profile(sampling_profile)
  , m_spectrum_mode(spectrum_mode)
  , m_job_queue(job_queue)
  , m_job_index(job_index)
  , m_abort_switch(abort_switch)
{
}

void SampleGeneratorJob::execute(const size_t thread_index)
{
    // Initialize thread-local variables.
    Spectrum::set_mode(m_spectrum_mode);

#ifdef PRINT_DETAILED_PROGRESS
    Stopwatch<DefaultWallclockTimer> stopwatch(0);
    stopwatch.measure();
    const double t1 = stopwatch.get_seconds();
#endif

    // We will base the number of samples to be rendered by this job on
    // the number of samples already reserved (not necessarily rendered).
    const std::uint64_t current_sample_count = m_sample_counter.read();

    // Reserve a number of samples to be rendered by this job.
    const std::uint64_t job_sample_count = m_sampling_profile.get_job_sample_count(current_sample_count);
    const std::uint64_t acquired_sample_count = m_sample_counter.reserve(job_sample_count);

    // Terminate this job is there are no more samples to render.
    if (acquired_sample_count == 0)
        return;

    // The first phase is uninterruptible in order to always have something to
    // show during navigation. todo: the renderer freezes if it cannot generate
    // samples during this phase; fix.
    const bool abortable = current_sample_count > m_sampling_profile.m_samples_in_uninterruptible_phase;

    // Render the samples and store them into the accumulation buffer.
    if (abortable)
    {
        m_sample_generator->generate_samples(
            static_cast<size_t>(acquired_sample_count),
            m_buffer,
            m_abort_switch);
    }
    else
    {
        AbortSwitch no_abort;
        m_sample_generator->generate_samples(
            static_cast<size_t>(acquired_sample_count),
            m_buffer,
            no_abort);
    }

#ifdef PRINT_DETAILED_PROGRESS
    stopwatch.measure();
    const double t2 = stopwatch.get_seconds();

    RENDERER_LOG_DEBUG(
        "job " FMT_SIZE_T ", rendered " FMT_UINT64 " samples%s, in %s",
        m_job_index,
        acquired_sample_count,
        abortable ? "" : " (non-abortable)",
        pretty_time(t2 - t1).c_str());
#else
    (void)(m_job_index);
#endif

    // Reschedule this job.
    if (!abortable || !m_abort_switch.is_aborted())
        m_job_queue.schedule(this, false);
}

}   // namespace renderer
