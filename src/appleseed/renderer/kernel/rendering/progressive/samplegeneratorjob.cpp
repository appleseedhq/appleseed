
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "foundation/utility/stopwatch.h"

// Boost headers.
#include "boost/static_assert.hpp"

// Standard headers.
#include <algorithm>
#include <cmath>

using namespace foundation;
using namespace std;

namespace renderer
{

//#define PRINT_DETAILED_PROGRESS

SampleGeneratorJob::SampleGeneratorJob(
    SampleAccumulationBuffer&   buffer,
    ISampleGenerator*           sample_generator,
    SampleCounter&              sample_counter,
    JobQueue&                   job_queue,
    const size_t                job_index,
    const size_t                job_count,
    IAbortSwitch&               abort_switch)
  : m_buffer(buffer)
  , m_sample_generator(sample_generator)
  , m_sample_counter(sample_counter)
  , m_job_queue(job_queue)
  , m_job_index(job_index)
  , m_job_count(job_count)
  , m_abort_switch(abort_switch)
{
}

namespace
{
    // Duration of the interruptible phase.
    // The higher this value, the more refined is the image but the lower is the interactivity.
    const uint64 SamplesInUninterruptiblePhase = 32 * 32 * 2;

    // Duration of the linear phase.
    const uint64 SamplesInLinearPhase = 500 * 1000;

    // Refinement rates of the image in the different phases.
    // The higher the refinement rates, the lower the parallelism / CPU efficiency.
    const uint64 SamplesPerJobInLinearPhase = 250;
    const uint64 MaxSamplesPerJobInExponentialPhase = 250 * 1000;

    // Shape of the curve in the exponential phase.
    const double CurveExponentInExponentialPhase = 2.0;

    // Constraints.
    BOOST_STATIC_ASSERT(SamplesPerJobInLinearPhase <= SamplesInUninterruptiblePhase);
}

uint64 SampleGeneratorJob::samples_to_samples_per_job(const uint64 samples)
{
    if (samples < SamplesInLinearPhase)
        return SamplesPerJobInLinearPhase;

    const double x = (samples - SamplesInLinearPhase) * 0.001;
    const uint64 y = SamplesPerJobInLinearPhase + truncate<uint64>(pow(x, CurveExponentInExponentialPhase));

    return min(y, MaxSamplesPerJobInExponentialPhase);
}

void SampleGeneratorJob::execute(const size_t thread_index)
{
#ifdef PRINT_DETAILED_PROGRESS
    Stopwatch<DefaultWallclockTimer> stopwatch(0);
    stopwatch.measure();
    const double t1 = stopwatch.get_seconds();
#endif

    // We will base the number of samples to be rendered by this job on
    // the number of samples already reserved (not necessarily rendered).
    const uint64 current_sample_count = m_sample_counter.read();

    // Reserve a number of samples to be rendered by this job.
    const uint64 acquired_sample_count =
        m_sample_counter.reserve(
            samples_to_samples_per_job(current_sample_count));

    // Terminate this job is there are no more samples to render.
    if (acquired_sample_count == 0)
        return;

    // The first phase is uninterruptible in order to always have something to
    // show during navigation. todo: the renderer freezes if it cannot generate
    // samples during this phase; fix.
    const bool abortable = current_sample_count > SamplesInUninterruptiblePhase;

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
#endif

    // Reschedule this job.
    if (!abortable || !m_abort_switch.is_aborted())
        m_job_queue.schedule(this, false);
}

}   // namespace renderer
