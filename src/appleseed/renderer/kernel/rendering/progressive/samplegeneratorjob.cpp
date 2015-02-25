
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
#include "samplegeneratorjob.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/progressive/samplecounter.h"
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/kernel/rendering/sampleaccumulationbuffer.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <algorithm>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// SampleGeneratorJob class implementation.
//

SampleGeneratorJob::SampleGeneratorJob(
    SampleAccumulationBuffer&   buffer,
    ISampleGenerator*           sample_generator,
    SampleCounter&              sample_counter,
    JobQueue&                   job_queue,
    const size_t                job_index,
    const size_t                job_count,
    const size_t                pass,
    IAbortSwitch&               abort_switch)
  : m_buffer(buffer)
  , m_sample_generator(sample_generator)
  , m_sample_counter(sample_counter)
  , m_job_queue(job_queue)
  , m_job_index(job_index)
  , m_job_count(job_count)
  , m_pass(pass)
  , m_abort_switch(abort_switch)
{
}

//#define PRINT_DETAILED_PROGRESS

void SampleGeneratorJob::execute(const size_t thread_index)
{
    const uint64 MinStoredSamples = 8192;

    if (m_sample_counter.read() < MinStoredSamples)
    {
        // Reserve a number of samples to be rendered by this job.
        const size_t DesiredSampleCount = 1024;
        const size_t acquired_sample_count = m_sample_counter.reserve(DesiredSampleCount);

        // Terminate this job is there are no more samples to render.
        if (acquired_sample_count == 0)
            return;

#ifdef PRINT_DETAILED_PROGRESS
        RENDERER_LOG_DEBUG(
            "job " FMT_SIZE_T ", pass " FMT_SIZE_T ", rendering " FMT_SIZE_T " samples, non-abortable",
            m_job_index,
            m_pass,
            acquired_sample_count);
#endif

        // The first pass is uninterruptible in order to always get something
        // on screen during navigation. todo: this needs to change as it will
        // freeze rendering if the first pass cannot generate samples.
        AbortSwitch no_abort;
        m_sample_generator->generate_samples(
            acquired_sample_count,
            m_buffer,
            no_abort);
    }
    else
    {
        // Reserve a number of samples to be rendered by this job.
        const size_t MaxSamplesPerPass = 256 * 1024;
        const size_t desired_sample_count = min((m_pass + 1) * 8192, MaxSamplesPerPass);
        const size_t acquired_sample_count = m_sample_counter.reserve(desired_sample_count);

        // Terminate this job is there are no more samples to render.
        if (acquired_sample_count == 0)
            return;

#ifdef PRINT_DETAILED_PROGRESS
        RENDERER_LOG_DEBUG(
            "job " FMT_SIZE_T ", pass " FMT_SIZE_T ", rendering " FMT_SIZE_T " samples",
            m_job_index,
            m_pass,
            acquired_sample_count);
#endif

        // Render the samples and store them into the accumulation buffer.
        m_sample_generator->generate_samples(
            acquired_sample_count,
            m_buffer,
            m_abort_switch);
    }

    // Reschedule this job.
    if (!m_abort_switch.is_aborted())
    {
        m_job_queue.schedule(
            new SampleGeneratorJob(
                m_buffer,
                m_sample_generator,
                m_sample_counter,
                m_job_queue,
                m_job_index,
                m_job_count,
                m_pass + 1,
                m_abort_switch));
    }
}

}   // namespace renderer
