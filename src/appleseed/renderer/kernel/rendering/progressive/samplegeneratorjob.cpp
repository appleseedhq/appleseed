
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/kernel/rendering/progressive/progressiveframebuffer.h"
#include "renderer/kernel/rendering/progressive/samplegenerator.h"
#include "renderer/kernel/rendering/itilecallback.h"

// appleseed.foundation headers.
#include "foundation/utility/memory.h"

// Standard headers.
#include <algorithm>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// SampleGeneratorJob class implementation.
//

// Constructor.
SampleGeneratorJob::SampleGeneratorJob(
    Frame&                  frame,
    ProgressiveFrameBuffer& framebuffer,
    SampleGenerator&        sample_generator,
    ITileCallback*          tile_callback,
    JobQueue&               job_queue,
    const size_t            job_index,
    const size_t            job_count,
    const size_t            pass)
  : m_frame(frame)
  , m_framebuffer(framebuffer)
  , m_sample_generator(sample_generator)
  , m_tile_callback(tile_callback)
  , m_job_queue(job_queue)
  , m_job_index(job_index)
  , m_job_count(job_count)
  , m_pass(pass)
{
}

// Execute the job.
void SampleGeneratorJob::execute(const size_t thread_index)
{
    if (m_tile_callback)
    {
        m_tile_callback->pre_render(
            0,
            0,
            m_framebuffer.get_width(),
            m_framebuffer.get_height());
    }

    render();

    if (m_tile_callback)
        m_tile_callback->post_render(m_frame);

    m_job_queue.schedule(
        new SampleGeneratorJob(
            m_frame,
            m_framebuffer,
            m_sample_generator,
            m_tile_callback,
            m_job_queue,
            m_job_index,
            m_job_count,
            m_pass + 1));
}

size_t SampleGeneratorJob::get_sample_count() const
{
    const size_t MinSampleCount = 1024 * 4;
    const size_t MaxSampleCount = 1024 * 128;
    const size_t MinSamplePassCount = 8;
    const size_t SampleIncrement = 4096;

    if (m_pass < MinSamplePassCount)
        return MinSampleCount;
    else
    {
        const size_t p = m_pass - MinSamplePassCount;
        return min<size_t>(MinSampleCount + p * SampleIncrement, MaxSampleCount);
    }
}

void SampleGeneratorJob::render()
{
    const size_t sample_count = get_sample_count();

/*
    RENDERER_LOG_DEBUG(
        "sample generator job " FMT_SIZE_T "/" FMT_SIZE_T ": pass " FMT_SIZE_T ", generating " FMT_SIZE_T " samples",
        m_job_index + 1,
        m_job_count,
        m_pass,
        sample_count);
*/

    ensure_size(m_samples, sample_count);

    assert(!m_samples.empty());

    m_sample_generator.generate_samples(sample_count, &m_samples.front());
    m_framebuffer.store_samples(sample_count, &m_samples.front());

    const bool RoundRobinRender = false;

    if (!RoundRobinRender || m_pass % m_job_count == m_job_index)
        m_framebuffer.render_to_frame(m_frame);
}

}   // namespace renderer
