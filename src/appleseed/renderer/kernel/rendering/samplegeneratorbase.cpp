
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
#include "samplegeneratorbase.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/sampleaccumulationbuffer.h"

// appleseed.foundation headers.
#include "foundation/memory/memory.h"
#include "foundation/utility/job.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

namespace
{
    // Number of consecutive samples rendered by a job.
    // This parameter is only used to break correlation between jobs and samples.
    // It has no impact on performance.
    const size_t SampleBatchSize = 67;
}

SampleGeneratorBase::SampleGeneratorBase(
    const size_t                generator_index,
    const size_t                generator_count)
  : m_generator_index(generator_index)
  , m_stride((generator_count - 1) * SampleBatchSize)
{
    reset();
}

void SampleGeneratorBase::reset()
{
    m_sequence_index = m_generator_index * SampleBatchSize;
    m_current_batch_size = 0;
    m_invalid_sample_count = 0;
}

void SampleGeneratorBase::generate_samples(
    const size_t                sample_count,
    SampleAccumulationBuffer&   buffer,
    IAbortSwitch&               abort_switch)
{
    assert(sample_count > 0);

    clear_keep_memory(m_samples);
    m_samples.reserve(sample_count);

    size_t stored = 0;

    while (stored < sample_count)
    {
        stored += generate_samples(m_sequence_index, m_samples);
        ++m_sequence_index;

        if (++m_current_batch_size == SampleBatchSize)
        {
            m_current_batch_size = 0;
            m_sequence_index += m_stride;

            if (abort_switch.is_aborted())
                break;
        }
    }

    if (stored > 0)
        buffer.store_samples(stored, &m_samples[0], abort_switch);
}

void SampleGeneratorBase::signal_invalid_sample()
{
    // todo: mark pixel as faulty in the diagnostic map.

    ++m_invalid_sample_count;

    const size_t MaxWarningsPerThread = 5;

    if (m_invalid_sample_count <= MaxWarningsPerThread)
        RENDERER_LOG_WARNING("a sample had NaN, negative or infinite components and was ignored.");
    else if (m_invalid_sample_count == MaxWarningsPerThread + 1)
        RENDERER_LOG_WARNING("more invalid samples found, omitting warning messages for brevity.");
}

}   // namespace renderer
