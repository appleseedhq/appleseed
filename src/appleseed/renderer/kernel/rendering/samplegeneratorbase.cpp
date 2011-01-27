
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
#include "samplegeneratorbase.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/progressiveframebuffer.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/job.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

namespace
{
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
}

void SampleGeneratorBase::generate_samples(
    const size_t                sample_count,
    ProgressiveFrameBuffer&     framebuffer,
    AbortSwitch&                abort_switch)
{
    assert(sample_count > 0);

    ensure_size(m_samples, sample_count);

    Sample* RESTRICT sample_org = &m_samples[0];
    Sample* RESTRICT sample_ptr = sample_org;
    Sample* RESTRICT sample_end = sample_ptr + sample_count;

    while (sample_ptr < sample_end)
    {
        generate_sample(*sample_ptr++);

        ++m_sequence_index;

        if (++m_current_batch_size == SampleBatchSize)
        {
            m_current_batch_size = 0;
            m_sequence_index += m_stride;

            if (abort_switch.is_aborted())
                break;
        }
    }

    // Only store those samples that were effectively computed.
    framebuffer.store_samples(sample_ptr - sample_org, sample_org);
}

}   // namespace renderer
