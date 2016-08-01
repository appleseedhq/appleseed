
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_SAMPLEGENERATORJOB_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_SAMPLEGENERATORJOB_H

// appleseed.foundation headers.
#include "foundation/platform/types.h"
#include "foundation/utility/job.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class ISampleGenerator; }
namespace renderer  { class SampleAccumulationBuffer; }
namespace renderer  { class SampleCounter; }

namespace renderer
{

class SampleGeneratorJob
  : public foundation::IJob
{
  public:
    // Number of samples per job as a function of the number of samples already rendered.
    static foundation::uint64 samples_to_samples_per_job(
        const foundation::uint64    samples);

    // Constructor.
    SampleGeneratorJob(
        SampleAccumulationBuffer&   buffer,
        ISampleGenerator*           sample_generator,
        SampleCounter&              sample_counter,
        foundation::JobQueue&       job_queue,
        const size_t                job_index,
        const size_t                job_count,
        foundation::IAbortSwitch&   abort_switch);

    // Execute the job.
    virtual void execute(const size_t thread_index);

  private:
    SampleAccumulationBuffer&       m_buffer;
    ISampleGenerator*               m_sample_generator;
    SampleCounter&                  m_sample_counter;
    foundation::JobQueue&           m_job_queue;
    const size_t                    m_job_index;
    const size_t                    m_job_count;
    foundation::IAbortSwitch&       m_abort_switch;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_SAMPLEGENERATORJOB_H
