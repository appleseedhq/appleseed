
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/utility/job.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

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
    struct SamplingProfile
    {
        // Duration of the interruptible phase.
        // The higher this value, the more refined is the image but the lower is the interactivity.
        std::uint64_t   m_samples_in_uninterruptible_phase = 32 * 32 * 2;

        // Duration of the linear phase.
        std::uint64_t   m_samples_in_linear_phase = 500 * 1000;

        // Refinement rates of the image in the different phases.
        // The higher the refinement rates, the lower the parallelism / CPU efficiency.
        std::uint64_t   m_samples_per_job_in_linear_phase = 250;
        std::uint64_t   m_max_samples_per_job_in_exponential_phase = 250 * 1000;

        // Shape of the curve in the exponential phase.
        double          m_curve_exponent_in_exponential_phase = 2.0;

        // Return the number of samples to be rendered by a job as a function of the number of samples already rendered.
        std::uint64_t get_job_sample_count(const std::uint64_t samples) const;
    };

    // Constructor.
    SampleGeneratorJob(
        SampleAccumulationBuffer&   buffer,
        ISampleGenerator*           sample_generator,
        SampleCounter&              sample_counter,
        const SamplingProfile&      sampling_profile,
        const Spectrum::Mode        spectrum_mode,
        foundation::JobQueue&       job_queue,
        const size_t                job_index,
        foundation::IAbortSwitch&   abort_switch);

    // Execute the job.
    void execute(const size_t thread_index) override;

  private:
    SampleAccumulationBuffer&       m_buffer;
    ISampleGenerator*               m_sample_generator;
    SampleCounter&                  m_sample_counter;
    const SamplingProfile           m_sampling_profile;
    const Spectrum::Mode            m_spectrum_mode;
    foundation::JobQueue&           m_job_queue;
    const size_t                    m_job_index;
    foundation::IAbortSwitch&       m_abort_switch;
};

}   // namespace renderer
