
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Stephen Agyemang, The appleseedhq Organization
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
#include "gptpasscallback.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/platform/types.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <limits>

using namespace foundation;
using namespace std;

namespace renderer
{

GPTPassCallback::GPTPassCallback(
        const GPTParameters&            params,
        STree*                          sd_tree,
        const size_t                    sample_budget,
        const size_t                    max_passes)
  : m_params(params)
  , m_sd_tree(sd_tree)
  , m_passes_left_curr_iter(0)
  , m_passes_rendered(0)
  , m_last_extrapolated_variance(std::numeric_limits<float>::infinity())
  , m_sample_budget(sample_budget)
  , m_iter(0)
  , m_is_final_iter(false)
  , m_var_increase(false)
{
    m_max_passes = m_sample_budget / m_params.m_samples_per_pass;
    
    if(m_max_passes > max_passes)
        m_max_passes = max_passes;
    
    m_remaining_passes = m_max_passes;
}

void GPTPassCallback::release()
{
    delete this;
}

void GPTPassCallback::on_pass_begin(
    const Frame&            frame,
    JobQueue&               job_queue,
    IAbortSwitch&           abort_switch)
{
    if(m_passes_left_curr_iter > 0)
        return;
    
    // New iteration.

    // Prepare pass.
    m_num_passes_curr_iter = m_passes_left_curr_iter = std::min(size_t(1) << m_iter, m_remaining_passes);

    if(m_is_final_iter || m_remaining_passes - m_passes_left_curr_iter < 2 * m_passes_left_curr_iter)
    {
        m_passes_left_curr_iter = m_remaining_passes;
        m_is_final_iter = true;
        m_sd_tree->start_final_iteration();
    }
    
    if(!m_var_increase && m_iter > 0)
    {
        // Clear the frame.
        m_framebuffer->clear();
        m_sd_tree->buildSDTree();
        m_sd_tree->resetSDTree(m_iter, m_params.m_samples_per_pass);
    }

    ++m_iter;
}

bool GPTPassCallback::on_pass_end(
    const Frame&            frame,
    JobQueue&               job_queue,
    IAbortSwitch&           abort_switch)
{
    ++m_passes_rendered;
    --m_passes_left_curr_iter;
    --m_remaining_passes;

    if(m_passes_rendered >= m_max_passes)
    {
        // Do end logic.
        const float variance = m_framebuffer->estimator_variance();
        RENDERER_LOG_INFO("Final variance estimate: %s", pretty_scalar(variance, 7).c_str());

        return true;
    }

    if(m_passes_left_curr_iter == 0)
    {
        // Update the variance projection.
        const size_t remaining_passes_at_curr_iter_start = m_remaining_passes + m_num_passes_curr_iter;
        const size_t samples_rendered = m_passes_rendered * m_params.m_samples_per_pass;
        const float variance = m_framebuffer->estimator_variance();
        const float current_extraplolated_variance =
            variance * m_num_passes_curr_iter / remaining_passes_at_curr_iter_start;

        RENDERER_LOG_INFO("Variance: %s", pretty_scalar(variance, 7).c_str());

        RENDERER_LOG_INFO("Extrapolated variance:\n    Previous: %s\n    Current: %s\n",
                    pretty_scalar(m_last_extrapolated_variance, 7).c_str(),
                    pretty_scalar(current_extraplolated_variance, 7).c_str());

        if(samples_rendered > 256 && // TODO: make this number a user param?
           current_extraplolated_variance > m_last_extrapolated_variance)
        {
            RENDERER_LOG_INFO("Extrapolated variance is increasing, initiating final iteration");
            m_var_increase = m_is_final_iter = true;
        }

        m_last_extrapolated_variance = current_extraplolated_variance;
    }

    return false;
}

size_t GPTPassCallback::get_samples_per_pass() const
{
    return m_params.m_samples_per_pass;
}

void GPTPassCallback::set_framebuffer(
    VarianceTrackingShadingResultFrameBufferFactory* framebuffer)
{
    m_framebuffer = framebuffer;
}

}   // namespace renderer
