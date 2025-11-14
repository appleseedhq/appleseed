
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
#include "foundation/string/string.h"

// Standard headers.
#include <algorithm>
#include <limits>

using namespace foundation;
using namespace std;

namespace renderer
{

const size_t ImageBufferCapacity = 4;

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
    
    if (m_max_passes > max_passes)
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
    if (m_passes_left_curr_iter > 0)
        return;
    
    // New iteration.

    // Prepare pass.
    m_num_passes_curr_iter = m_passes_left_curr_iter = std::min(size_t(1) << m_iter, m_remaining_passes);

    if (m_is_final_iter || m_remaining_passes - m_passes_left_curr_iter < 2 * m_passes_left_curr_iter)
    {
        m_passes_left_curr_iter = m_remaining_passes;
        m_is_final_iter = true;
        m_sd_tree->start_final_iteration();
    }
    
    if (!m_var_increase && m_iter > 0)
    {
        // Clear the frame and build the tree.
        m_framebuffer->clear();
        m_sd_tree->build(m_iter);

        switch (m_params.m_save_mode)
        {
        case SaveMode::All:
            m_sd_tree->write_to_disk(m_iter, true);
            break;

        case SaveMode::Final:
            if(m_is_final_iter)
                m_sd_tree->write_to_disk(m_iter, false);
            break;
        
        default:
            break;
        }
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

    if (m_passes_rendered >= m_max_passes || abort_switch.is_aborted())
    {
        const float variance = m_framebuffer->estimator_variance();
        RENDERER_LOG_INFO("Final iteration variance estimate: %s", pretty_scalar(variance, 7).c_str());

        if (m_params.m_iteration_progression == IterationProgression::Combine)
        {
            image_to_buffer(frame.image(), 1.0f / variance);
            combine_iterations(frame);
        }

        return true;
    }

    if (m_passes_left_curr_iter == 0)
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

        if (m_params.m_iteration_progression == IterationProgression::Automatic && samples_rendered > 256 &&
           current_extraplolated_variance > m_last_extrapolated_variance)
        {
            RENDERER_LOG_INFO("Extrapolated variance is increasing, initiating final iteration");
            m_var_increase = m_is_final_iter = true;
        }

        m_last_extrapolated_variance = current_extraplolated_variance;

        if (m_params.m_iteration_progression == IterationProgression::Combine)
            image_to_buffer(frame.image(), 1.0f / variance);
    }

    return false;
}

void GPTPassCallback::set_framebuffer(
    VarianceTrackingShadingResultFrameBufferFactory* framebuffer)
{
    m_framebuffer = framebuffer;
}

void GPTPassCallback::image_to_buffer(
    const Image& image,
    const float  inverse_variance)
{
    // Store rendered images.
    if (m_image_buffer.size() == ImageBufferCapacity)
    {
        m_image_buffer.pop_front();
        m_inverse_variance_buffer.pop_front();
    }
    m_image_buffer.push_back(image);
    m_inverse_variance_buffer.push_back(inverse_variance);
}

// Inverse variance weighted sample combination [Müller 2019]
void GPTPassCallback::combine_iterations(const Frame& frame)
{
    assert(m_image_buffer.size() > 0 && m_inverse_variance_buffer.size() == m_image_buffer.size());

    // No need to weight single image.
    if (m_image_buffer.size() == 1)
        return;

    float total_inverse_variances = 0.0f;

    for (const float& var : m_inverse_variance_buffer)
    {
        total_inverse_variances += var;
    }

    // If no variance was detected use the last image as is.
    if (total_inverse_variances <= 0.0f)
        return;

    // Calculate the weights.
    std::string weight_str = "[";

    for (float& var : m_inverse_variance_buffer)
    {
        var /= total_inverse_variances;
        weight_str += pretty_scalar(var, 3) + ", ";
    }
    weight_str = weight_str.substr(0, weight_str.size() - 2) + "]"; // subtract last comma

    RENDERER_LOG_INFO(
        "Applying inverse variance weighted sample combination on last %s images with weights %s",
        pretty_uint(m_image_buffer.size()).c_str(),
        weight_str.c_str());

    CanvasProperties image_properties = frame.image().properties();

    // Arrays to store pixel channels.
    std::unique_ptr<float> final_pixel(new float[image_properties.m_channel_count]);
    std::unique_ptr<float> temp_pixel(new float[image_properties.m_channel_count]);

    for (size_t y = 0; y < image_properties.m_canvas_height; ++y)
        for (size_t x = 0; x < image_properties.m_canvas_width; ++x)
        {
            auto image_itr = m_image_buffer.cbegin();
            auto weight_itr = m_inverse_variance_buffer.cbegin();

            // Clear the final pixel receiver.
            float* channel = final_pixel.get();
            for (size_t c = 0; c < image_properties.m_channel_count; ++c)
                channel[c] = 0.0f;

            // Iterate over the stored images.
            for (size_t i = 0; i < m_image_buffer.size(); ++i)
            {
                // Get image's pixel data.
                image_itr->get_pixel(x, y, temp_pixel.get(), image_properties.m_channel_count);

                // Add inverse variance weighted channels to final pixel.
                float* final_channel = final_pixel.get();
                float* temp_channel = temp_pixel.get();
                for (size_t c = 0; c < image_properties.m_channel_count; ++c)
                    final_channel[c] += temp_channel[c] * (*weight_itr);

                ++image_itr;
                ++weight_itr;
            }

            // Set the final pixel in the frame.
            frame.image().set_pixel(x, y, final_pixel.get(), image_properties.m_channel_count);
        }
}

}   // namespace renderer