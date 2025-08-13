
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

#pragma once

// appleseed.renderer headers.
#include "renderer/kernel/lighting/gpt/gptparameters.h"
#include "renderer/kernel/lighting/sdtree.h"
#include "renderer/kernel/rendering/ipasscallback.h"
#include "renderer/kernel/rendering/variancetrackingshadingresultframebufferfactory.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"

// Standard headers.
#include <list>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class JobQueue; }
namespace renderer      { class Frame; }

namespace renderer
{

//
// This class is responsible for the path guiding budget balancing logic.
//

class GPTPassCallback
  : public IPassCallback
{
  public:
    // Constructor.
    GPTPassCallback(
        const GPTParameters&                             params,
        STree*                                           sd_tree,
        const size_t                                     sample_budget,
        const size_t                                     max_passes);

    // Delete this instance.
    void release() override;

    // This method is called at the beginning of a pass.
    void on_pass_begin(
        const Frame&                                     frame,
        foundation::JobQueue&                            job_queue,
        foundation::IAbortSwitch&                        abort_switch) override;

    // This method is called at the end of a pass.
    bool on_pass_end(
        const Frame&                                     frame,
        foundation::JobQueue&                            job_queue,
        foundation::IAbortSwitch&                        abort_switch) override;

    void set_framebuffer(
        VarianceTrackingShadingResultFrameBufferFactory* framebuffer);

  private:
    void image_to_buffer(
        const foundation::Image&                         image,
        const float                                      inverse_variance);

    void combine_iterations(const Frame&                 frame);

    const GPTParameters                                  m_params;
    size_t                                               m_iter;
    size_t                                               m_max_passes;
    size_t                                               m_passes_rendered;
    size_t                                               m_remaining_passes;
    size_t                                               m_passes_left_curr_iter;
    size_t                                               m_num_passes_curr_iter;
    STree*                                               m_sd_tree;
    size_t                                               m_sample_budget;
    float                                                m_last_extrapolated_variance;
    bool                                                 m_is_final_iter;
    bool                                                 m_var_increase;
    VarianceTrackingShadingResultFrameBufferFactory*     m_framebuffer;

    std::list<foundation::Image>                         m_image_buffer;
    std::list<float>                                     m_inverse_variance_buffer;
};

}   // namespace renderer
