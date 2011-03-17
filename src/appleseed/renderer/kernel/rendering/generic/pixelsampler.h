
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_PIXELSAMPLER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_PIXELSAMPLER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// Standard headers.
#include <vector>

namespace renderer
{

//
// A strictly deterministic pixel sampler.
//
// Reference:
//
//   Strictly Deterministic Sampling Methods in Computer Graphics 
//   http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.88.7937
//

class PixelSampler
{
  public:
    // Initialize the pixel sampler for a given number of samples per pixel.
    // Note that the input to this method is actually the square root of
    // the number of samples per pixel.
    void initialize(const size_t sqrt_sample_count);

    // Compute the position of a pixel sample and the initial instance number
    // of the corresponding sampling context, given the integer coordinates
    // of the subpixel grid cell containing the sample. The coordinates of the
    // pixel sample are expressed in sample space (http://appleseedhq.net/conventions).
    void sample(
        const size_t            sx,
        const size_t            sy,
        foundation::Vector2d&   sample_position,
        size_t&                 initial_instance);

  private:
    double                      m_rcp_sqrt_sample_count;    // 1.0 / sqrt_sample_count
    double                      m_rcp_period;               // 1.0 / m_period
    size_t                      m_log_period;               // log2(m_period)
    size_t                      m_period;                   // m_sigma.size()
    size_t                      m_period_mask;              // m_period - 1
    std::vector<size_t>         m_sigma;                    // 2^N * radical_inverse_base2(0..N-1)
};


//
// PixelSampler class implementation.
//

FORCE_INLINE void PixelSampler::sample(
    const size_t                sx,
    const size_t                sy,
    foundation::Vector2d&       sample_position,
    size_t&                     initial_instance)
{
    // Compute the initial instance number of the sampling context.
    const size_t j = sx & m_period_mask;
    const size_t k = sy & m_period_mask;
    const size_t sigma_j = m_sigma[j];
    const size_t sigma_k = m_sigma[k];
    initial_instance = (j << m_log_period) + sigma_k;

    // Compute the sample coordinates in image space.
    sample_position[0] = (sx + sigma_k * m_rcp_period) * m_rcp_sqrt_sample_count;
    sample_position[1] = (sy + sigma_j * m_rcp_period) * m_rcp_sqrt_sample_count;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_PIXELSAMPLER_H
