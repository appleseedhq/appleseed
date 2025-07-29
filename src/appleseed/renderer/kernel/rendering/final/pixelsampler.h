
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

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>
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
  : public foundation::NonCopyable
{
  public:
    // Initialize the pixel sampler for a given subpixel grid size.
    void initialize(const size_t subpixel_grid_size);

    // Compute the position of a pixel sample and optionally the initial instance
    // number of the corresponding sampling context, given the integer coordinates
    // of the subpixel grid cell containing the sample. The coordinates of the
    // pixel sample are expressed in continuous image space
    // (https://github.com/appleseedhq/appleseed/wiki/Terminology).
    void sample(
        const int               sx,
        const int               sy,
        foundation::Vector2d&   sample_position) const;
    void sample(
        const int               sx,
        const int               sy,
        foundation::Vector2d&   sample_position,
        size_t&                 initial_instance) const;

  private:
    size_t                      m_subpixel_grid_size;
    double                      m_rcp_subpixel_grid_size;   // 1.0 / subpixel_grid_size
    double                      m_rcp_period;               // 1.0 / m_period
    size_t                      m_log_period;               // log2(m_period)
    size_t                      m_period;                   // m_sigma.size()
    size_t                      m_period_mask;              // m_period - 1
    std::vector<size_t>         m_sigma;                    // 2^N * radical_inverse_base2(0..N-1)
};


//
// PixelSampler class implementation.
//

APPLESEED_FORCE_INLINE void PixelSampler::sample(
    const int                   sx,
    const int                   sy,
    foundation::Vector2d&       sample_position) const
{
    // Compute the sample coordinates in image space.
    if (m_subpixel_grid_size == 1)
    {
        sample_position[0] = sx + 0.5;
        sample_position[1] = sy + 0.5;
    }
    else
    {
        const size_t j = sx & m_period_mask;
        const size_t k = sy & m_period_mask;
        const size_t sigma_j = m_sigma[j];
        const size_t sigma_k = m_sigma[k];

        sample_position[0] = (sx + sigma_k * m_rcp_period) * m_rcp_subpixel_grid_size;
        sample_position[1] = (sy + sigma_j * m_rcp_period) * m_rcp_subpixel_grid_size;
    }
}

APPLESEED_FORCE_INLINE void PixelSampler::sample(
    const int                   sx,
    const int                   sy,
    foundation::Vector2d&       sample_position,
    size_t&                     initial_instance) const
{
    const size_t j = sx & m_period_mask;
    const size_t k = sy & m_period_mask;
    const size_t sigma_k = m_sigma[k];

    // Compute the initial instance number of the sampling context.
    initial_instance = (j << m_log_period) + sigma_k;

    // Compute the sample coordinates in image space.
    if (m_subpixel_grid_size == 1)
    {
        sample_position[0] = sx + 0.5;
        sample_position[1] = sy + 0.5;
    }
    else
    {
        const size_t sigma_j = m_sigma[j];

        sample_position[0] = (sx + sigma_k * m_rcp_period) * m_rcp_subpixel_grid_size;
        sample_position[1] = (sy + sigma_j * m_rcp_period) * m_rcp_subpixel_grid_size;
    }
}

}   // namespace renderer
