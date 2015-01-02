
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "pixelsampler.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

using namespace foundation;

namespace renderer
{

//
// PixelSampler class implementation.
//

void PixelSampler::initialize(const size_t subpixel_grid_size)
{
    m_subpixel_grid_size = subpixel_grid_size;
    m_rcp_subpixel_grid_size = 1.0 / subpixel_grid_size;

    const size_t subpixel_count = subpixel_grid_size * subpixel_grid_size;
    m_log_period = int_log2(subpixel_count * 32);

    if (m_log_period > 16)
        m_log_period = 16;

    m_period = size_t(1) << m_log_period;
    m_sigma.resize(m_period);

    // Precompute the first N values of 2^N * radical_inverse_base2(0..N-1).
    for (size_t i = 0; i < m_period; ++i)
    {
        size_t b = m_period;
        size_t x = 0;

        for (size_t n = i; n; n >>= 1)
        {
            b >>= 1;
            x += (n & 1) * b;
        }

        m_sigma[i] = x;
    }

    m_rcp_period = 1.0 / m_period;
    m_period_mask = m_period - 1;
}

}   // namespace renderer
