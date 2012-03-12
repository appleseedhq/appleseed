
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

#ifndef EWA_H
#define EWA_H

// todo: remove all appleseed dependencies.
#include "foundation/math/fastmath.h"
#include "foundation/platform/compiler.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstring>

//---------------------------------------------------------------------------------------------
// EWA filter implementation for AtomKraft.
// http://www.cs.cmu.edu/~ph/texfund/texfund.pdf
//---------------------------------------------------------------------------------------------

class EWAFilterAK
{
  public:
    EWAFilterAK()
    {
        for (int i = 0; i < WeightCount; ++i)
        {
            const float Alpha = 2.0f;
            const float q = static_cast<float>(i) / (WeightCount - 1);
            m_weights[i] = std::exp(-Alpha * q);
        }
    }

    // Coordinates are expressed in [0,texture_width)x[0,texture_height) (note: open on the right).
    void filter_ellipse(
        const float     texture[],
        const int       texture_width,
        const int       texture_height,
        const int       texture_channels,
        const float     texture_gamma,
        const float     center_x,
        const float     center_y,
        const float     dudx,
        const float     dudy,
        const float     dvdx,
        const float     dvdy,
        float           result[])
    {
        // Compute the inclusion threshold.
        const float F = static_cast<float>(WeightCount);

        // Compute the ellipse coefficients.
        float A = dvdx * dvdx + dvdy * dvdy + 1.0f;
        float B = -2.0f * (dudx * dvdx + dudy * dvdy);
        float C = dudx * dudx + dudy * dudy + 1.0f;
        const float K = F / (A * C - B * B * 0.25f);
        A *= K;
        B *= K;
        C *= K;

        // Compute the bounding box of the ellipse.
        const float ku = 2.0f * C * sqrt(F / (4.0f * A * C * C - C * B * B));
        const float kv = 2.0f * A * sqrt(F / (4.0f * A * A * C - A * B * B));
        const int min_x = std::max(static_cast<int>(center_x - ku), 0);
        const int min_y = std::max(static_cast<int>(center_y - kv), 0);
        const int max_x = std::min(static_cast<int>(std::ceil(center_x + ku)), texture_width);
        const int max_y = std::min(static_cast<int>(std::ceil(center_y + kv)), texture_height);

        std::memset(result, 0, texture_channels * sizeof(float));
        float den = 0.0f;

        const float u = (min_x + 0.5f) - center_x;
        const float Ddq = 2.0f * A;

        for (int y = min_y; y < max_y; ++y)
        {
            const float v = (y + 0.5f) - center_y;
            float dq = A * (2.0f * u + 1.0f) + B * v;
            float q = (C * v + B * u) * v + A * u * u;

            for (int x = min_x; x < max_x; ++x)
            {
                if (q < F)
                {
                    const float w = m_weights[q <= 0.0f ? 0 : static_cast<size_t>(q)];
                    const int texture_index = (y * texture_width + x) * texture_channels;

                    if (texture_gamma != 1.0f)
                    {
                        for (int c = 0; c < texture_channels; ++c)
                            m_scratch[c] = texture[texture_index + c];

                        const int blocks = texture_channels / 4;

                        for (int b = 0; b < blocks; ++b)
                            foundation::fast_pow_refined(m_scratch + b * 4, texture_gamma);

                        const int done = blocks * 4;

                        for (int c = done; c < texture_channels; ++c)
                            m_scratch[c] = foundation::fast_pow_refined(m_scratch[c], texture_gamma);

                        for (int c = 0; c < texture_channels; ++c)
                            result[c] += w * m_scratch[c];
                    }
                    else
                    {
                        for (int c = 0; c < texture_channels; ++c)
                            result[c] += w * texture[texture_index + c]; 
                    }

                    den += w;
                }

                q += dq;
                dq += Ddq;
            }
        }

        assert(den > 0.0f);

        const float rcp_den = 1.0f / den;

        for (int c = 0; c < texture_channels; ++c)
            result[c] *= rcp_den;
    }

    // Trapezoid vertices are in [0,texture_width)x[0,texture_height) (note: open on the right).
    void filter_trapezoid(
        const float     texture[],
        const int       texture_width,
        const int       texture_height,
        const int       texture_channels,
        const float     texture_gamma,
        const float     v00x,
        const float     v00y,
        const float     v10x,
        const float     v10y,
        const float     v01x,
        const float     v01y,
        const float     v11x,
        const float     v11y,
        float           result[])
    {
        // Compute the parameters of the inscribed ellipse.
        const float m_00_10_x = 0.5f * (v00x + v10x);
        const float m_00_10_y = 0.5f * (v00y + v10y);
        const float m_01_11_x = 0.5f * (v01x + v11x);
        const float m_01_11_y = 0.5f * (v01y + v11y);
        const float m_10_11_x = 0.5f * (v10x + v11x);
        const float m_10_11_y = 0.5f * (v10y + v11y);
        const float center_x  = 0.5f * (m_00_10_x + m_01_11_x);
        const float center_y  = 0.5f * (m_00_10_y + m_01_11_y);
        const float du_x = m_10_11_x - center_x;
        const float du_y = m_10_11_y - center_y;
        const float dv_x = m_01_11_x - center_x;
        const float dv_y = m_01_11_y - center_y;

        filter_ellipse(
            texture,
            texture_width,
            texture_height,
            texture_channels,
            texture_gamma,
            center_x,
            center_y,
            du_x,
            dv_x,
            du_y,
            dv_y,
            result);
    }

  private:
    enum { WeightCount = 256 };
    enum { MaxChannelCount = 8 };

    float           m_weights[WeightCount];
    SSE_ALIGN float m_scratch[MaxChannelCount];
};

#endif
