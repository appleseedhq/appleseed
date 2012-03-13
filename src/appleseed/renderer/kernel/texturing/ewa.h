
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

#include <cassert>
#include <cmath>
#include <cstring>

//---------------------------------------------------------------------------------------------
// A qualifier to specify the alignment of a variable, a structure member or a structure.
//---------------------------------------------------------------------------------------------

// Visual C++.
#if defined _MSC_VER
    #define ALIGN(n) __declspec(align(n))

// gcc.
#elif defined __GNUC__
    #define ALIGN(n) __attribute__((aligned(n)))

// Other compilers: ignore the qualifier.
#else
    #define ALIGN(n)
#endif

// Specify an alignment compatible with SSE.
#define SSE_ALIGN ALIGN(16)

//---------------------------------------------------------------------------------------------
// EWA filter implementation for AtomKraft.
// http://www.cs.cmu.edu/~ph/texfund/texfund.pdf
//---------------------------------------------------------------------------------------------

template <int NumChannels, typename Texture>
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
        const Texture&  texture,
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
        const int min_x = static_cast<int>(center_x - ku);
        const int min_y = static_cast<int>(center_y - kv);
        const int max_x = static_cast<int>(std::ceil(center_x + ku));
        const int max_y = static_cast<int>(std::ceil(center_y + kv));

        std::memset(result, 0, NumChannels * sizeof(float));
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
                    SSE_ALIGN float texel[NumChannels];
                    texture.get(x, y, texel);

                    const float w = m_weights[q <= 0.0f ? 0 : static_cast<size_t>(q)];

                    for (int c = 0; c < NumChannels; ++c)
                        result[c] += w * texel[c];

                    den += w;
                }

                q += dq;
                dq += Ddq;
            }
        }

        assert(den > 0.0f);

        const float rcp_den = 1.0f / den;

        for (int c = 0; c < NumChannels; ++c)
            result[c] *= rcp_den;
    }

  private:
    enum { WeightCount = 256 };
    float m_weights[WeightCount];
};

#endif
