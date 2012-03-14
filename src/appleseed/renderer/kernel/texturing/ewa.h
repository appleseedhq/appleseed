
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
#include <xmmintrin.h>

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
//
// References:
//
//   http://www.cs.cmu.edu/~ph/texfund/texfund.pdf
//   http://www.pmavridis.com/data/I3D11_EllipticalFiltering.pdf
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
    void filter(
        const Texture&  texture,
        const float     center_x,
        const float     center_y,
        const float     dudx,
        const float     dudy,
        const float     dvdx,
        const float     dvdy,
        const float     max_radius,
        float           result[])
    {
        // Compute the coefficients of the original ellipse.
        float a = dvdx * dvdx + dvdy * dvdy + 1.0f;
        float b = -2.0f * (dudx * dvdx + dudy * dvdy);
        float c = dudx * dudx + dudy * dudy + 1.0f;

        // Rescale the coefficients so that F = WeightCount.
        const float F = static_cast<float>(WeightCount);
        const float k = F / (a * c - b * b * 0.25f);
        a *= k;
        b *= k;
        c *= k;

        // Compute the coefficients of the orthogonal ellipse.
        const float r_2 = (a - c) * (a - c) + b * b;
        const float r = std::sqrt(r_2);
        const float a_prime = (a + c + r) * 0.5f;
        const float c_prime = (a + c - r) * 0.5f;

        // Compute the radii of the ellipse.
        float r1 = std::sqrt(F / a_prime);
        float r2 = std::sqrt(F / c_prime);

        // Bound the amount of work by clamping the radii the ellipse.
        if (r1 > max_radius || r2 > max_radius)
        {
            // Clamp the radii.
            if (r1 > max_radius) r1 = max_radius;
            if (r2 > max_radius) r2 = max_radius;

            // Compute the angle of the original ellipse.
            const float theta = 0.5f * std::atan(b / (a - c));

            // Compute the coefficients of the new ellipse.
            const float r1_2 = r1 * r1;
            const float r2_2 = r2 * r2;
            const float cos_theta = std::cos(theta);
            const float cos_theta_2 = cos_theta * cos_theta;
            const float sin_theta_2 = 1.0f - cos_theta_2;
            a = r1_2 * cos_theta_2 + r2_2 * sin_theta_2;
            b = (r2_2 - r1_2) * std::sin(theta + theta);
            c = r1_2 * sin_theta_2 + r2_2 * cos_theta_2;

            // Rescale the coefficients so that F = WeightCount.
            const float k = F / (r1_2 * r2_2);
            a *= k;
            b *= k;
            c *= k;
        }

        // Compute the bounding box of the ellipse.
        const float half_width = 2.0f * c * std::sqrt(F / (4.0f * a * c * c - c * b * b));
        const float half_height = 2.0f * a * std::sqrt(F / (4.0f * a * a * c - a * b * b));
        const int min_x = truncate<int>(center_x - half_width);
        const int min_y = truncate<int>(center_y - half_height);
        const int max_x = truncate<int>(std::ceil(center_x + half_width));
        const int max_y = truncate<int>(std::ceil(center_y + half_height));

        std::memset(result, 0, NumChannels * sizeof(float));
        float den = 0.0f;

        const float u = (min_x + 0.5f) - center_x;
        const float ddq = 2.0f * a;

        for (int y = min_y; y < max_y; ++y)
        {
            const float v = (y + 0.5f) - center_y;
            float dq = a * (2.0f * u + 1.0f) + b * v;
            float q = (c * v + b * u) * v + a * u * u;

            for (int x = min_x; x < max_x; ++x)
            {
                if (q < F)
                {
                    SSE_ALIGN float texel[NumChannels];
                    texture.get(x, y, texel);

                    const float w = m_weights[q <= 0.0f ? 0 : truncate<size_t>(q)];

                    for (int c = 0; c < NumChannels; ++c)
                        result[c] += w * texel[c];

                    den += w;
                }

                q += dq;
                dq += ddq;
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

    // Fast floating point-to-integer truncation using SSE. Equivalent to static_cast<Int>(x).
    template <typename Int>
    static Int truncate(const float x)
    {
        return static_cast<Int>(_mm_cvttss_si32(_mm_load_ss(&x)));
    }
};

#endif
