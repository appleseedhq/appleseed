
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

// Interface header.
#include "mipgen.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>

namespace
{
    template <typename T>
    inline bool is_pow2(const T x)
    {
        return (x & (x - 1)) == 0;
    }

    inline float mitchell_netravali_filter(float x, const float sharpness)
    {
        x += x;

        const float xabs = fabs(x);

        if (xabs > 2.0f)
            return 0.0f;

        const float b = 1.0f - sharpness;
        const float c = 0.5f * sharpness;
        const float xx = x * x;

        if (xabs < 1.0f)
        {
            return (1.0f / 6.0f) *
                   (xabs * xx * (12.0f - 9.0f * b - 6.0f * c) +
                    xx * (-18.0f + 12.0f * b + 6.0f * c) +
                    (6.0f - 2.0f * b));
        }
        else
        {
            return (1.0f / 6.0f) *
                   (xabs * xx * (-b - 6.0f * c) +
                    xx * (6.0f * b + 30.0f * c) +
                    xabs * (-12.0f * b - 48.0f * c) +
                    (8.0f * b + 24.0f * c));
        }
    }
}

void generate_mipmap_level(
    float* __restrict           output,
    const float* __restrict     input,
    const int                   input_width,
    const int                   input_height,
    const int                   input_channels,
    const int                   level,
    const int                   filter_radius,
    const float                 filter_sharpness)
{
    assert(output);
    assert(input);
    assert(input_width > 0);
    assert(input_height > 0);
    assert(is_pow2(input_width));
    assert(is_pow2(input_height));
    assert(input_channels > 0);
    assert(level > 0);

    const int output_width = std::max(1, input_width >> level);
    const int output_height = std::max(1, input_height >> level);
    const int half_size = 1 << (level - 1);

    for (int oy = 0; oy < output_height; ++oy)
    {
        for (int ox = 0; ox < output_width; ++ox)
        {
            const int cx = (ox << level) + half_size;
            const int cy = (oy << level) + half_size;

            for (int c = 0; c < input_channels; ++c)
                output[(oy * output_width + ox) * input_channels + c] = 0.0f;

            float weight = 0.0f;

            for (int wy = -filter_radius; wy < filter_radius; ++wy)
            {
                for (int wx = -filter_radius; wx < filter_radius; ++wx)
                {
                    const int ix = cx + wx;
                    const int iy = cy + wy;

                    if (ix < 0 || iy < 0 || ix >= input_width || iy >= input_height)
                        continue;

                    const float dx = wx + 0.5f;
                    const float dy = wy + 0.5f;
                    const float r2 = dx * dx + dy * dy;
                    const float r = std::sqrt(r2) / filter_radius;
                    const float w = mitchell_netravali_filter(r, filter_sharpness);

                    if (w == 0.0f)
                        continue;

                    for (int c = 0; c < input_channels; ++c)
                    {
                        output[(oy * output_width + ox) * input_channels + c] +=
                            w * input[(iy * input_width + ix) * input_channels + c];
                    }

                    weight += w;
                }
            }

            if (weight != 0.0f)
            {
                const float rcp_weight = 1.0f / weight;

                for (int c = 0; c < input_channels; ++c)
                {
                    float& component = output[(oy * output_width + ox) * input_channels + c];
                    component *= rcp_weight;
                    if (component < 0.0f)
                        component = 0.0f;
                }
            }
        }
    }
}
