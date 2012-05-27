
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune
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

#include "mipmap.h"

namespace ak
{
    void generate_mipmap_level_float_clamp_linear_rgba(
        float* __restrict           output,
        const float* __restrict     input,
        const int                   input_width,
        const int                   input_height,
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
        assert(level > 0);
        assert(filter_radius > 0);
        assert(filter_sharpness >= 0.0f && filter_sharpness <= 1.0f);

        const int output_width = std::max(1, input_width >> level);
        const int output_height = std::max(1, input_height >> level);
        const int half_size = 1 << (level - 1);
        const float rcp_filter_radius = 1.0f / filter_radius;

        for (int oy = 0; oy < output_height; ++oy)
        {
            for (int ox = 0; ox < output_width; ++ox)
            {
                const int cx = (ox << level) + half_size;
                const int cy = (oy << level) + half_size;

                for (int c = 0; c < 4; ++c)
                    output[(oy * output_width + ox) * 4 + c] = 0.0f;

                float weight = 0.0f;

                for (int wy = -filter_radius; wy < filter_radius; ++wy)
                {
                    for (int wx = -filter_radius; wx < filter_radius; ++wx)
                    {
                        const float dx = static_cast<float>(wx) + 0.5f;
                        const float dy = static_cast<float>(wy) + 0.5f;
                        const float r2 = dx * dx + dy * dy;
                        const float r = std::sqrt(r2) * rcp_filter_radius;
                        const float w = details::mitchell_netravali_filter(r, filter_sharpness);

                        if (w == 0.0f)
                            continue;

                        int ix = cx + wx;
                        int iy = cy + wy;

                        if (ix < 0) ix = 0;
                        if (iy < 0) iy = 0;
                        if (ix > input_width - 1) ix = input_width - 1;
                        if (iy > input_height - 1) iy = input_height - 1;

                        for (int c = 0; c < 4; ++c)
                        {
                            float val = input[(iy * input_width + ix) * 4 + c];
                            output[(oy * output_width + ox) * 4 + c] += w * val;
                        }

                        weight += w;
                    }
                }

                if (weight != 0.0f)
                {
                    const float rcp_weight = 1.0f / weight;

                    for (int c = 0; c < 4; ++c)
                    {
                        float val = output[(oy * output_width + ox) * 4 + c];

                        val *= rcp_weight;

                        if (val < 0.0f)
                            val = 0.0f;

                        output[(oy * output_width + ox) * 4 + c] = val;
                    }
                }
            }
        }
    }
}
