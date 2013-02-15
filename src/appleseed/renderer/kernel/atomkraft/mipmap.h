
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef MIPMAP_H
#define MIPMAP_H

#include "common.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstring>

namespace ak
{

//---------------------------------------------------------------------------------------------
// Fast mipmap generation for AtomKraft.
//---------------------------------------------------------------------------------------------

template <int NumChannels, typename TiledTexture>
void generate_mipmap_level(
    TiledTexture&               output,
    const TiledTexture&         input,
    const int                   level,
    const int                   tile_x,
    const int                   tile_y,
    const int                   filter_radius = 2,
    const float                 filter_sharpness = 0.5f);

template <typename TiledTexture>
void generate_mipmap_level_float_clamp_linear_rgba(
    TiledTexture&               output,
    const TiledTexture&         input,
    const int                   level,
    const int                   tile_x,
    const int                   tile_y,
    const int                   filter_radius = 2,
    const float                 filter_sharpness = 0.5f);

//---------------------------------------------------------------------------------------------
// Implementation.
//---------------------------------------------------------------------------------------------

namespace details
{
    inline float mitchell_netravali_filter(float x, const float sharpness)
    {
        x += x;

        const float xabs = std::abs(x);

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

template <int NumChannels, typename TiledTexture>
void generate_mipmap_level(
    TiledTexture&               output,
    const TiledTexture&         input,
    const int                   level,
    const int                   tile_x,
    const int                   tile_y,
    const int                   filter_radius,
    const float                 filter_sharpness)
{
    assert(level > 0);
    assert(tile_x >= 0 && tile_x < output.tile_count_x());
    assert(tile_y >= 0 && tile_y < output.tile_count_y());
    assert(filter_radius > 0);
    assert(filter_sharpness >= 0.0f && filter_sharpness <= 1.0f);

    const int half_size = 1 << (level - 1);
    const float rcp_filter_radius = 1.0f / filter_radius;

    typename TiledTexture::TileType& output_tile = output.tile(tile_x, tile_y);
    const int output_tile_width = output_tile.width();
    const int output_tile_height = output_tile.height();

    for (int tile_opy = 0; tile_opy < output_tile_height; ++tile_opy)
    {
        for (int tile_opx = 0; tile_opx < output_tile_width; ++tile_opx)
        {
            SSE_ALIGN float output_texel[NumChannels];
            std::memset(output_texel, 0, NumChannels * sizeof(float));

            float weight = 0.0f;

            const int image_opx = tile_x * output.tile_width() + tile_opx;
            const int image_opy = tile_y * output.tile_height() + tile_opy;

            const int image_ipx = (image_opx << level) + half_size;
            const int image_ipy = (image_opy << level) + half_size;

            const int image_ipx_min = image_ipx - filter_radius;
            const int image_ipy_min = image_ipy - filter_radius;
            const int image_ipx_max = image_ipx + filter_radius - 1;
            const int image_ipy_max = image_ipy + filter_radius - 1;

            const int itx_min = std::max(image_ipx_min / input.tile_width(), 0);
            const int ity_min = std::max(image_ipy_min / input.tile_height(), 0);
            const int itx_max = std::min(image_ipx_max / input.tile_width(), input.tile_count_x() - 1);
            const int ity_max = std::min(image_ipy_max / input.tile_height(), input.tile_count_y() - 1);

            for (int ity = ity_min; ity <= ity_max; ++ity)
            {
                for (int itx = itx_min; itx <= itx_max; ++itx)
                {
                    const typename TiledTexture::TileType& input_tile = input.tile(itx, ity);

                    const int tile_ipx_origin = itx * input.tile_width();
                    const int tile_ipy_origin = ity * input.tile_height();

                    const int raw_tile_ipx_min = image_ipx_min - tile_ipx_origin;
                    const int raw_tile_ipy_min = image_ipy_min - tile_ipy_origin;
                    const int raw_tile_ipx_max = image_ipx_max - tile_ipx_origin;
                    const int raw_tile_ipy_max = image_ipy_max - tile_ipy_origin;

                    const int tile_ipx_min = itx == 0 ? raw_tile_ipx_min : std::max(raw_tile_ipx_min, 0);
                    const int tile_ipy_min = ity == 0 ? raw_tile_ipy_min : std::max(raw_tile_ipy_min, 0);
                    const int tile_ipx_max = itx == input.tile_count_x() - 1 ? raw_tile_ipx_max : std::min(raw_tile_ipx_max, input_tile.width());
                    const int tile_ipy_max = ity == input.tile_count_y() - 1 ? raw_tile_ipy_max : std::min(raw_tile_ipy_max, input_tile.height());

                    for (int ipy = tile_ipy_min; ipy < tile_ipy_max; ++ipy)
                    {
                        for (int ipx = tile_ipx_min; ipx < tile_ipx_max; ++ipx)
                        {
                            const float dx = static_cast<float>(tile_ipx_origin + ipx - image_ipx) + 0.5f;
                            const float dy = static_cast<float>(tile_ipy_origin + ipy - image_ipy) + 0.5f;
                            const float r2 = dx * dx + dy * dy;
                            const float r = std::sqrt(r2) * rcp_filter_radius;
                            const float w = details::mitchell_netravali_filter(r, filter_sharpness);

                            if (w == 0.0f)
                                continue;

                            SSE_ALIGN float input_texel[NumChannels];
                            input_tile.get(ipx, ipy, input_texel);

                            for (int c = 0; c < NumChannels; ++c)
                                output_texel[c] += w * input_texel[c];

                            weight += w;
                        }
                    }
                }
            }

            if (weight != 0.0f)
            {
                const float rcp_weight = 1.0f / weight;

                for (int c = 0; c < NumChannels; ++c)
                {
                    output_texel[c] *= rcp_weight;

                    if (output_texel[c] < 0.0f)
                        output_texel[c] = 0.0f;
                }

                output_tile.put(tile_opx, tile_opy, output_texel);
            }
        }
    }
}

template <typename TiledTexture>
void generate_mipmap_level_float_clamp_linear_rgba(
    TiledTexture&               output,
    const TiledTexture&         input,
    const int                   level,
    const int                   tile_x,
    const int                   tile_y,
    const int                   filter_radius,
    const float                 filter_sharpness)
{
    assert(level > 0);
    assert(tile_x >= 0 && tile_x < output.tile_count_x());
    assert(tile_y >= 0 && tile_y < output.tile_count_y());
    assert(filter_radius > 0);
    assert(filter_sharpness >= 0.0f && filter_sharpness <= 1.0f);

    const int half_size = 1 << (level - 1);
    const float rcp_filter_radius = 1.0f / filter_radius;

    typename TiledTexture::TileType& output_tile = output.tile(tile_x, tile_y);
    const int output_tile_width = output_tile.width();
    const int output_tile_height = output_tile.height();
    float* output_tile_pixels = static_cast<float*>(output_tile.pixels());

    for (int tile_opy = 0; tile_opy < output_tile_height; ++tile_opy)
    {
        for (int tile_opx = 0; tile_opx < output_tile_width; ++tile_opx)
        {
            float* output_texel = output_tile_pixels + (tile_opy * output_tile_width + tile_opx) * 4;

            for (int c = 0; c < 4; ++c)
                output_texel[c] = 0.0f;

            float weight = 0.0f;

            const int image_opx = tile_x * output.tile_width() + tile_opx;
            const int image_opy = tile_y * output.tile_height() + tile_opy;

            const int image_ipx = (image_opx << level) + half_size;
            const int image_ipy = (image_opy << level) + half_size;

            const int image_ipx_min = image_ipx - filter_radius;
            const int image_ipy_min = image_ipy - filter_radius;
            const int image_ipx_max = image_ipx + filter_radius - 1;
            const int image_ipy_max = image_ipy + filter_radius - 1;

            const int itx_min = std::max(image_ipx_min / input.tile_width(), 0);
            const int ity_min = std::max(image_ipy_min / input.tile_height(), 0);
            const int itx_max = std::min(image_ipx_max / input.tile_width(), input.tile_count_x() - 1);
            const int ity_max = std::min(image_ipy_max / input.tile_height(), input.tile_count_y() - 1);

            for (int ity = ity_min; ity <= ity_max; ++ity)
            {
                for (int itx = itx_min; itx <= itx_max; ++itx)
                {
                    const typename TiledTexture::TileType& input_tile = input.tile(itx, ity);
                    const int input_tile_width = input_tile.width();
                    const int input_tile_height = input_tile.height();

                    const int tile_ipx_origin = itx * input.tile_width();
                    const int tile_ipy_origin = ity * input.tile_height();

                    const int raw_tile_ipx_min = image_ipx_min - tile_ipx_origin;
                    const int raw_tile_ipy_min = image_ipy_min - tile_ipy_origin;
                    const int raw_tile_ipx_max = image_ipx_max - tile_ipx_origin;
                    const int raw_tile_ipy_max = image_ipy_max - tile_ipy_origin;

                    const int tile_ipx_min = itx == 0 ? raw_tile_ipx_min : std::max(raw_tile_ipx_min, 0);
                    const int tile_ipy_min = ity == 0 ? raw_tile_ipy_min : std::max(raw_tile_ipy_min, 0);
                    const int tile_ipx_max = itx == input.tile_count_x() - 1 ? raw_tile_ipx_max : std::min(raw_tile_ipx_max, input_tile_width);
                    const int tile_ipy_max = ity == input.tile_count_y() - 1 ? raw_tile_ipy_max : std::min(raw_tile_ipy_max, input_tile_height);

                    for (int ipy = tile_ipy_min; ipy < tile_ipy_max; ++ipy)
                    {
                        for (int ipx = tile_ipx_min; ipx < tile_ipx_max; ++ipx)
                        {
                            const float dx = static_cast<float>(tile_ipx_origin + ipx - image_ipx) + 0.5f;
                            const float dy = static_cast<float>(tile_ipy_origin + ipy - image_ipy) + 0.5f;
                            const float r2 = dx * dx + dy * dy;
                            const float r = std::sqrt(r2) * rcp_filter_radius;
                            const float w = details::mitchell_netravali_filter(r, filter_sharpness);

                            if (w == 0.0f)
                                continue;

                            const int clamped_ipx = ipx < 0 ? 0 : ipx > input_tile_width - 1 ? input_tile_width - 1 : ipx;
                            const int clamped_ipy = ipy < 0 ? 0 : ipy > input_tile_height - 1 ? input_tile_height - 1 : ipy;

                            const float* input_texel =
                                static_cast<const float*>(input_tile.pixels()) + (clamped_ipy * input_tile_width + clamped_ipx) * 4;

                            for (int c = 0; c < 4; ++c)
                                output_texel[c] += w * input_texel[c];

                            weight += w;
                        }
                    }
                }
            }

            if (weight != 0.0f)
            {
                const float rcp_weight = 1.0f / weight;

                for (int c = 0; c < 4; ++c)
                {
                    float val = output_texel[c];

                    val *= rcp_weight;

                    if (val < 0.0f)
                        val = 0.0f;

                    output_texel[c] = val;
                }
            }
        }
    }
}

}   // namespace ak

#endif
