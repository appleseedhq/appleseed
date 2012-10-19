
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "sampleaccumulationbuffer.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/kernel/rendering/generic/developedpixelbuffer.h"
#include "renderer/kernel/shading/shadingresult.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/tile.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

SampleAccumulationBuffer::SampleAccumulationBuffer(
    const size_t            width,
    const size_t            height,
    const size_t            aov_count)
  : m_width(width)
  , m_height(height)
  , m_aov_count(aov_count)
  , m_channel_count(1 + 4 + aov_count * 3)
{
    m_buffer.resize(m_width * m_height * m_channel_count, 0.0f);
}

size_t SampleAccumulationBuffer::get_width() const
{
    return m_width;
}

size_t SampleAccumulationBuffer::get_height() const
{
    return m_height;
}

void SampleAccumulationBuffer::add(
    const size_t            x,
    const size_t            y,
    const ShadingResult&    sample,
    const float             weight)
{
    assert(x < m_width);
    assert(y < m_height);
    assert(sample.m_color_space == ColorSpaceLinearRGB);
    assert(sample.m_aovs.size() == m_aov_count);

    float* ptr = &m_buffer[(y * m_width + x) * m_channel_count];

    *ptr++ += weight;

    *ptr++ += sample.m_alpha[0] * weight;

    *ptr++ += sample.m_color[0] * weight;
    *ptr++ += sample.m_color[1] * weight;
    *ptr++ += sample.m_color[2] * weight;

    for (size_t i = 0; i < m_aov_count; ++i)
    {
        *ptr++ += sample.m_aovs[i][0] * weight;
        *ptr++ += sample.m_aovs[i][1] * weight;
        *ptr++ += sample.m_aovs[i][2] * weight;
    }
}

void SampleAccumulationBuffer::develop_pixel(
    const size_t            x,
    const size_t            y,
    DevelopedPixelBuffer&   pixel_buffer) const
{
    assert(x < m_width);
    assert(y < m_height);

    const float* ptr = &m_buffer[(y * m_width + x) * m_channel_count];

    const float weight = *ptr++;
    const float rcp_weight = weight == 0.0f ? 0.0f : 1.0f / weight;

    const float alpha = *ptr++;

    const Color4f color(ptr[0], ptr[1], ptr[2], alpha);
    pixel_buffer.set_pixel_color(x, y, color * rcp_weight);
    ptr += 3;

    for (size_t i = 0; i < m_aov_count; ++i)
    {
        const Color3f aov(ptr[0], ptr[1], ptr[2]);
        pixel_buffer.set_pixel_aov(x, y, i, aov * rcp_weight);
        ptr += 3;
    }
}

void SampleAccumulationBuffer::develop_to_tile_premult_alpha(
    const AABB2u&           bbox,
    Tile&                   tile,
    TileStack&              aov_tiles) const
{
    for (size_t y = bbox.min.y; y <= bbox.max.y; ++y)
    {
        for (size_t x = bbox.min.x; x <= bbox.max.x; ++x)
        {
            const float* ptr = &m_buffer[(y * m_width + x) * m_channel_count];

            const float weight = *ptr++;
            const float rcp_weight = weight == 0.0f ? 0.0f : 1.0f / weight;

            const float alpha = *ptr++;

            const Color4f color(ptr[0], ptr[1], ptr[2], alpha);
            tile.set_pixel(x, y, color * rcp_weight);
            ptr += 3;

            for (size_t i = 0; i < m_aov_count; ++i)
            {
                const Color4f aov(ptr[0], ptr[1], ptr[2], alpha);
                aov_tiles.set_pixel(x, y, i, aov * rcp_weight);
                ptr += 3;
            }
        }
    }
}

void SampleAccumulationBuffer::develop_to_tile_premult_alpha(
    Tile&                   tile,
    TileStack&              aov_tiles) const
{
    develop_to_tile_premult_alpha(
        AABB2u(Vector2u(0, 0), Vector2u(m_width - 1, m_height - 1)),
        tile,
        aov_tiles);
}

void SampleAccumulationBuffer::develop_to_tile_straight_alpha(
    const AABB2u&           bbox,
    Tile&                   tile,
    TileStack&              aov_tiles) const
{
    for (size_t y = bbox.min.y; y <= bbox.max.y; ++y)
    {
        for (size_t x = bbox.min.x; x <= bbox.max.x; ++x)
        {
            const float* ptr = &m_buffer[(y * m_width + x) * m_channel_count];

            const float weight = *ptr++;
            const float rcp_weight = weight == 0.0f ? 0.0f : 1.0f / weight;

            const float alpha = *ptr++;
            const float rcp_alpha = alpha == 0.0f ? 0.0f : 1.0f / alpha;

            Color4f color(ptr[0], ptr[1], ptr[2], alpha);
            color.rgb() *= rcp_alpha;
            color.a *= rcp_weight;
            tile.set_pixel(x, y, color);
            ptr += 3;

            for (size_t i = 0; i < m_aov_count; ++i)
            {
                Color4f aov(ptr[0], ptr[1], ptr[2], alpha);
                aov.rgb() *= rcp_alpha;
                aov.a *= rcp_weight;
                aov_tiles.set_pixel(x, y, i, aov);
                ptr += 3;
            }
        }
    }
}

void SampleAccumulationBuffer::develop_to_tile_straight_alpha(
    Tile&                   tile,
    TileStack&              aov_tiles) const
{
    develop_to_tile_straight_alpha(
        AABB2u(Vector2u(0, 0), Vector2u(m_width - 1, m_height - 1)),
        tile,
        aov_tiles);
}

}   // namespace renderer
