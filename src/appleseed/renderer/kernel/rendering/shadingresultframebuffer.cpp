
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

// Interface header.
#include "shadingresultframebuffer.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/kernel/shading/shadingresult.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/tile.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

ShadingResultFrameBuffer::ShadingResultFrameBuffer(
    const size_t                    width,
    const size_t                    height,
    const size_t                    aov_count)
  : AccumulatorTile(
        width,
        height,
        get_total_channel_count(aov_count))
  , m_aov_count(aov_count)
  , m_scratch(get_total_channel_count(aov_count))
{
}

ShadingResultFrameBuffer::ShadingResultFrameBuffer(
    const size_t                    width,
    const size_t                    height,
    const size_t                    aov_count,
    const AABB2u&                   crop_window)
  : AccumulatorTile(
        width,
        height,
        get_total_channel_count(aov_count),
        crop_window)
  , m_aov_count(aov_count)
  , m_scratch(get_total_channel_count(aov_count))
{
}

void ShadingResultFrameBuffer::add(
    const Vector2u&                 pi,
    const ShadingResult&            sample)
{
    float* ptr = &m_scratch[0];

    *ptr++ = sample.m_main[0];
    *ptr++ = sample.m_main[1];
    *ptr++ = sample.m_main[2];
    *ptr++ = sample.m_main[3];

    for (size_t i = 0, e = m_aov_count; i < e; ++i)
    {
        const Color4f& aov = sample.m_aovs[i];
        *ptr++ = aov[0];
        *ptr++ = aov[1];
        *ptr++ = aov[2];
        *ptr++ = aov[3];
    }

    AccumulatorTile::add(pi, &m_scratch[0]);
}

void ShadingResultFrameBuffer::merge(
    const size_t                    dest_x,
    const size_t                    dest_y,
    const ShadingResultFrameBuffer& source,
    const size_t                    source_x,
    const size_t                    source_y,
    const float                     scaling)
{
    assert(m_channel_count == source.m_channel_count);

    const float* APPLESEED_RESTRICT source_ptr = source.pixel(source_x, source_y);
    float* APPLESEED_RESTRICT dest_ptr = pixel(dest_x, dest_y);

    for (size_t i = 0, e = m_channel_count; i < e; ++i)
        dest_ptr[i] += source_ptr[i] * scaling;
}

void ShadingResultFrameBuffer::develop_to_tile(
    Tile&                           tile,
    TileStack&                      aov_tiles) const
{
    const float* ptr = pixel(0);

    for (size_t y = 0, h = m_height; y < h; ++y)
    {
        for (size_t x = 0, w = m_width; x < w; ++x)
        {
            const float weight = *ptr++;
            const float rcp_weight = weight == 0.0f ? 0.0f : 1.0f / weight;

            const Color4f color(ptr[0], ptr[1], ptr[2], ptr[3]);
            tile.set_pixel(x, y, color * rcp_weight);
            ptr += 4;

            for (size_t i = 0, e = m_aov_count; i < e; ++i)
            {
                const Color4f aov(ptr[0], ptr[1], ptr[2], ptr[3]);
                aov_tiles.set_pixel(x, y, i, aov * rcp_weight);
                ptr += 4;
            }
        }
    }
}

}   // namespace renderer
