
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
#include "developedpixelbuffer.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

DevelopedPixelBuffer::DevelopedPixelBuffer(
    const int           width,
    const int           height,
    const size_t        aov_count,
    const AABB2u&       tile_bbox,
    const ColorSpace    color_space,
    const float         max_contrast)
  : m_width(width + 1)                      // 1 pixel margin on the left
  , m_height(height + 1)                    // 1 pixel margin at the top
  , m_channel_count(4 + aov_count * 3)      // main RGBA channels + RGB channels per AOV
  , m_tile_bbox(tile_bbox)
  , m_color_space(color_space)
  , m_max_contrast(max_contrast)
{
    m_buffer.resize(m_width * m_height * m_channel_count, 0.0f);
}

void DevelopedPixelBuffer::set_pixel_color(
    const int           x,
    const int           y,
    const Color4f&      linear_rgba)
{
    const Color3f rgb =
        transform_color(linear_rgba.rgb(), ColorSpaceLinearRGB, m_color_space);

    float* p = pixel(x, y);

    p[0] = rgb[0];
    p[1] = rgb[1];
    p[2] = rgb[2];
    p[3] = linear_rgba[3];
}

void DevelopedPixelBuffer::set_pixel_aov(
    const int           x,
    const int           y,
    const size_t        aov_index,
    const Color3f&      linear_rgb)
{
    const Color3f rgb =
        transform_color(linear_rgb, ColorSpaceLinearRGB, m_color_space);

    float* p = pixel(x, y) + 4 + aov_index * 3;

    p[0] = rgb[0];
    p[1] = rgb[1];
    p[2] = rgb[2];
}

bool DevelopedPixelBuffer::check_contrast(
    const int           ix,
    const int           iy,
    const int           tx,
    const int           ty) const
{
    assert(tx + 1 >= 0 && tx + 1 < m_width);
    assert(ty + 1 >= 0 && ty + 1 < m_height);

    if (m_tile_bbox.min.y > 0 || iy > 0)
    {
        if (!check_contrast_between(tx, ty, tx, ty - 1))
            return false;
    }

    if (m_tile_bbox.min.x > 0 || ix > 0)
    {
        if (!check_contrast_between(tx, ty, tx - 1, ty))
            return false;
    }

    return true;
}

float* DevelopedPixelBuffer::pixel(
    const int           x,
    const int           y)
{
    const int x1 = x + 1;
    const int y1 = y + 1;

    assert(x1 >= 0 && x1 < m_width);
    assert(y1 >= 0 && y1 < m_height);

    return &m_buffer[(y1 * m_width + x1) * m_channel_count];
}

const float* DevelopedPixelBuffer::pixel(
    const int           x,
    const int           y) const
{
    return const_cast<DevelopedPixelBuffer*>(this)->pixel(x, y);
}

bool DevelopedPixelBuffer::check_contrast_between(
    const int           x1,
    const int           y1,
    const int           x2,
    const int           y2) const
{
    const float* RESTRICT p1 = pixel(x1, y1);
    const float* RESTRICT p2 = pixel(x2, y2);

    for (size_t i = 0; i < m_channel_count; ++i)
    {
        if (abs(p1[i] - p2[i]) > m_max_contrast)
            return false;
    }

    return true;
}

}   // namespace renderer
