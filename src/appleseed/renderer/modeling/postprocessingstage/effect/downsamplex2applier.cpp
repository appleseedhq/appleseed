
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Tiago Chaves, The appleseedhq Organization
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
#include "downsamplex2applier.h"

// appleseed.renderer headers.
#include "renderer/utility/rgbcolorsampling.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cmath>

using namespace foundation;

namespace renderer
{

//
// DownsampleX2Applier class implementation.
//

DownsampleX2Applier::DownsampleX2Applier(
    const DownsampleX2Params& params)
  : m_src_width(params.src_image.properties().m_canvas_width)
  , m_src_height(params.src_image.properties().m_canvas_height)
  , m_src_image(params.src_image)
{
}

void DownsampleX2Applier::release()
{
    delete this;
}

void DownsampleX2Applier::apply(
    Image&              image,
    const std::size_t   tile_x,
    const std::size_t   tile_y) const
{
    assert(tile_x < image.properties().m_tile_count_x);
    assert(tile_y < image.properties().m_tile_count_y);

    Tile& tile = image.tile(tile_x, tile_y);
    const std::size_t tile_width = tile.get_width();
    const std::size_t tile_height = tile.get_height();
    const Vector2u tile_offset(
        tile_x * image.properties().m_tile_width,
        tile_y * image.properties().m_tile_height);

    const std::size_t dst_width = image.properties().m_canvas_width;
    const std::size_t dst_height = image.properties().m_canvas_height;

    // Scale x1/2 through box filtering.
    assert(dst_width == m_src_width / 2);
    assert(dst_height == m_src_height / 2);

    for (std::size_t y = 0; y < tile_height; ++y)
    {
        for (std::size_t x = 0; x < tile_width; ++x)
        {
            const std::size_t y0 = 2 * (y + tile_offset.y);
            const std::size_t y1 = y0 + 1;
            const std::size_t x0 = 2 * (x + tile_offset.x);
            const std::size_t x1 = x0 + 1;

            Color3f c00, c01, c10, c11;
            m_src_image.get_pixel(x0, y0, c00);
            m_src_image.get_pixel(x1, y0, c10);
            m_src_image.get_pixel(x0, y1, c01);
            m_src_image.get_pixel(x1, y1, c11);

            const Color3f result = 0.25f * (c00 + c01 + c10 + c11);
            tile.set_pixel(x, y, result);
        }
    }
}

}   // namespace renderer
