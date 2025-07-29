
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
#include "resamplex2applier.h"

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
// ResampleX2Applier class implementation.
//

ResampleX2Applier::ResampleX2Applier(
    const Image&            src_image,
    const SamplingMode      mode)
  : m_mode(mode)
  , m_src_width(src_image.properties().m_canvas_width)
  , m_src_height(src_image.properties().m_canvas_height)
  , m_src_image(src_image)
{
}

void ResampleX2Applier::release()
{
    delete this;
}

inline const Color3f ResampleX2Applier::sample(
    const float     fx,
    const float     fy) const
{
    const std::size_t x0 = truncate<std::size_t>(fx);
    const std::size_t y0 = truncate<std::size_t>(fy);
    const std::size_t x1 = std::min(x0 + 1, m_src_width - 1);
    const std::size_t y1 = std::min(y0 + 1, m_src_height - 1);

    // Retrieve the four surrounding pixels.
    Color3f c00, c10, c01, c11;
    m_src_image.get_pixel(x0, y0, c00);
    m_src_image.get_pixel(x1, y0, c10);
    m_src_image.get_pixel(x0, y1, c01);
    m_src_image.get_pixel(x1, y1, c11);

    if (m_mode == SamplingMode::DOUBLE)
    {
        // Compute bilinear interpolation weights.
        const float wx1 = fx - x0;
        const float wy1 = fy - y0;
        const float wx0 = 1.0f - wx1;
        const float wy0 = 1.0f - wy1;

        // Return the weighted sum.
        return
            c00 * wx0 * wy0 +
            c10 * wx1 * wy0 +
            c01 * wx0 * wy1 +
            c11 * wx1 * wy1;
    }
    else // m_mode == SamplingMode::HALVE
    {
        // Return the average sum.
        return 0.25f * (c00 + c10 + c01 + c11);
    }
}

void ResampleX2Applier::apply(
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

    const Vector2f scaling_factor(
        static_cast<float>(m_src_width - 1) / (dst_width - 1),
        static_cast<float>(m_src_height - 1) / (dst_height - 1));

    // Note: using "dst / 2 == m_src" instead of "dst == m_src * 2" when upsampling accounts for
    // the destination image having odd dimension values, making "dst == 2 * m_src + 1" actually.
    // Thus, integer truncation also allows a source image with odd dimensions when downsampling.
    assert(
        m_mode == SamplingMode::DOUBLE
        ? dst_width / 2 == m_src_width && dst_height / 2 == m_src_height    // scale x2 with bilinear filtering
        : dst_width == m_src_width / 2 && dst_height == m_src_height / 2);  // scale x1/2 with box filtering

    for (std::size_t y = 0; y < tile_height; ++y)
    {
        for (std::size_t x = 0; x < tile_width; ++x)
        {
            // Map the pixel coordinate from image to src_image.
            const float fy = (y + tile_offset.y) * scaling_factor.y;
            const float fx = (x + tile_offset.x) * scaling_factor.x;

            tile.set_pixel(x, y, sample(fx, fy));
        }
    }
}

}   // namespace renderer
