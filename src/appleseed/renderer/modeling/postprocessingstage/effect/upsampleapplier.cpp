
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
#include "upsampleapplier.h"

// appleseed.renderer headers.
#include "renderer/utility/rgbcolorsampling.h"
#include "renderer/modeling/postprocessingstage/Instrumentor.h" // FIXME remove

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
// UpsampleApplier class implementation.
//

UpsampleApplier::UpsampleApplier(
    const UpsampleParams& params)
  : m_src_width(params.src_image.properties().m_canvas_width)
  , m_src_height(params.src_image.properties().m_canvas_height)
  , m_border_size(2)
  , m_src_image_with_border(
      Image(
          m_src_width + 2 * m_border_size,
          m_src_height + 2 * m_border_size,
          m_src_width + 2 * m_border_size,
          m_src_height + 2 * m_border_size,
          params.src_image.properties().m_channel_count,
          params.src_image.properties().m_pixel_format))
{
PROFILE_FUNCTION();
    const foundation::Image& src_image = params.src_image;
    const std::size_t src_width_with_border = m_src_width + 2 * m_border_size;
    const std::size_t src_height_with_border = m_src_height + 2 * m_border_size;

    // Copy src_image pixels into the center of m_src_image_with_border.
    for (std::size_t y = 0; y < m_src_height; ++y)
    {
        for (std::size_t x = 0; x < m_src_width; ++x)
        {
            Color3f color;
            src_image.get_pixel(x, y, color);

            m_src_image_with_border.set_pixel(
                x + m_border_size,
                y + m_border_size,
                color);
        }
    }

    // Fill in the borders by copying the color of the closest pixel (i.e. texture clamping).
{ PROFILE_SCOPE("Fill borders");
    // Copy corners.
    {
        Color3f top_left, top_right, bottom_left, bottom_right;
        src_image.get_pixel(0, m_src_height - 1, top_left);
        src_image.get_pixel(m_src_width - 1, m_src_height - 1, top_right);
        src_image.get_pixel(0, 0, bottom_left);
        src_image.get_pixel(m_src_width - 1, 0, bottom_right);

        m_src_image_with_border.set_pixel(0, src_height_with_border - 1, top_left);
        m_src_image_with_border.set_pixel(src_width_with_border - 1, src_height_with_border - 1, top_right);
        m_src_image_with_border.set_pixel(0, 0, bottom_left);
        m_src_image_with_border.set_pixel(src_width_with_border - 1, 0, bottom_right);
    }

    // Copy vertical borders.
    for (std::size_t y = 0; y < m_src_height; ++y)
    {
        Color3f left, right;
        src_image.get_pixel(0, y, left);
        src_image.get_pixel(m_src_width - 1, y, right);

        m_src_image_with_border.set_pixel(0, y, left);
        m_src_image_with_border.set_pixel(src_width_with_border - 1, y, right);
    }

    // Copy horizontal borders.
    for (std::size_t x = 0; x < m_src_width; ++x)
    {
        Color3f top, bottom;
        src_image.get_pixel(x, 0, bottom);
        src_image.get_pixel(x, m_src_height - 1, top);

        m_src_image_with_border.set_pixel(x, 0, bottom);
        m_src_image_with_border.set_pixel(x, src_height_with_border - 1, top);
    }
}
}

void UpsampleApplier::release()
{
    delete this;
}

void UpsampleApplier::apply(
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

    for (std::size_t y = 0; y < tile_height; ++y)
    {
        for (std::size_t x = 0; x < tile_width; ++x)
        {
            // Map the pixel coordinate from image to src_image, then shift it by m_border_size.
            const float fy = (y + tile_offset.y) * scaling_factor.y + m_border_size;
            const float fx = (x + tile_offset.x) * scaling_factor.x + m_border_size;

            // Dual-filtering based upsample.
            const Color3f result = (
                blerp(m_src_image_with_border, fx - 1.0f, fy)
                + blerp(m_src_image_with_border, fx + 1.0f, fy)
                + blerp(m_src_image_with_border, fx, fy - 1.0f)
                + blerp(m_src_image_with_border, fx, fy + 1.0f)
                + 2.0f * blerp(m_src_image_with_border, fx - 0.5f, fy - 0.5f)
                + 2.0f * blerp(m_src_image_with_border, fx - 0.5f, fy + 0.5f)
                + 2.0f * blerp(m_src_image_with_border, fx + 0.5f, fy - 0.5f)
                + 2.0f * blerp(m_src_image_with_border, fx + 0.5f, fy + 0.5f))
                / 12.0f;

            tile.set_pixel(x, y, result);
        }
    }
}

}   // namespace renderer
