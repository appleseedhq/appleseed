
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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
#include "drawing.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"

// Standard headers.
#include <algorithm>
#include <cstdint>

namespace foundation
{

//
// Drawing class implementation.
//

void Drawing::draw_filled_rect(
    Image&              image,
    const Vector2i&     from,
    const Vector2i&     to,
    const Color4f&      color)
{
    const CanvasProperties& props = image.properties();
    const int w = static_cast<int>(props.m_canvas_width);
    const int h = static_cast<int>(props.m_canvas_height);

    const size_t x0 = static_cast<size_t>(std::max(from.x, 0));
    const size_t y0 = static_cast<size_t>(std::max(from.y, 0));
    const size_t x1 = static_cast<size_t>(std::min(to.x, w - 1));
    const size_t y1 = static_cast<size_t>(std::min(to.y, h - 1));

    const Color4f color_premult = color.premultiplied();

    for (size_t y = y0; y <= y1; ++y)
    {
        for (size_t x = x0; x <= x1; ++x)
        {
            Color4f background_premult;
            image.get_pixel(x, y, background_premult);

            image.set_pixel(
                x, y,
                color_premult + background_premult * (1.0f - color_premult.a));
        }
    }
}

void Drawing::draw_dot(
    Image&              image,
    const Vector2d&     position,
    const Color4f&      color)
{
    static const std::uint8_t DotAlphaMask[16] =
    {
         24, 171, 178,  48,
        159, 255, 255, 207,
        183, 255, 255, 231,
         64, 227, 241, 112
    };

    const CanvasProperties& props = image.properties();
    const int w = static_cast<int>(props.m_canvas_width);
    const int h = static_cast<int>(props.m_canvas_height);

    const int cx = static_cast<int>(position.x * props.m_canvas_width);
    const int cy = static_cast<int>(position.y * props.m_canvas_height);

    for (int y = 0; y < 4; ++y)
    {
        for (int x = 0; x < 4; ++x)
        {
            const int ix = cx + x - 2;
            const int iy = cy + y - 2;

            if (ix >= 0 && iy >= 0 && ix < w && iy < h)
            {
                const float dot_alpha = DotAlphaMask[y * 4 + x] * (1.0f / 255.0f);

                Color4f color_premult = color;
                color_premult.a *= dot_alpha;
                color_premult.premultiply_in_place();

                Color4f background_premult;
                image.get_pixel(
                    static_cast<size_t>(ix),
                    static_cast<size_t>(iy),
                    background_premult);

                image.set_pixel(
                    static_cast<size_t>(ix),
                    static_cast<size_t>(iy),
                    color_premult + background_premult * (1.0f - color_premult.a));
            }
        }
    }
}

void Drawing::blit_bitmap(
    Image&              image,
    const Vector2i&     position,
    const void*         bitmap,
    const size_t        bitmap_width,
    const size_t        bitmap_height,
    const PixelFormat   bitmap_pixel_format,
    const Color4f&      tint)
{
    const CanvasProperties& props = image.properties();
    const int image_width = static_cast<int>(props.m_canvas_width);
    const int image_height = static_cast<int>(props.m_canvas_height);

    const size_t pixel_size = 4 * Pixel::size(bitmap_pixel_format);

    for (size_t y = 0; y < bitmap_height; ++y)
    {
        for (size_t x = 0; x < bitmap_width; ++x)
        {
            const int ix = position.x + static_cast<int>(x);
            const int iy = position.y + static_cast<int>(y);

            if (ix >= 0 && iy >= 0 && ix < image_width && iy < image_height)
            {
                const std::uint8_t* pixel = static_cast<const std::uint8_t*>(bitmap) + (y * bitmap_width + x) * pixel_size;

                Color4f color_premult(0.0f);
                Pixel::convert_from_format<float>(
                    bitmap_pixel_format,
                    pixel,
                    pixel + pixel_size,
                    1,
                    &color_premult[0],
                    1);

                color_premult *= tint;

                Color4f background_premult;
                image.get_pixel(
                    static_cast<size_t>(ix),
                    static_cast<size_t>(iy),
                    background_premult);

                image.set_pixel(
                    static_cast<size_t>(ix),
                    static_cast<size_t>(iy),
                    color_premult + background_premult * (1.0f - color_premult.a));
            }
        }
    }
}

}   // namespace foundation
