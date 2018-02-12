
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
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

using namespace std;

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

    const size_t x0 = static_cast<size_t>(max(from.x, 0));
    const size_t y0 = static_cast<size_t>(max(from.y, 0));
    const size_t x1 = static_cast<size_t>(min(to.x, w - 1));
    const size_t y1 = static_cast<size_t>(min(to.y, h - 1));

    for (size_t y = y0; y <= y1; ++y)
    {
        for (size_t x = x0; x <= x1; ++x)
        {
            Color4f background;
            image.get_pixel(x, y, background);

            image.set_pixel(
                x, y,
                color * color.a + background * (1.0f - color.a));
        }
    }
}

void Drawing::draw_dot(
    Image&              image,
    const Vector2d&     position,
    const Color4f&      color)
{
    static size_t DotIntensity[16] =
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
                const float alpha = color.a * DotIntensity[y * 4 + x] * (1.0f / 255.0f);

                Color4f background;
                image.get_pixel(
                    static_cast<size_t>(ix),
                    static_cast<size_t>(iy),
                    background);

                image.set_pixel(
                    static_cast<size_t>(ix),
                    static_cast<size_t>(iy),
                    color * alpha + background * (1.0f - alpha));
            }
        }
    }
}

void Drawing::blit_bitmap(
    Image&              image,
    const Vector2i&     position,
    const uint8*        bitmap,
    const size_t        bitmap_width,
    const size_t        bitmap_height,
    const PixelFormat   bitmap_pixel_format,
    const Color4f&      multiplier)
{
    const CanvasProperties& props = image.properties();
    const int image_width = static_cast<int>(props.m_canvas_width);
    const int image_height = static_cast<int>(props.m_canvas_height);

    for (size_t y = 0; y < bitmap_height; ++y)
    {
        for (size_t x = 0; x < bitmap_width; ++x)
        {
            const int ix = position.x + static_cast<int>(x);
            const int iy = position.y + static_cast<int>(y);

            if (ix >= 0 && iy >= 0 && ix < image_width && iy < image_height)
            {
                const uint8* pixel = bitmap + (y * bitmap_width + x) * 4;

                Color4f color(0.0f);
                Pixel::convert_from_format<float>(
                    bitmap_pixel_format,
                    pixel,
                    pixel + 4,
                    1,
                    &color[0],
                    1);

                color *= multiplier;

                Color4f background;
                image.get_pixel(
                    static_cast<size_t>(ix),
                    static_cast<size_t>(iy),
                    background);

                image.set_pixel(
                    static_cast<size_t>(ix),
                    static_cast<size_t>(iy),
                    color * color.a + background * (1.0f - color.a));
            }
        }
    }
}

}   // namespace foundation
