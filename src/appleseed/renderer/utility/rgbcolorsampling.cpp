
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
#include "rgbcolorsampling.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"

using namespace foundation;

namespace renderer
{

Color3f blerp(
    const Image&        image,
    const std::size_t   x0,
    const std::size_t   y0,
    const std::size_t   x1,
    const std::size_t   y1,
    const float         fx,
    const float         fy)
{
    // Retrieve the four surrounding pixels.
    Color3f c00, c10, c01, c11;
    image.get_pixel(x0, y0, c00);
    image.get_pixel(x1, y0, c10);
    image.get_pixel(x0, y1, c01);
    image.get_pixel(x1, y1, c11);

    // Compute weights.
    const float wx1 = fx - x0;
    const float wy1 = fy - y0;
    const float wx0 = 1.0f - wx1;
    const float wy0 = 1.0f - wy1;

    // Return the weighted sum.
    const Color3f result =
        c00 * wx0 * wy0 +
        c10 * wx1 * wy0 +
        c01 * wx0 * wy1 +
        c11 * wx1 * wy1;

    return result;
}

Color3f box_sample(const Image& image, const float fx, const float fy)
{
    const std::size_t width = image.properties().m_canvas_width;
    const std::size_t height = image.properties().m_canvas_height;

    assert(0.0f <= fx && fx <= width - 1.0f);
    assert(0.0f <= fy && fy <= height - 1.0f);

    const std::size_t x0 = truncate<std::size_t>(fx);
    const std::size_t y0 = truncate<std::size_t>(fy);
    const std::size_t x1 = std::min<std::size_t>(x0 + 1, width - 1);
    const std::size_t y1 = std::min<std::size_t>(y0 + 1, height - 1);

    return blerp(image, x0, y0, x1, y1, fx, fy);
}

Color3f clamped_box_sample(const Image& image, const float fx, const float fy)
{
    const std::size_t width = image.properties().m_canvas_width;
    const std::size_t height = image.properties().m_canvas_height;

    // Compute to which region the coordinate (fx, fy) belongs, similar to Cohen–Sutherland's algorithm.
    const int x_region = (fx < 0.0f) ? -1 : (fx > width - 1.0f) ? 1 : 0;
    const int y_region = (fy < 0.0f) ? -1 : (fy > height - 1.0f) ? 1 : 0;

    // There are 9 regions relative to the image, with 3 base cases:
    //   -+ 0+ ++    inside: 00
    //   -0 00 +0    outside lateral: 0+, -0, +0, 0-
    //   -- 0- +-    outside diagonal: -+, ++, --, +-

    // Thus, we can easily check the region of (fx, fy) and clamp it to be inside.
    const std::size_t x = static_cast<std::size_t>((x_region == -1) ? 0 : width - 1);
    const std::size_t y = static_cast<std::size_t>((y_region == -1) ? 0 : height - 1);

    if (x_region * y_region != 0)
    {
        Color3f corner_pixel;
        image.get_pixel(x, y, corner_pixel);

        return corner_pixel;
    }

    const std::size_t x0 = truncate<std::size_t>(fx);
    const std::size_t y0 = truncate<std::size_t>(fy);
    const std::size_t x1 = std::min<std::size_t>(x0 + 1, width - 1);
    const std::size_t y1 = std::min<std::size_t>(y0 + 1, height - 1);

    if (x_region != 0)
    {
        Color3f top_pixel, bottom_pixel;
        image.get_pixel(x, y0, bottom_pixel);
        image.get_pixel(x, y1, top_pixel);

        return lerp(bottom_pixel, top_pixel, fy - y0);
    }
    else if (y_region != 0)
    {
        Color3f left_pixel, right_pixel;
        image.get_pixel(x0, y, left_pixel);
        image.get_pixel(x1, y, right_pixel);

        return lerp(left_pixel, right_pixel, fx - x0);
    }
    else // inside the image
    {
        return blerp(image, x0, y0, x1, y1, fx, fy);
    }
}

Color3f kawase_sample(
    const Image&        image,
    const std::size_t   x,
    const std::size_t   y,
    const std::size_t   offset)
{
    const float fx = static_cast<float>(x);
    const float fy = static_cast<float>(y);
    const float off = static_cast<float>(offset) + 0.5f;

    // Since each corner sample is an average of 4 values, 16 pixels are used in total.
    Color3f top_left = clamped_box_sample(image, fx - off, fy + off);
    Color3f top_right = clamped_box_sample(image, fx + off, fy + off);
    Color3f bottom_left = clamped_box_sample(image, fx - off, fy - off);
    Color3f bottom_right = clamped_box_sample(image, fx + off, fy - off);

    // Average sample colors.
    return 0.25f * (top_left + top_right + bottom_left + bottom_right);
}

Color3f dual_filter_downsample(
    const Image&        image,
    const float         fx,
    const float         fy,
    const std::size_t   offset)
{
    const float off = static_cast<float>(offset);

    Color3f center = clamped_box_sample(image, fx, fy);
    Color3f top_left = clamped_box_sample(image, fx - off, fy + off);
    Color3f top_right = clamped_box_sample(image, fx + off, fy + off);
    Color3f bottom_left = clamped_box_sample(image, fx - off, fy - off);
    Color3f bottom_right = clamped_box_sample(image, fx + off, fy - off);

    return (
        4.0f * center
        + top_left + top_right + bottom_left + bottom_right)
        / 8.0f;
}

Color3f dual_filter_upsample(
    const Image&        image,
    const float         fx,
    const float         fy,
    const std::size_t   offset)
{
    const float off = static_cast<float>(offset);
    const float half_off = 0.5f * off;

    Color3f top = clamped_box_sample(image, fx, fy + off);
    Color3f left = clamped_box_sample(image, fx - off, fy);
    Color3f right = clamped_box_sample(image, fx + off, fy);
    Color3f bottom = clamped_box_sample(image, fx, fy - off);

    Color3f top_left = clamped_box_sample(image, fx - half_off, fy + half_off);
    Color3f top_right = clamped_box_sample(image, fx + half_off, fy + half_off);
    Color3f bottom_left = clamped_box_sample(image, fx - half_off, fy - half_off);
    Color3f bottom_right = clamped_box_sample(image, fx + half_off, fy - half_off);

    return (
        top + left + right + bottom
        + 2.0f * (top_left + top_right + bottom_left + bottom_right))
        / 12.0f;
}

Color3f box_9tap_upsample(
    const Image&    image,
    const float     fx,
    const float     fy)
{
    const float off = 1.0f;

    return (
        clamped_box_sample(image, fx - off, fy - off) +           // bottom left
        clamped_box_sample(image, fx      , fy - off) * 2.0f +    // bottom
        clamped_box_sample(image, fx + off, fy - off) +           // bottom right

        clamped_box_sample(image, fx - off, fy      ) * 2.0f +    // left
        clamped_box_sample(image, fx      , fy      ) * 4.0f +    // center
        clamped_box_sample(image, fx + off, fy      ) * 2.0f +    // right

        clamped_box_sample(image, fx - off, fy + off) +           // top left
        clamped_box_sample(image, fx      , fy + off) * 2.0f +    // top
        clamped_box_sample(image, fx + off, fy + off))            // top right
        / 16.0f;
}

Color3f box_13tap_downsample(
    const Image&    image,
    const float     fx,
    const float     fy)
{
    // . . . . . . .
    // . A . B . C .
    // . . D . E . .
    // . F . G . H .
    // . . I . J . .
    // . K . L . M .
    // . . . . . . .

    Color3f A = clamped_box_sample(image, fx - 1.0f, fy - 1.0f);
    Color3f B = clamped_box_sample(image, fx       , fy - 1.0f);
    Color3f C = clamped_box_sample(image, fx + 1.0f, fy - 1.0f);
    Color3f D = clamped_box_sample(image, fx - 0.5f, fy - 0.5f);
    Color3f E = clamped_box_sample(image, fx + 0.5f, fy - 0.5f);
    Color3f F = clamped_box_sample(image, fx - 1.0f, fy       );
    Color3f G = clamped_box_sample(image, fx,        fy       );
    Color3f H = clamped_box_sample(image, fx + 1.0f, fy       );
    Color3f I = clamped_box_sample(image, fx - 0.5f, fy + 0.5f);
    Color3f J = clamped_box_sample(image, fx + 0.5f, fy + 0.5f);
    Color3f K = clamped_box_sample(image, fx - 1.0f, fy + 1.0f);
    Color3f L = clamped_box_sample(image, fx       , fy + 1.0f);
    Color3f M = clamped_box_sample(image, fx + 1.0f, fy + 1.0f);

    return (
        (D + E + I + J) * 0.5f +    // center
        (A + B + G + F) * 0.125f +  // top left
        (B + C + H + G) * 0.125f +  // top right
        (F + G + L + K) * 0.125f +  // bottom left
        (G + H + M + L) * 0.125f)   // bottom right
        / 4.0f;
}

}   // namespace renderer
