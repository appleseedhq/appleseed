
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Achal Pandey, The appleseedhq Organization
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
#include "colormap.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <limits>

namespace foundation
{

namespace
{
    template <typename Func>
    void for_each_pixel(
        const Image&    image,
        const AABB2u&   crop_window,
        const Func&     visitor)
    {
        for (size_t y = crop_window.min.y; y <= crop_window.max.y; ++y)
        {
            for (size_t x = crop_window.min.x; x <= crop_window.max.x; ++x)
            {
                Color3f color;
                image.get_pixel(x, y, color);
                visitor(color);
            }
        }
    }

    template <typename Func>
    void for_each_pixel(
        Image&          image,
        const AABB2u&   crop_window,
        const Func&     mutator)
    {
        for (size_t y = crop_window.min.y; y <= crop_window.max.y; ++y)
        {
            for (size_t x = crop_window.min.x; x <= crop_window.max.x; ++x)
            {
                Color3f color;
                image.get_pixel(x, y, color);
                image.set_pixel(x, y, mutator(color));
            }
        }
    }
}

void ColorMap::find_min_max_red_channel(
    const Image&    image,
    float&          min_value,
    float&          max_value)
{
    find_min_max_red_channel(
        image,
        get_full_crop_window(image),
        min_value,
        max_value);
}

void ColorMap::find_min_max_red_channel(
    const Image&    image,
    const AABB2u&   crop_window,
    float&          min_value,
    float&          max_value)
{
    min_value = +std::numeric_limits<float>::max();
    max_value = -std::numeric_limits<float>::max();

    for_each_pixel(image, crop_window, [&min_value, &max_value](const Color3f& color)
    {
        min_value = std::min(color[0], min_value);
        max_value = std::max(color[0], max_value);
    });
}

void ColorMap::find_min_max_relative_luminance(
    const Image&    image,
    float&          min_luminance,
    float&          max_luminance)
{
    find_min_max_relative_luminance(
        image,
        get_full_crop_window(image),
        min_luminance,
        max_luminance);
}

void ColorMap::find_min_max_relative_luminance(
    const Image&    image,
    const AABB2u&   crop_window,
    float&          min_luminance,
    float&          max_luminance)
{
    min_luminance = +std::numeric_limits<float>::max();
    max_luminance = -std::numeric_limits<float>::max();

    for_each_pixel(image, crop_window, [&min_luminance, &max_luminance](const Color3f& color)
    {
        min_luminance = std::min(luminance(color), min_luminance);
        max_luminance = std::max(luminance(color), max_luminance);
    });
}

void ColorMap::set_palette_from_array(const float* values, const size_t entry_count)
{
    m_palette.resize(entry_count);

    for (size_t i = 0; i < entry_count; ++i)
    {
        m_palette[i] =
            Color3f(
                values[i * 3 + 0],
                values[i * 3 + 1],
                values[i * 3 + 2]);
    }
}

void ColorMap::set_palette_from_image_file(const Image& image)
{
    const size_t image_width = image.properties().m_canvas_width;
    m_palette.resize(image_width);

    for (size_t i = 0; i < image_width; ++i)
        image.get_pixel(i, 0, m_palette[i]);
}

void ColorMap::remap_red_channel(
    Image&          image,
    const float     min_value,
    const float     max_value) const
{
    remap_red_channel(
        image,
        get_full_crop_window(image),
        min_value,
        max_value);
}

void ColorMap::remap_red_channel(
    Image&          image,
    const AABB2u&   crop_window,
    const float     min_value,
    const float     max_value) const
{
    if (max_value == min_value)
    {
        const Color3f mapped_color = evaluate_palette(0.0f);

        for_each_pixel(image, crop_window, [mapped_color](const Color3f& color)
        {
            return mapped_color;
        });
    }
    else
    {
        const float k = 1.0f / (max_value - min_value);

        for_each_pixel(image, crop_window, [this, min_value, k](const Color3f& color)
        {
            const float x = saturate((color[0] - min_value) * k);
            return evaluate_palette(x);
        });
    }
}

void ColorMap::remap_relative_luminance(
    Image&          image,
    const float     min_luminance,
    const float     max_luminance) const
{
    remap_relative_luminance(
        image,
        get_full_crop_window(image),
        min_luminance,
        max_luminance);
}

void ColorMap::remap_relative_luminance(
    Image&          image,
    const AABB2u&   crop_window,
    const float     min_luminance,
    const float     max_luminance) const
{
    if (min_luminance == max_luminance)
    {
        const Color3f mapped_color = evaluate_palette(0.0f);

        for_each_pixel(image, crop_window, [mapped_color](const Color3f& color)
        {
            return mapped_color;
        });
    }
    else
    {
        const float k = 1.0f / (max_luminance - min_luminance);

        for_each_pixel(image, crop_window, [this, min_luminance, k](const Color3f& color)
        {
            const float x = saturate((luminance(color) - min_luminance) * k);
            return evaluate_palette(x);
        });
    }
}

Color3f ColorMap::evaluate_palette(float x) const
{
    assert(m_palette.size() >= 2);

    x *= m_palette.size() - 1;

    const size_t ix = std::min(truncate<size_t>(x), m_palette.size() - 2);
    const float w = x - ix;

    return lerp(m_palette[ix], m_palette[ix + 1], w);
}

AABB2u ColorMap::get_full_crop_window(const Image& image)
{
    const CanvasProperties& props = image.properties();
    return
        AABB2u(
            Vector2u(0, 0),
            Vector2u(props.m_canvas_width - 1, props.m_canvas_height - 1));
}

}   // namespace foundation
