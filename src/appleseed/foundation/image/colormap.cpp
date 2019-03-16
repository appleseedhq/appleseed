
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

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

void ColorMap::set_palette(const float* values, const size_t entry_count)
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

void ColorMap::remap_colors(
    const AABB2u&       crop_window,
    Image*              image,
    const float         max_val,
    const float         min_val)
{
    const float min_value = static_cast<float>(min_val);
    const float max_value = 
        max_val == 0
            ? get_max_value(image, crop_window)
            : static_cast<float>(max_val);

    if (max_value == 0.0f)
    {
        fill_aov(image, crop_window, evaluate_palette(0.0f));
        return;
    }

    assert(max_value > min_value);

    for (size_t y = crop_window.min.y; y <= crop_window.max.y; ++y)
    {
        for (size_t x = crop_window.min.x; x <= crop_window.max.x; ++x)
        {
            Color3f value;
            image->get_pixel(x, y, value);

            const float c = saturate(fit(value[0], min_value, max_value, 0.0f, 1.0f));

            image->set_pixel(x, y, evaluate_palette(c));
        }
    }
}

Color3f ColorMap::evaluate_palette(float x) const
{
    // When AOV contains a single channel we do not set the color palette.
    if (m_palette.empty())
        return Color3f(x);

    x *= m_palette.size() - 1;

    const size_t ix = min(truncate<size_t>(x), m_palette.size() - 2);
    const float w = x - ix;

    return lerp(m_palette[ix], m_palette[ix + 1], w);
}

float ColorMap::get_max_value(const Image* image, const AABB2u& crop_window)
{
    float max_value = 0.0f;

    for (size_t y = crop_window.min.y; y <= crop_window.max.y; ++y)
    {
        for (size_t x = crop_window.min.x; x <= crop_window.max.x; ++x)
        {
            Color3f value;
            image->get_pixel(x, y, value);
            max_value = max(value[0], max_value);
        }
    }

    return max_value;
}

void ColorMap::fill_aov(
    Image*          image,
    const AABB2u&   crop_window,
    const Color3f&  color)
{
    for (size_t y = crop_window.min.y; y <= crop_window.max.y; ++y)
    {
        for (size_t x = crop_window.min.x; x <= crop_window.max.x; ++x)
            image->set_pixel(x, y, color);
    }
}

}   // namespace foundation
