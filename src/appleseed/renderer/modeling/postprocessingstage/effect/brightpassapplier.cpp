
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
#include "brightpassapplier.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"

using namespace foundation;

namespace renderer
{

//
// BrightPassApplier class implementation.
//

BrightPassApplier::BrightPassApplier(
    const float     threshold,
    const float     soft_threshold)
  : m_threshold(threshold)
  , m_knee(threshold * soft_threshold)
{
}

void BrightPassApplier::release()
{
    delete this;
}

void BrightPassApplier::apply(
    Image&              image,
    const std::size_t   tile_x,
    const std::size_t   tile_y) const
{
    assert(tile_x < image.properties().m_tile_count_x);
    assert(tile_y < image.properties().m_tile_count_y);

    Tile& tile = image.tile(tile_x, tile_y);
    const std::size_t tile_width = tile.get_width();
    const std::size_t tile_height = tile.get_height();

    //
    // References:
    //
    //   https://github.com/keijiro/KinoBloom/
    //   https://catlikecoding.com/unity/tutorials/advanced-rendering/bloom/
    //

    const float eps = default_eps<float>(); // used to avoid divisions by zero

    for (std::size_t y = 0; y < tile_height; ++y)
    {
        for (std::size_t x = 0; x < tile_width; ++x)
        {
            Color3f color;
            tile.get_pixel(x, y, color);

            const float brightness = max_value(color);
            float contribution = brightness - m_threshold;

            if (m_knee > 0.0f)
            {
                float soft = contribution + m_knee;
                soft = clamp(soft, 0.0f, 2.0f * m_knee);
                soft = soft * soft * safe_rcp<float>(4.0f * m_knee, eps);
                contribution = std::max(soft, contribution);
            }

            // Filter out dark pixels.
            if (contribution <= 0.0f)
                color *= 0.0f;
            else
                color *= contribution * safe_rcp<float>(brightness, eps);

            tile.set_pixel(x, y, color);
        }
    }
}

}   // namespace renderer
