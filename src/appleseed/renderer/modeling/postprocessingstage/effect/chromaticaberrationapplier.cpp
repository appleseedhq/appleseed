
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
#include "chromaticaberrationapplier.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/math/scalar.h"

using namespace foundation;

namespace renderer
{

//
// ChromaticAberrationApplier class implementation.
//

ChromaticAberrationApplier::ChromaticAberrationApplier(
    const Image&        src_image,
    const float         strength,
    const std::size_t   sample_count)
  : m_strength(strength)
  , m_sample_count(sample_count)
  , m_src_image(src_image)
  , m_resolution(
        Vector2f(
            static_cast<float>(src_image.properties().m_canvas_width),
            static_cast<float>(src_image.properties().m_canvas_height)))
{
}

void ChromaticAberrationApplier::release()
{
    delete this;
}

Color3f ChromaticAberrationApplier::sample_at(const Vector2f& uv) const
{
    // Remap uv to image coordinates, clamping out of range values.
    const float fx = clamp(uv.x * m_resolution.x, 0.0f, m_resolution.x - 1.0f);
    const float fy = clamp(uv.y * m_resolution.y, 0.0f, m_resolution.y - 1.0f);

    // Sample pixel colors from the original image.
    Color3f sample;
    m_src_image.get_pixel(truncate<std::size_t>(fx), truncate<std::size_t>(fy), sample);

    return sample;
}

namespace
{
    Color3f hue_offset(const float t)
    {
        //
        // Linearly interpolates blur-weights from red to green to blue:
        //
        //      0|<------------ t ------------>|1
        //
        //       red          green         blue
        //       |---------     .     ---------|
        //       |         \   / \   /         |
        //       |          \./   \./          |
        //       |          / \   / \          |
        //       |         /   \ /   \         |
        //       |=========-----.-----=========|
        //                   ^     ^
        //    red+green = yellow  cyan = green+blue
        //
        // Reference:
        //
        //    https://www.shadertoy.com/view/MdsyDX
        //

        const float t0 = 3.0f * t - 1.5f;

        return
            Color3f(
                saturate(-t0),
                saturate(1.0f - std::abs(t0)),
                saturate(+t0));
    }

    Vector2f radial_distort(const Vector2f& uv, const float amount)
    {
        const Vector2f radius(uv - Vector2f(0.5f));

        // Increase distortion towards the image edges.
        return uv + radius * dot(radius, radius) * amount;
    }
}


void ChromaticAberrationApplier::apply(
    Image&              image,
    const std::size_t   tile_x,
    const std::size_t   tile_y) const
{
    const CanvasProperties& props = image.properties();

    assert(tile_x < props.m_tile_count_x);
    assert(tile_y < props.m_tile_count_y);

    Tile& tile = image.tile(tile_x, tile_y);
    const std::size_t tile_width = tile.get_width();
    const std::size_t tile_height = tile.get_height();
    const Vector2u tile_offset(
        tile_x * props.m_tile_width,
        tile_y * props.m_tile_height);

    for (std::size_t y = 0; y < tile_height; ++y)
    {
        for (std::size_t x = 0; x < tile_width; ++x)
        {
            // Compute the pixel coordinate in relation to the image.
            const float fx = static_cast<float>(x + tile_offset.x) + 0.5f;
            const float fy = static_cast<float>(y + tile_offset.y) + 0.5f;

            // Pixel coordinate normalized to be in the [0, 1] range.
            const Vector2f uv(fx / m_resolution.x, fy / m_resolution.y);

            //
            // Simulate lateral chromatic aberration.
            //
            // References:
            //
            //   https://www.shadertoy.com/view/XssGz8
            //   http://loopit.dk/rendering_inside.pdf (slides 19-20)
            //   https://en.wikipedia.org/wiki/Chromatic_aberration#Types
            //

            Color3f color_sum(0.0f);
            Color3f weight_sum(0.0f);

            for (std::size_t i = 0; i < m_sample_count; ++i)
            {
                const float t = static_cast<float>(i) / (m_sample_count - 1.0f);

                const Color3f weight = hue_offset(t);
                weight_sum += weight;

                const Vector2f shifted_uv(radial_distort(uv, 0.6f * m_strength * t));
                color_sum += weight * sample_at(shifted_uv);
            }

            tile.set_pixel(x, y, color_sum / weight_sum);
        }
    }
}

}   // namespace renderer
