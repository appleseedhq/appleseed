
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
    const float         offset,
    const std::size_t   min_shift,
    const std::size_t   max_shift)
  : m_offset(offset)
  , m_min_shift(min_shift)
  , m_max_shift(max_shift)
  , m_src_image(src_image)
{
}

void ChromaticAberrationApplier::release()
{
    delete this;
}

Color3f ChromaticAberrationApplier::sample_at(
    const std::size_t pixel_x,
    const std::size_t pixel_y) const
{
    Color3f sample;

    // Clamp out of range coordinates.
    m_src_image.get_pixel(
        std::min(pixel_x, m_src_image.properties().m_canvas_width - 1),
        std::min(pixel_y, m_src_image.properties().m_canvas_height - 1),
        sample);

    return sample;
}

void ChromaticAberrationApplier::apply(
    Image&              image,
    const std::size_t   tile_x,
    const std::size_t   tile_y) const
{
    const CanvasProperties& props = image.properties();

    const Vector2f resolution(
        static_cast<float>(props.m_canvas_width),
        static_cast<float>(props.m_canvas_height));

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
            const Vector2u pixel_coord = Vector2u(x, y) + tile_offset;

            // Pixel coordinate normalized to be in the [-1, 1] range vertically.
            const Vector2f coord((2.0f * static_cast<Vector2f>(pixel_coord) - resolution) / resolution.y);

            // Increase color shifting (linearly) towards the edges of the frame.
            const float radial_intensity = norm(coord) - m_offset;

            const std::size_t shift_amount =
                round<std::size_t>(
                    mix(static_cast<float>(m_min_shift), static_cast<float>(m_max_shift), radial_intensity));

            // Sample the original pixel coordinate with slightly different shifts for each color component.
            const Color3f color(
                sample_at(pixel_coord.x + 0 * shift_amount, pixel_coord.y + 0 * shift_amount).r,
                sample_at(pixel_coord.x + 1 * shift_amount, pixel_coord.y + 1 * shift_amount).g,
                sample_at(pixel_coord.x + 2 * shift_amount, pixel_coord.y + 2 * shift_amount).b);

            tile.set_pixel(x, y, color);
        }
    }
}

}   // namespace renderer