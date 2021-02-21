
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
#include "vignetteapplier.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"

using namespace foundation;

namespace renderer
{

//
// VignetteApplier class implementation.
//

VignetteApplier::VignetteApplier(
    const float frame_width,
    const float frame_height,
    const float intensity,
    const float anisotropy)
  : m_intensity(intensity)
  , m_resolution(Vector2f(frame_width, frame_height))
  , m_vignette_resolution(
        Vector2f(
            lerp(frame_height, frame_width, anisotropy),    // vignette_width
            frame_height))                                  // vignette_height
{
    //
    // We normalize pixel coordinates when applying the effect so that they range from -1 to 1 vertically.
    //
    // Horizontally, we divide frame_width by the frame_height for a perfectly rounded vignette (i.e. no anisotropy), and by
    // the frame_width to respect the frame's aspect ratio (i.e. full anisotropy), hence the lerp in m_vignette_resolution.x.
    //
    // See "Normalizing coordinates" in https://shadertoyunofficial.wordpress.com/2019/01/02/programming-tricks-in-shadertoy-glsl/
    //
}

void VignetteApplier::release()
{
    delete this;
}

void VignetteApplier::apply(
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

    for (std::size_t y = 0; y < tile_height; ++y)
    {
        for (std::size_t x = 0; x < tile_width; ++x)
        {
            const Vector2f pixel_coord = static_cast<Vector2f>(Vector2u(x, y) + tile_offset);

            // Pixel coordinate normalized to be in the [-1, 1] range vertically.
            const Vector2f coord = (2.0f * pixel_coord - m_resolution) / m_vignette_resolution;

            //
            // Port of Keijiro Takahashi's natural vignetting effect for Unity.
            // Recreates natural illumination falloff, which is approximated by the "cosine fourth" law of illumination falloff.
            //
            // References:
            //
            //   https://github.com/keijiro/KinoVignette
            //   https://en.wikipedia.org/wiki/Vignetting#Natural_vignetting
            //

            const float linear_radial_falloff = norm(coord) * m_intensity;
            const float quadratic_radial_falloff = linear_radial_falloff * linear_radial_falloff + 1.0f;

            // Inversely proportional to the fourth power of the distance from the pixel to the image center.
            const float inverse_biquadratic_radial_falloff = 1.0f / (quadratic_radial_falloff * quadratic_radial_falloff);

            Color4f pixel;
            tile.get_pixel(x, y, pixel);

            pixel.rgb() *= inverse_biquadratic_radial_falloff;
            tile.set_pixel(x, y, pixel);
        }
    }
}

}   // namespace renderer
