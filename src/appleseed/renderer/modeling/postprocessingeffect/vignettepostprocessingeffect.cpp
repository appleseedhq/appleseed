
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
#include "vignettepostprocessingeffect.h"

// appleseed.renderer headers.
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

using namespace foundation;

namespace renderer
{

// FIXME move into a anonymous namespace and only expose the factory (?)

//
// VignetteEffect class implementation.
//

VignetteEffect::VignetteEffect(
    const float         intensity,
    const float         anisotropy,
    const Vector2u&     resolution,
    const Vector2f&     normalization_factor)
  : m_intensity(intensity)
  , m_anisotropy(anisotropy)
  , m_resolution(resolution)
  , m_normalization_factor(normalization_factor)
{
}

void VignetteEffect::release()
{
    delete this;
}

void VignetteEffect::apply(
    const Frame&    frame,
    const size_t    tile_x,
    const size_t    tile_y,
    IAbortSwitch&   abort_switch) const
{
    Image& image = frame.image();

    assert(tile_x < image.properties().m_tile_count_x);
    assert(tile_y < image.properties().m_tile_count_y);

    Tile& tile = image.tile(tile_x, tile_y);
    const size_t tile_width = tile.get_width();
    const size_t tile_height = tile.get_height();
    const size_t tile_offset_x = tile_x * tile_width;
    const size_t tile_offset_y = tile_y * tile_height;

    for (size_t y = 0; y < tile_height; ++y)
    {
        for (size_t x = 0; x < tile_width; ++x)
        {
            const Vector2u pixel_coord = Vector2u(x + tile_offset_x, y + tile_offset_y);

            // Pixel coordinate normalized to be in the [-1, 1] range vertically.
            const Vector2f coord = (2.0f * static_cast<Vector2f>(pixel_coord - m_resolution)) / m_normalization_factor;

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


//
// VignetteEffectFactory class implementation.
//

void VignetteEffectFactory::release()
{
    delete this;
}

VignetteEffect* VignetteEffectFactory::create(
    const ParamArray&   effect_params,
    const size_t        thread_index)
{
    const Vector2u resolution(
        effect_params.get_required<size_t>("resolution_x"),
        effect_params.get_required<size_t>("resolution_y"));
    const Vector2f normalization_factor(
        effect_params.get_required<float>("normalization_factor_x"),
        effect_params.get_required<float>("normalization_factor_y"));
    return new VignetteEffect(
        effect_params.get_required<float>("intensity"),
        effect_params.get_required<float>("anisotropy"),
        // effect_params.get_required<Vector2u>("resolution"),
        // effect_params.get_required<Vector2f>("normalization_factor")
        resolution, normalization_factor); // FIXME
}

}   // namespace renderer
