
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
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

#include <iostream>

using namespace foundation;

namespace renderer
{

namespace
{

    //
    // Vignette post-processing effect applier.
    //

    class VignetteApplier
    : public IEffectApplier
    {
      public:
        VignetteApplier(const VignetteParams& params)
          : m_intensity(params.intensity)
          , m_anisotropy(params.anisotropy)
          , m_resolution(static_cast<Vector2f>(Vector2u(params.frame_width, params.frame_height)))
          , m_normalization_factor(Vector2f(params.distorted_frame_width, static_cast<float>(params.frame_height)))
        {
        }

        void release() override
        {
            delete this;
        }

        void apply(
            const Frame&    frame,
            const size_t    tile_x,
            const size_t    tile_y) const override
        {
            Image& image = frame.image();

            assert(tile_x < image.properties().m_tile_count_x);
            assert(tile_y < image.properties().m_tile_count_y);

            Tile& tile = image.tile(tile_x, tile_y);
            const size_t tile_width = tile.get_width();
            const size_t tile_height = tile.get_height();
            const Vector2u tile_offset(
                tile_x * image.properties().m_tile_width,
                tile_y * image.properties().m_tile_height);

            for (size_t y = 0; y < tile_height; ++y)
            {
                for (size_t x = 0; x < tile_width; ++x)
                {
                    const Vector2f pixel_coord = static_cast<Vector2f>(Vector2u(x, y) + tile_offset);

                    // Pixel coordinate normalized to be in the [-1, 1] range vertically.
                    const Vector2f coord = (2.0f * pixel_coord - m_resolution) / m_normalization_factor;

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

      private:
        // Settings.
        const float                     m_intensity;
        const float                     m_anisotropy;

        // Context.
        const foundation::Vector2f      m_resolution;
        const foundation::Vector2f      m_normalization_factor;
    };
}


//
// VignetteApplierFactory class implementation.
//

VignetteApplierFactory::VignetteApplierFactory(
    const VignetteParams& params)
  : m_params(params)
{
}

void VignetteApplierFactory::release()
{
    delete this;
}

IEffectApplier* VignetteApplierFactory::create()
{
    return new VignetteApplier(m_params);
}

IEffectApplier* VignetteApplierFactory::create(
    const VignetteParams& params)
{
    return new VignetteApplier(params);
}

}   // namespace renderer
