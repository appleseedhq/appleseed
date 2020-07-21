
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
#include "tonemapapplier.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"

using namespace foundation;

namespace renderer
{

//
// ToneMapApplier class implementation.
//

void ToneMapApplier::release()
{
    delete this;
}

void ToneMapApplier::apply(
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
            // TODO move gamma correction out of the TMOs (?)

            // FIXME unpremultiply, tonemap, saturate, then premultiply
            //       as in conversion.cpp (also check if it is Color3f)

            Color4f pixel;
            tile.get_pixel(x, y, pixel);

            // pixel.unpremultiply_in_place();

            tone_map(pixel.rgb());

            // pixel.rgb() = srgb_to_linear_rgb(pixel.rgb());
            pixel = saturate(pixel);

            // pixel.premultiply_in_place();

            tile.set_pixel(x, y, pixel);
        }
    }
}

//
// AcesNarkowiczApplier class implementation.
//

AcesNarkowiczApplier::AcesNarkowiczApplier(const float gamma)
  : m_gamma(gamma)
{
}

void AcesNarkowiczApplier::tone_map(Color3f& color) const
{
    //
    // Apply Krzysztof Narkowicz's fitting of the ACES curve.
    //
    // Note: for the original ACES curve, color should me multiplied by 0.6.
    //
    // Reference:
    //
    //   https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/
    //

    const Color3f a(2.51f);
    const Color3f b(0.03f);
    const Color3f c(2.43f);
    const Color3f d(0.59f);
    const Color3f e(0.14f);
    color =
        (color * (a * color + b)) /
        (color * (c * color + d) + e);

    // Gamma correct.
    const float rcp_gamma = 1.0f / m_gamma;
    color = Color3f(
        pow(color[0], rcp_gamma),
        pow(color[1], rcp_gamma),
        pow(color[2], rcp_gamma));
}

//
// AcesUnrealApplier class implementation.
//

AcesUnrealApplier::AcesUnrealApplier()
{
}

void AcesUnrealApplier::tone_map(Color3f& color) const
{
    //
    // Apply the color grading curve from Unreal 3,
    // adapted by Romain Guy to be close to the ACES curve.
    //
    // Note: gamma 2.2 correction is baked in.
    //
    // Reference:
    //
    //   https://www.shadertoy.com/view/llXyWr
    //

    color = color / (color + Color3f(0.155f)) * 1.019f;
}

//
// FilmicHejlApplier class implementation.
//

FilmicHejlApplier::FilmicHejlApplier()
{
}

void FilmicHejlApplier::tone_map(Color3f& color) const
{
    //
    // Apply a filmic tone mapper developed by Jim Hejl and Richard Burgess-Dawson at EA.
    //
    // References:
    //
    //   http://filmicworlds.com/blog/filmic-tonemapping-operators/
    //   https://de.slideshare.net/hpduiker/filmic-tonemapping-for-realtime-rendering-siggraph-2010-color-course
    //

    color = component_wise_max(Color3f(0.0f), color - Color3f(0.004f));
    color =
        (color * (6.2f * color + Color3f(0.5f))) /
        (color * (6.2f * color + Color3f(1.7f)) + Color3f(0.06f));
}

//
// Filmic (Uncharted)
//

FilmicUnchartedApplier::FilmicUnchartedApplier(
    const float     gamma,
    const float     A,
    const float     B,
    const float     C,
    const float     D,
    const float     E,
    const float     F,
    const float     W,
    const float     exposure_bias)
  : m_gamma(gamma)
  , m_A(A)
  , m_B(B)
  , m_C(C)
  , m_D(D)
  , m_E(E)
  , m_F(F)
  , m_W(W)
  , m_exposure_bias(exposure_bias)
{
}

inline float FilmicUnchartedApplier::uncharted_tone_map(const float x) const
{
    return ((x * (m_A * x + m_C * m_B) + m_D * m_E) /
            (x * (m_A * x + m_B) + m_D * m_F)) - m_E / m_F;
}

inline Color3f FilmicUnchartedApplier::uncharted_tone_map(const Color3f& x) const
{
    return ((x * (m_A * x + Color3f(m_C * m_B)) + Color3f(m_D * m_E)) /
            (x * (m_A * x + Color3f(m_B)) + Color3f(m_D * m_F))) - Color3f(m_E / m_F);
}

void FilmicUnchartedApplier::tone_map(Color3f& color) const
{
    //
    // Apply a filmic tone mapper developed by John Hable for Uncharted 2.
    // ((x * (A * x + C * B) + D * E) / (x * (A * x + B) + D * F)) - E / F
    //
    // References:
    //
    //   http://filmicworlds.com/blog/filmic-tonemapping-operators/
    //   http://filmicworlds.com/blog/filmic-tonemapping-with-piecewise-power-curves/
    //   https://www.slideshare.net/ozlael/hable-john-uncharted2-hdr-lighting (see slides 141-142)
    //

    color *= m_exposure_bias;

    color = uncharted_tone_map(color);

    const Color3f white_scale(1.0f / uncharted_tone_map(m_W));
    color *= white_scale;

    // Gamma correct.
    const float rcp_gamma = 1.0f / m_gamma;
    color = Color3f(
        pow(color[0], rcp_gamma),
        pow(color[1], rcp_gamma),
        pow(color[2], rcp_gamma));
}

//
// ReinhardApplier class implementation.
//

ReinhardApplier::ReinhardApplier(const float gamma, const bool use_luminance)
  : m_gamma(gamma)
  , m_use_luminance(use_luminance)
{
}

void ReinhardApplier::tone_map(Color3f& color) const
{
    //
    // Apply Reinhard's simple tone mapping operator (Eq. 3).
    //
    // Reference:
    //
    //   http://www.cmap.polytechnique.fr/~peyre/cours/x2005signal/hdr_photographic.pdf
    //

    if (m_use_luminance)
    {
        color /= 1.0f + luminance(color);

        //
        // Note: this is equivalent to the paper's implementation:
        //
        // const float L = luminance(color);     // world luminance
        // const float Ld = L / (1.0f + L);      // display luminance
        // color = Ld * color / L;
        //
    }
    else
    {
        color /= Color3f(1.0f) + color;
    }

    // Gamma correct.
    const float rcp_gamma = 1.0f / m_gamma;
    color = Color3f(
        pow(color[0], rcp_gamma),
        pow(color[1], rcp_gamma),
        pow(color[2], rcp_gamma));
}

//
// ReinhardExtendedApplier class implementation.
//

ReinhardExtendedApplier::ReinhardExtendedApplier(
    const float gamma,
    const float max_white,
    const bool use_luminance)
  : m_gamma(gamma)
  , m_max_white(max_white)
  , m_use_luminance(use_luminance)
{
}

void ReinhardExtendedApplier::tone_map(Color3f& color) const
{
    //
    // Apply Reinhard's extended tone mapping operator (Eq. 4).
    //
    // Note that we use Lwhite = Lmax to avoid burn-out, where:
    //   * Lwhite is the smallest luminance that will be mapped to pure white
    //   * Lmax is the maximum luminance in the scene
    //
    // Reference:
    //
    //   http://www.cmap.polytechnique.fr/~peyre/cours/x2005signal/hdr_photographic.pdf
    //

    if (m_use_luminance)
    {
        const float L = luminance(color);
        const float Ld = (L * (1.0f + L / (m_max_white * m_max_white))) / (1.0f + L);
        color = Ld * color / L;
    }
    else
    {
        color = (color * (Color3f(1.0f) + color / (m_max_white * m_max_white))) /
                (Color3f(1.0f) + color);
    }

    // Gamma correct.
    const float rcp_gamma = 1.0f / m_gamma;
    color = Color3f(
        pow(color[0], rcp_gamma),
        pow(color[1], rcp_gamma),
        pow(color[2], rcp_gamma));
}

}   // namespace renderer
