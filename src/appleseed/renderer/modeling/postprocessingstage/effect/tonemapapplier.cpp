
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

ToneMapApplier::ToneMapApplier(const ToneMapFunction tone_map)
  : tone_map(tone_map)
{
}

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
            Color4f pixel;
            tile.get_pixel(x, y, pixel);

            // FIXME unpremultiply, tonemap, saturate, then premultiply
            //       as in conversion.cpp (also check if it is Color3f)
            tone_map(pixel.rgb());

            tile.set_pixel(x, y, saturate(pixel));
        }
    }
}


//
// AcesUnrealApplier class implementation.
//

AcesUnrealApplier::AcesUnrealApplier()
  : ToneMapApplier(
      [](Color3f& color)
      {
          color = color / (color + Color3f(0.155f)) * 1.019f;
      })
{
}


//
// FilmicHejlApplier class implementation.
//

FilmicHejlApplier::FilmicHejlApplier()
  : ToneMapApplier(
      [](Color3f& color)
      {
          //
          // Apply a filmic tone mapper developed by Jim Hejl and Richard Burgess-Dawson at EA.
          //
          // References:
          //
          //   "Filmic Tonemapping Operators", John Hable
          //   http://filmicworlds.com/blog/filmic-tonemapping-operators/
          //
          //   "Filmic Tonemapping for Real-time Rendering", Haarm-Pieter Duiker
          //   https://de.slideshare.net/hpduiker/filmic-tonemapping-for-realtime-rendering-siggraph-2010-color-course
          //

          color = component_wise_max(Color3f(0.0f), color - Color3f(0.004f));
          color =
            (color * (6.2f * color + Color3f(0.5f))) /
            (color * (6.2f * color + Color3f(1.7f)) + Color3f(0.06f));
      })
{
}


//
// ReinhardApplier class implementation.
//

ReinhardApplier::ReinhardApplier(float gamma)
  : m_gamma(gamma)
  , ToneMapApplier(
      [gamma](Color3f& color)
      {
          //
          // Apply Reinhard's simple tone mapping operator (Eq. 3).
          //
          // Reference:
          //
          //   "Photographic Tone Reproduction for Digital Images", Reinhard et. al
          //   http://www.cmap.polytechnique.fr/~peyre/cours/x2005signal/hdr_photographic.pdf
          //

          const float L = luminance(color);     // world luminance
          const float Ld = L / (1.0f + L);      // display luminance
          color = Ld * color / L;

          // Gamma correct.
          const float rcp_gamma = 1.0f / gamma;
          color = Color3f(
              pow(color[0], rcp_gamma),
              pow(color[1], rcp_gamma),
              pow(color[2], rcp_gamma));
      })
{
}


//
// ReinhardExtendedApplier class implementation.
//

ReinhardExtendedApplier::ReinhardExtendedApplier(float gamma, float  max_white)
  : m_gamma(gamma)
  , m_max_white(max_white)
  , ToneMapApplier(
      [gamma, max_white](Color3f& color)
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
          //   "Photographic Tone Reproduction for Digital Images", Reinhard et. al
          //   http://www.cmap.polytechnique.fr/~peyre/cours/x2005signal/hdr_photographic.pdf
          //

          const float L = luminance(color);
          const float Ld = (L * (1.0f + L / (max_white * max_white))) / (1.0f + L);
          color = Ld * color / L;

          // Gamma correct.
          const float rcp_gamma = 1.0f / gamma;
          color = Color3f(
              pow(color[0], rcp_gamma),
              pow(color[1], rcp_gamma),
              pow(color[2], rcp_gamma));
      })
{
}

}   // namespace renderer
