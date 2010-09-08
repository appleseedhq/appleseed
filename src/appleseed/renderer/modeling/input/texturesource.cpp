
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "texturesource.h"

// appleseed.renderer headers.
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/input/inputparams.h"
#include "renderer/modeling/scene/textureinstance.h"

// appleseed.foundation headers.
#include "foundation/math/hash.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <algorithm>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// TextureSource class implementation.
//

// If defined, render each tile with a solid, unique color.
#undef RENDERER_DISPLAY_TEXTURE_TILES

namespace
{
    // Compute a color from a given integer value.
    template <typename T>
    inline Color4f integer_to_color(const T i)
    {
        const uint32 u = static_cast<uint32>(i);    // keep the low 32 bits

        const uint32 x = hashint32(u);
        const uint32 y = hashint32(u + 1);
        const uint32 z = hashint32(u + 2);

        return
            Color4f(
                static_cast<float>(x) * (1.0f / 4294967295.0f),
                static_cast<float>(y) * (1.0f / 4294967295.0f),
                static_cast<float>(z) * (1.0f / 4294967295.0f),
                1.0f);
    }

    // Apply an addressing mode to texture coordinates.
    inline void apply_addressing_mode(
        const TextureAddressingMode addressing_mode,
        Vector2d&                   p)
    {
        switch (addressing_mode)
        {
          case TextureAddressingClamp:
            p.x = saturate(p.x);
            p.y = saturate(p.y);
            break;
          case TextureAddressingWrap:
            p.x = wrap(p.x);
            p.y = wrap(p.y);
            break;
          default:
            assert(!"Wrong texture addressing mode.");
        }
    }

    // Constrain (integer) pixel coordinates to a canvas.
    inline Vector<size_t, 2> constrain_to_canvas(
        const TextureAddressingMode addressing_mode,
        const size_t                canvas_width,
        const size_t                canvas_height,
        int                         ix,
        int                         iy)
    {
        const int max_x = static_cast<int>(canvas_width - 1);
        const int max_y = static_cast<int>(canvas_height - 1);

        switch (addressing_mode)
        {
          case TextureAddressingClamp:
            if (ix < 0) ix = 0;
            if (iy < 0) iy = 0;
            if (ix > max_x) ix = max_x;
            if (iy > max_y) iy = max_y;
            break;
          case TextureAddressingWrap:
            if (ix < 0) ix = max_x;
            if (iy < 0) iy = max_y;
            if (ix > max_x) ix = 0;
            if (iy > max_y) iy = 0;
            break;
          default:
            assert(!"Wrong texture addressing mode.");
        }

        return
            Vector<size_t, 2>(
                static_cast<size_t>(ix),
                static_cast<size_t>(iy));
    }

    // Utility function to sample a tile.
    inline void sample_tile(
        TextureCache&   texture_cache,
        const UniqueID  assembly_uid,
        const size_t    texture_index,
        const size_t    tile_x,
        const size_t    tile_y,
        const size_t    pixel_x,
        const size_t    pixel_y,
        Color4f&        sample)
    {
        // Retrieve the tile.
        const Tile& tile =
            texture_cache.get(
                assembly_uid,
                texture_index,
                tile_x,
                tile_y);

        // Sample the tile.
        if (tile.get_channel_count() == 3)
        {
            Color3f rgb;
            tile.get_pixel(pixel_x, pixel_y, rgb);
            sample[0] = rgb[0];
            sample[1] = rgb[1];
            sample[2] = rgb[2];
            sample[3] = 1.0f;
        }
        else tile.get_pixel(pixel_x, pixel_y, sample);
    }
}

// Constructor.
TextureSource::TextureSource(
    const UniqueID              assembly_uid,
    const TextureInstance&      texture_instance,
    const CanvasProperties&     texture_props)
  : Source(false)
  , m_assembly_uid(assembly_uid)
  , m_texture_index(texture_instance.get_texture_index())
  , m_addressing_mode(texture_instance.get_addressing_mode())
  , m_filtering_mode(texture_instance.get_filtering_mode())
  , m_multiplier(texture_instance.get_multiplier())
  , m_lighting_conditions(          // todo: this should be user-settable
        IlluminantCIED65,
        XYZCMFCIE196410Deg)
  , m_texture_props(texture_props)
{
}

// Retrieve a given texel. Return a color in the linear RGB color space.
Color4f TextureSource::get_texel(
    TextureCache&               texture_cache,
    const int                   ix,
    const int                   iy) const
{
    const Vector<size_t, 2> p =
        constrain_to_canvas(
            m_addressing_mode,
            m_texture_props.m_canvas_width,
            m_texture_props.m_canvas_height,
            ix,
            iy);

    assert(p.x >= 0);
    assert(p.y >= 0);
    assert(p.x < m_texture_props.m_canvas_width);
    assert(p.y < m_texture_props.m_canvas_height);

    // Compute the coordinates of the tile containing the texel (x, y).
    const size_t tile_x = truncate<size_t>(p.x * m_texture_props.m_rcp_tile_width);
    const size_t tile_y = truncate<size_t>(p.y * m_texture_props.m_rcp_tile_height);
    assert(tile_x < m_texture_props.m_tile_count_x);
    assert(tile_y < m_texture_props.m_tile_count_y);

#ifdef RENDERER_DISPLAY_TEXTURE_TILES

    return
        integer_to_color(
            mix32(
                static_cast<uint32>(m_assembly_uid),
                static_cast<uint32>(m_texture_index),
                static_cast<uint32>(tile_x),
                static_cast<uint32>(tile_y)));

#endif

    // Compute the tile space coordinates of the texel (x, y).
    const size_t pixel_x = p.x - tile_x * m_texture_props.m_tile_width;
    const size_t pixel_y = p.y - tile_y * m_texture_props.m_tile_height;
    assert(pixel_x < m_texture_props.m_tile_width);
    assert(pixel_y < m_texture_props.m_tile_height);

    // Sample the tile.
    Color4f sample;
    sample_tile(
        texture_cache,
        m_assembly_uid,
        m_texture_index,
        tile_x,
        tile_y,
        pixel_x,
        pixel_y,
        sample);

    return sample;
}

// Retrieve a 2x2 block of texels. Texels are expressed in the linear RGB color space.
void TextureSource::get_texels_2x2(
    TextureCache&               texture_cache,
    const int                   ix,
    const int                   iy,
    Color4f&                    sample_00,
    Color4f&                    sample_10,
    Color4f&                    sample_01,
    Color4f&                    sample_11) const
{
    const Vector<size_t, 2> p00 =
        constrain_to_canvas(
            m_addressing_mode,
            m_texture_props.m_canvas_width,
            m_texture_props.m_canvas_height,
            ix + 0,
            iy + 0);
    const Vector<size_t, 2> p11 =
        constrain_to_canvas(
            m_addressing_mode,
            m_texture_props.m_canvas_width,
            m_texture_props.m_canvas_height,
            ix + 1,
            iy + 1);
    const Vector<size_t, 2> p10(p11.x, p00.y);
    const Vector<size_t, 2> p01(p00.x, p11.y);

    // Compute the coordinates of the tile containing each texel.
    const size_t tile_x_00 = truncate<size_t>(p00.x * m_texture_props.m_rcp_tile_width);
    const size_t tile_y_00 = truncate<size_t>(p00.y * m_texture_props.m_rcp_tile_height);
    const size_t tile_x_11 = truncate<size_t>(p11.x * m_texture_props.m_rcp_tile_width);
    const size_t tile_y_11 = truncate<size_t>(p11.y * m_texture_props.m_rcp_tile_height);

    // Check whether all four texels are part of the same tile.
    const size_t tile_x_mask = tile_x_00 ^ tile_x_11;
    const size_t tile_y_mask = tile_y_00 ^ tile_y_11;

    if (tile_x_mask | tile_y_mask)
    {
        // Compute the tile space coordinates of each texel.
        const size_t pixel_x_00 = p00.x - tile_x_00 * m_texture_props.m_tile_width;
        const size_t pixel_y_00 = p00.y - tile_y_00 * m_texture_props.m_tile_height;
        const size_t pixel_x_11 = p11.x - tile_x_11 * m_texture_props.m_tile_width;
        const size_t pixel_y_11 = p11.y - tile_y_11 * m_texture_props.m_tile_height;

        // Sample the tile.
        sample_tile(
            texture_cache,
            m_assembly_uid,
            m_texture_index,
            tile_x_00,
            tile_y_00,
            pixel_x_00,
            pixel_y_00,
            sample_00);
        sample_tile(
            texture_cache,
            m_assembly_uid,
            m_texture_index,
            tile_x_11,
            tile_y_00,
            pixel_x_11,
            pixel_y_00,
            sample_10);
        sample_tile(
            texture_cache,
            m_assembly_uid,
            m_texture_index,
            tile_x_00,
            tile_y_11,
            pixel_x_00,
            pixel_y_11,
            sample_01);
        sample_tile(
            texture_cache,
            m_assembly_uid,
            m_texture_index,
            tile_x_11,
            tile_y_11,
            pixel_x_11,
            pixel_y_11,
            sample_11);
    }
    else
    {
        // Compute the tile space coordinates of each texel.
        const size_t org_x = tile_x_00 * m_texture_props.m_tile_width;
        const size_t org_y = tile_y_00 * m_texture_props.m_tile_height;
        const size_t pixel_x_00 = p00.x - org_x;
        const size_t pixel_y_00 = p00.y - org_y;
        const size_t pixel_x_11 = p11.x - org_x;
        const size_t pixel_y_11 = p11.y - org_y;

        // Retrieve the tile.
        const Tile& tile =
            texture_cache.get(
                m_assembly_uid,
                m_texture_index,
                tile_x_00,
                tile_y_00);

        // Sample the tile.
        if (tile.get_channel_count() == 3)
        {
            Color3f rgb;
            tile.get_pixel(pixel_x_00, pixel_y_00, rgb);
            sample_00[0] = rgb[0];
            sample_00[1] = rgb[1];
            sample_00[2] = rgb[2];
            sample_00[3] = 1.0f;
            tile.get_pixel(pixel_x_11, pixel_y_00, rgb);
            sample_10[0] = rgb[0];
            sample_10[1] = rgb[1];
            sample_10[2] = rgb[2];
            sample_10[3] = 1.0f;
            tile.get_pixel(pixel_x_00, pixel_y_11, rgb);
            sample_01[0] = rgb[0];
            sample_01[1] = rgb[1];
            sample_01[2] = rgb[2];
            sample_01[3] = 1.0f;
            tile.get_pixel(pixel_x_11, pixel_y_11, rgb);
            sample_11[0] = rgb[0];
            sample_11[1] = rgb[1];
            sample_11[2] = rgb[2];
            sample_11[3] = 1.0f;
        }
        else
        {
            tile.get_pixel(pixel_x_00, pixel_y_00, sample_00);
            tile.get_pixel(pixel_x_11, pixel_y_00, sample_10);
            tile.get_pixel(pixel_x_00, pixel_y_11, sample_01);
            tile.get_pixel(pixel_x_11, pixel_y_11, sample_11);
        }
    }
}

// Sample the texture. Return a color in the linear RGB color space.
Color4f TextureSource::sample_texture(
    TextureCache&               texture_cache,
    const InputParams&          params) const
{
    // Fetch the texture coordinates.
    Vector2d p = params.m_uv;

    // Apply the texture addressing mode.
    apply_addressing_mode(m_addressing_mode, p);

    // Transform the texture coordinates to image coordinates.
    p.y = 1.0 - p.y;
    p.x *= m_texture_props.m_canvas_width;
    p.y *= m_texture_props.m_canvas_height;

    switch (m_filtering_mode)
    {
      case TextureFilteringNearest:
        {
            const int ix = truncate<int>(p.x);
            const int iy = truncate<int>(p.y);

            return get_texel(texture_cache, ix, iy);
        }

      case TextureFilteringBilinear:
        {
            p.x -= 0.5;
            p.y -= 0.5;

            const int ix = truncate<int>(floor(p.x));
            const int iy = truncate<int>(floor(p.y));

            // Retrieve the four surrounding texels.
            Color4f t00, t10, t01, t11;
            get_texels_2x2(
                texture_cache,
                ix, iy,
                t00, t10, t01, t11);

            // Compute weights.
            float wx1 = static_cast<float>(p.x - ix);
            float wy1 = static_cast<float>(p.y - iy);
            float wx0 = 1.0f - wx1;
            float wy0 = 1.0f - wy1;

            // Apply weights.
            t00 *= wx0 * wy0;
            t10 *= wx1 * wy0;
            t01 *= wx0 * wy1;
            t11 *= wx1 * wy1;

            // Accumulate.
            t00 += t10;
            t00 += t01;
            t00 += t11;

            return t00;
        }

      default:
        assert(!"Wrong texture filtering mode.");
        return Color4f(0.0f);
    }
}

}   // namespace renderer
