
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/hash/hash.h"
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

//
// TextureSource class implementation.
//

// If defined, render each tile with a unique, solid color.
#undef DEBUG_DISPLAY_TEXTURE_TILES

namespace
{
    // Apply an addressing mode to texture coordinates.
    inline void apply_addressing_mode(
        const TextureAddressingMode addressing_mode,
        Vector2f&                   p)
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
        TextureCache&               texture_cache,
        const UniqueID              assembly_uid,
        const UniqueID              texture_uid,
        const size_t                tile_x,
        const size_t                tile_y,
        const size_t                pixel_x,
        const size_t                pixel_y,
        Color4f&                    sample)
    {
        // Retrieve the tile.
        const Tile& tile =
            texture_cache.get(
                assembly_uid,
                texture_uid,
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

TextureSource::TextureSource(
    const UniqueID              assembly_uid,
    const TextureInstance&      texture_instance)
  : Source(false)
  , m_assembly_uid(assembly_uid)
  , m_texture_instance(texture_instance)
  , m_texture_uid(texture_instance.get_texture().get_uid())
  , m_texture_props(texture_instance.get_texture().properties())
  , m_texture_transform(texture_instance.get_transform())
  , m_scalar_canvas_width(static_cast<float>(m_texture_props.m_canvas_width))
  , m_scalar_canvas_height(static_cast<float>(m_texture_props.m_canvas_height))
  , m_max_x(static_cast<float>(m_texture_props.m_canvas_width - 1))
  , m_max_y(static_cast<float>(m_texture_props.m_canvas_height - 1))
{
}

std::uint64_t TextureSource::compute_signature() const
{
    return m_texture_instance.compute_signature();
}

TextureSource::Hints TextureSource::get_hints() const
{
    Hints hints;
    hints.m_width = m_texture_props.m_canvas_width;
    hints.m_height = m_texture_props.m_canvas_height;
    return hints;
}

Vector2f TextureSource::apply_transform(const Vector2f& uv) const
{
    // Convert to 3D coordinates.
    Vector3f p(uv.x, uv.y, 0.0f);

    // Apply transform.
    p = m_texture_transform.point_to_local(p);

    // Convert back to 2D coordinates.
    return Vector2f(p.x, p.y);
}

Color4f TextureSource::get_texel(
    TextureCache&               texture_cache,
    const size_t                ix,
    const size_t                iy) const
{
    assert(ix < m_texture_props.m_canvas_width);
    assert(iy < m_texture_props.m_canvas_height);

    // Compute the coordinates of the tile containing the texel (x, y).
    const size_t tile_x = truncate<size_t>(ix * m_texture_props.m_rcp_tile_width);
    const size_t tile_y = truncate<size_t>(iy * m_texture_props.m_rcp_tile_height);
    assert(tile_x < m_texture_props.m_tile_count_x);
    assert(tile_y < m_texture_props.m_tile_count_y);

#ifdef DEBUG_DISPLAY_TEXTURE_TILES

    return
        Color4f(
            integer_to_color3(
                mix_uint32(
                    static_cast<std::uint32_t>(m_assembly_uid),
                    static_cast<std::uint32_t>(m_texture_uid),
                    static_cast<std::uint32_t>(tile_x),
                    static_cast<std::uint32_t>(tile_y))),
            1.0f);

#endif

    // Compute the tile space coordinates of the texel (x, y).
    const size_t pixel_x = ix - tile_x * m_texture_props.m_tile_width;
    const size_t pixel_y = iy - tile_y * m_texture_props.m_tile_height;
    assert(pixel_x < m_texture_props.m_tile_width);
    assert(pixel_y < m_texture_props.m_tile_height);

    // Sample the tile.
    Color4f sample;
    sample_tile(
        texture_cache,
        m_assembly_uid,
        m_texture_uid,
        tile_x,
        tile_y,
        pixel_x,
        pixel_y,
        sample);

    return sample;
}

void TextureSource::get_texels_2x2(
    TextureCache&               texture_cache,
    const int                   ix,
    const int                   iy,
    Color4f&                    t00,
    Color4f&                    t10,
    Color4f&                    t01,
    Color4f&                    t11) const
{
    const Vector<size_t, 2> p00 =
        constrain_to_canvas(
            m_texture_instance.get_addressing_mode(),
            m_texture_props.m_canvas_width,
            m_texture_props.m_canvas_height,
            ix + 0,
            iy + 0);

    const Vector<size_t, 2> p11 =
        constrain_to_canvas(
            m_texture_instance.get_addressing_mode(),
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
        // Not all four texels are part of the same tile.

        // Compute the tile space coordinates of each texel.
        const size_t pixel_x_00 = p00.x - tile_x_00 * m_texture_props.m_tile_width;
        const size_t pixel_y_00 = p00.y - tile_y_00 * m_texture_props.m_tile_height;
        const size_t pixel_x_11 = p11.x - tile_x_11 * m_texture_props.m_tile_width;
        const size_t pixel_y_11 = p11.y - tile_y_11 * m_texture_props.m_tile_height;

        // Sample the tile.
        sample_tile(texture_cache, m_assembly_uid, m_texture_uid, tile_x_00, tile_y_00, pixel_x_00, pixel_y_00, t00);
        sample_tile(texture_cache, m_assembly_uid, m_texture_uid, tile_x_11, tile_y_00, pixel_x_11, pixel_y_00, t10);
        sample_tile(texture_cache, m_assembly_uid, m_texture_uid, tile_x_00, tile_y_11, pixel_x_00, pixel_y_11, t01);
        sample_tile(texture_cache, m_assembly_uid, m_texture_uid, tile_x_11, tile_y_11, pixel_x_11, pixel_y_11, t11);
    }
    else
    {
        // All four texels are part of the same tile.

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
                m_texture_uid,
                tile_x_00,
                tile_y_00);

        // Sample the tile.
        if (tile.get_channel_count() == 3)
        {
            Color3f rgb;

            tile.get_pixel(pixel_x_00, pixel_y_00, rgb);
            t00[0] = rgb[0];
            t00[1] = rgb[1];
            t00[2] = rgb[2];
            t00[3] = 1.0f;

            tile.get_pixel(pixel_x_11, pixel_y_00, rgb);
            t10[0] = rgb[0];
            t10[1] = rgb[1];
            t10[2] = rgb[2];
            t10[3] = 1.0f;

            tile.get_pixel(pixel_x_00, pixel_y_11, rgb);
            t01[0] = rgb[0];
            t01[1] = rgb[1];
            t01[2] = rgb[2];
            t01[3] = 1.0f;

            tile.get_pixel(pixel_x_11, pixel_y_11, rgb);
            t11[0] = rgb[0];
            t11[1] = rgb[1];
            t11[2] = rgb[2];
            t11[3] = 1.0f;
        }
        else
        {
            tile.get_pixel(pixel_x_00, pixel_y_00, t00);
            tile.get_pixel(pixel_x_11, pixel_y_00, t10);
            tile.get_pixel(pixel_x_00, pixel_y_11, t01);
            tile.get_pixel(pixel_x_11, pixel_y_11, t11);
        }
    }
}

Color4f TextureSource::sample_texture(
    TextureCache&               texture_cache,
    const Vector2f&             uv) const
{
    // Start with the transformed input texture coordinates.
    Vector2f p = apply_transform(uv);
    p.y = 1.0f - p.y;

    // Apply the texture addressing mode.
    apply_addressing_mode(m_texture_instance.get_addressing_mode(), p);

    switch (m_texture_instance.get_filtering_mode())
    {
      case TextureFilteringNearest:
        {
            p.x = clamp(p.x * m_scalar_canvas_width, 0.0f, m_max_x);
            p.y = clamp(p.y * m_scalar_canvas_height, 0.0f, m_max_y);

            const size_t ix = truncate<size_t>(p.x);
            const size_t iy = truncate<size_t>(p.y);

            return get_texel(texture_cache, ix, iy);
        }

      case TextureFilteringBilinear:
        {
            p.x *= m_max_x;
            p.y *= m_max_y;

            const int ix = truncate<int>(p.x);
            const int iy = truncate<int>(p.y);

            // Retrieve the four surrounding texels.
            Color4f t00, t10, t01, t11;
            get_texels_2x2(
                texture_cache,
                ix, iy,
                t00, t10, t01, t11);

            // Compute weights.
            const float wx1 = p.x - ix;
            const float wy1 = p.y - iy;
            const float wx0 = 1.0f - wx1;
            const float wy0 = 1.0f - wy1;

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
