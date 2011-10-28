
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "texturecache.h"

// appleseed.renderer headers.
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/cache.h"

// appleseed.foundation headers.
#include "foundation/utility/memory.h"
#include "foundation/utility/string.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
//  TextureCache class implementation.
//

namespace
{
    // Convert a tile from the sRGB color space to the linear RGB color space.
    void convert_tile_srgb_to_linear_rgb(Tile& tile)
    {
        const size_t pixel_count = tile.get_pixel_count();
        const size_t channel_count = tile.get_channel_count();
        assert(channel_count == 3 || channel_count == 4);

        if (channel_count == 3)
        {
            for (size_t i = 0; i < pixel_count; ++i)
            {
                Color3f color;
                tile.get_pixel(i, color);
                tile.set_pixel(i, srgb_to_linear_rgb(color));
            }
        }
        else
        {
            for (size_t i = 0; i < pixel_count; ++i)
            {
                Color4f color;
                tile.get_pixel(i, color);
                color.rgb() = srgb_to_linear_rgb(color.rgb());
                tile.set_pixel(i, color);
            }
        }
    }

    // Convert a tile from the CIE XYZ color space to the linear RGB color space.
    void convert_tile_ciexyz_to_linear_rgb(Tile& tile)
    {
        const size_t pixel_count = tile.get_pixel_count();
        const size_t channel_count = tile.get_channel_count();
        assert(channel_count == 3 || channel_count == 4);

        if (channel_count == 3)
        {
            for (size_t i = 0; i < pixel_count; ++i)
            {
                Color3f color;
                tile.get_pixel(i, color);
                tile.set_pixel(i, ciexyz_to_linear_rgb(color));
            }
        }
        else
        {
            for (size_t i = 0; i < pixel_count; ++i)
            {
                Color4f color;
                tile.get_pixel(i, color);
                color.rgb() = ciexyz_to_linear_rgb(color.rgb());
                tile.set_pixel(i, color);
            }
        }
    }
}

TextureCache::TextureCache(
    const Scene&    scene,
    const size_t    memory_limit)
  : m_tile_swapper(scene, memory_limit)
  , m_tile_cache(m_tile_key_hasher, m_tile_swapper, TileKey::invalid())
{
}

TextureCache::~TextureCache()
{
    print_dual_stage_cache_stats(m_tile_cache, "texture cache statistics");
}


//
// TextureCache::TileSwapper class implementation.
//

TextureCache::TileSwapper::TileSwapper(
    const Scene&    scene,
    const size_t    memory_limit)
  : m_scene(scene)
  , m_memory_limit(memory_limit)
  , m_memory_size(0)
{
    assert(m_memory_limit > 0);
}

void TextureCache::TileSwapper::load(const TileKey& key, TilePtr& tile)
{
    // Fetch the texture container.
    const TextureContainer& textures =
        key.m_assembly_uid == ~UniqueID(0)
            ? m_scene.textures()
            : m_scene.assemblies().get_by_uid(key.m_assembly_uid)->textures();

    // Fetch the texture.
    assert(key.m_texture_index < textures.size());
    Texture* texture = textures.get_by_index(key.m_texture_index);

/*
    RENDERER_LOG_DEBUG(
        "loading tile (" FMT_SIZE_T ", " FMT_SIZE_T ") "
        "from texture \"%s\"",
        key.m_tile_x,
        key.m_tile_y,
        texture->get_name());
*/

    // Load the tile.
    tile = texture->load_tile(key.m_tile_x, key.m_tile_y);

    // Convert the tile to the linear RGB color space.
    switch (texture->get_color_space())
    {
      case ColorSpaceLinearRGB:
        break;

      case ColorSpaceSRGB:
        convert_tile_srgb_to_linear_rgb(*tile);
        break;

      case ColorSpaceCIEXYZ:
        convert_tile_ciexyz_to_linear_rgb(*tile);
        break;

      assert_otherwise;
    }

    // Track the amount of memory used by the tile cache.
    m_memory_size += dynamic_sizeof(*tile);
}

void TextureCache::TileSwapper::unload(const TileKey& key, TilePtr& tile)
{
    // Track the amount of memory used by the tile cache.
    const size_t tile_memory_size = dynamic_sizeof(*tile);
    assert(m_memory_size >= tile_memory_size);
    m_memory_size -= tile_memory_size;

    // Fetch the texture container.
    const TextureContainer& textures =
        key.m_assembly_uid == ~UniqueID(0)
            ? m_scene.textures()
            : m_scene.assemblies().get_by_uid(key.m_assembly_uid)->textures();

    // Fetch the texture.
    assert(key.m_texture_index < textures.size());
    Texture* texture = textures.get_by_index(key.m_texture_index);

/*
    RENDERER_LOG_DEBUG(
        "unloading tile (" FMT_SIZE_T ", " FMT_SIZE_T ") "
        "from texture \"%s\"",
        key.m_tile_x,
        key.m_tile_y,
        texture->get_name());
*/

    // Unload the tile.
    texture->unload_tile(key.m_tile_x, key.m_tile_y, tile);
}

bool TextureCache::TileSwapper::is_full(const size_t element_count) const
{
/*
    RENDERER_LOG_DEBUG(
        "texture cache contains %s %s (%s)",
        pretty_uint(element_count).c_str(),
        plural(element_count, "tile").c_str(),
        pretty_size(m_memory_size).c_str());
*/

    return m_memory_size >= m_memory_limit;
}

}   // namespace renderer
