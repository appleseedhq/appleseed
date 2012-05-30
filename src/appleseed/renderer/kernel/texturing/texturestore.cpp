
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "texturestore.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/tile.h"
#include "foundation/platform/types.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <string>

using namespace foundation;
using namespace std;

#undef TRACK_TILE_LOADING
#undef TRACK_TILE_UNLOADING
#undef TRACK_CACHE_SIZE

namespace renderer
{

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

TextureStore::TextureStore(
    const Scene&    scene,
    const size_t    memory_limit)
  : m_tile_swapper(scene, memory_limit)
  , m_tile_cache(m_tile_swapper)
{
}

TextureStore::~TextureStore()
{
    RENDERER_LOG_DEBUG(
        "texture store statistics:\n"
        "  cache            %s\n"
        "  peak size        %s\n",
        format_cache_stats(m_tile_cache).c_str(),
        pretty_size(m_tile_swapper.m_max_memory_size).c_str());
}

TextureStore::TileSwapper::TileSwapper(
    const Scene&    scene,
    const size_t    memory_limit)
  : m_scene(scene)
  , m_memory_limit(memory_limit)
  , m_memory_size(0)
  , m_max_memory_size(0)
{
    assert(m_memory_limit > 0);
}

void TextureStore::TileSwapper::load(const TileKey& key, TileRecord& record)
{
    // Fetch the texture container.
    const TextureContainer& textures =
        key.m_assembly_uid == ~0
            ? m_scene.textures()
            : m_scene.assemblies().get_by_uid(key.m_assembly_uid)->textures();

    // Fetch the texture.
    const size_t texture_index = key.get_texture_index();
    assert(texture_index < textures.size());
    Texture* texture = textures.get_by_index(texture_index);

#ifdef TRACK_TILE_LOADING
    RENDERER_LOG_DEBUG(
        "loading tile (" FMT_SIZE_T ", " FMT_SIZE_T ") "
        "from texture \"%s\"...",
        key.get_tile_x(),
        key.get_tile_y(),
        texture->get_name());
#endif

    // Load the tile.
    record.m_tile = texture->load_tile(key.get_tile_x(), key.get_tile_y());
    record.m_owners = 0;

    // Convert the tile to the linear RGB color space.
    switch (texture->get_color_space())
    {
      case ColorSpaceLinearRGB:
        break;

      case ColorSpaceSRGB:
        convert_tile_srgb_to_linear_rgb(*record.m_tile);
        break;

      case ColorSpaceCIEXYZ:
        convert_tile_ciexyz_to_linear_rgb(*record.m_tile);
        break;

      assert_otherwise;
    }

    // Track the amount of memory used by the tile cache.
    m_memory_size += dynamic_sizeof(*record.m_tile);
    m_max_memory_size = max(m_max_memory_size, m_memory_size);

#ifdef TRACK_CACHE_SIZE
    if (m_memory_size > m_memory_limit)
    {
        RENDERER_LOG_DEBUG(
            "texture store size is %s, exceeding capacity %s by %s",
            pretty_size(m_memory_size).c_str(),
            pretty_size(m_memory_limit).c_str(),
            pretty_size(m_memory_size - m_memory_limit).c_str());
    }
    else
    {
        RENDERER_LOG_DEBUG(
            "texture store size is %s, below capacity %s by %s",
            pretty_size(m_memory_size).c_str(),
            pretty_size(m_memory_limit).c_str(),
            pretty_size(m_memory_size - m_memory_limit).c_str());
    }
#endif
}

bool TextureStore::TileSwapper::unload(const TileKey& key, TileRecord& record)
{
    // Cannot unload tiles that are still in use.
    if (boost_atomic::atomic_read32(&record.m_owners) > 0)
        return false;

    // Track the amount of memory used by the tile cache.
    const size_t tile_memory_size = dynamic_sizeof(*record.m_tile);
    assert(m_memory_size >= tile_memory_size);
    m_memory_size -= tile_memory_size;

    // Fetch the texture container.
    const TextureContainer& textures =
        key.m_assembly_uid == ~0
            ? m_scene.textures()
            : m_scene.assemblies().get_by_uid(key.m_assembly_uid)->textures();

    // Fetch the texture.
    const size_t texture_index = key.get_texture_index();
    assert(texture_index < textures.size());
    Texture* texture = textures.get_by_index(texture_index);

#ifdef TRACK_TILE_UNLOADING
    RENDERER_LOG_DEBUG(
        "unloading tile (" FMT_SIZE_T ", " FMT_SIZE_T ") "
        "from texture \"%s\"...",
        key.get_tile_x(),
        key.get_tile_y(),
        texture->get_name());
#endif

    // Unload the tile.
    texture->unload_tile(key.get_tile_x(), key.get_tile_y(), record.m_tile);

    // Successfully unloaded the tile.
    return true;
}

}   // namespace renderer
