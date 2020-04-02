
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
#include "texturestore.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/tile.h"
#include "foundation/memory/memory.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <algorithm>
#include <string>

using namespace foundation;

namespace renderer
{

//
// TextureStore class implementation.
//

Dictionary TextureStore::get_params_metadata()
{
    Dictionary metadata;
    metadata.dictionaries().insert(
        "max_size",
        Dictionary()
            .insert("type", "int")
            .insert("default", get_default_size())
            .insert("label", "Texture Cache Size")
            .insert("help", "Texture cache size in bytes"));

    return metadata;
}

size_t TextureStore::get_default_size()
{
    return 1024 * 1024 * 1024;
}

TextureStore::TextureStore(
    const Scene&        scene,
    const ParamArray&   params)
  : m_tile_swapper(scene, params)
  , m_tile_cache(m_tile_key_hasher, m_tile_swapper)
{
}

StatisticsVector TextureStore::get_statistics() const
{
    Statistics stats = make_single_stage_cache_stats(m_tile_cache);
    stats.insert_size("peak size", m_tile_swapper.get_peak_memory_size());
    return StatisticsVector::make("texture store statistics", stats);
}


//
// TextureStore::TileSwapper class implementation.
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
                tile.set_pixel(i, fast_srgb_to_linear_rgb(color));
            }
        }
        else
        {
            for (size_t i = 0; i < pixel_count; ++i)
            {
                Color4f color;
                tile.get_pixel(i, color);
                color.rgb() = fast_srgb_to_linear_rgb(color.rgb());
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

TextureStore::TileSwapper::TileSwapper(
    const Scene&        scene,
    const ParamArray&   params)
  : m_scene(scene)
  , m_params(params)
  , m_memory_size(0)
  , m_peak_memory_size(0)
{
    gather_assemblies(scene.assemblies());
    print_settings();
}

void TextureStore::TileSwapper::print_settings() const
{
    RENDERER_LOG_INFO(
        "texture store settings:\n"
        "  max store size                %s\n"
        "  track store size              %s\n"
        "  track tile loading            %s\n"
        "  track tile unloading          %s",
        pretty_size(m_params.m_memory_limit).c_str(),
        m_params.m_track_store_size ? "on" : "off",
        m_params.m_track_tile_loading ? "on" : "off",
        m_params.m_track_tile_unloading ? "on" : "off");
}

void TextureStore::TileSwapper::load(const TileKey& key, TileRecord& record)
{
    // Fetch the texture container.
    const TextureContainer& textures =
        key.m_assembly_uid == ~UniqueID(0)
            ? m_scene.textures()
            : m_assemblies[key.m_assembly_uid]->textures();

    // Fetch the texture.
    Texture* texture = textures.get_by_uid(key.m_texture_uid);
    assert(texture != nullptr);

    if (m_params.m_track_tile_loading)
    {
        RENDERER_LOG_DEBUG(
            "loading tile (" FMT_SIZE_T ", " FMT_SIZE_T ") "
            "from texture \"%s\"...",
            key.get_tile_x(),
            key.get_tile_y(),
            texture->get_path().c_str());
    }

    // Load the tile.
    record.m_tile_ptr = texture->load_tile(key.get_tile_x(), key.get_tile_y());
    record.m_owners = 0;

    // Convert the tile to the linear RGB color space.
    switch (texture->get_color_space())
    {
      case ColorSpaceLinearRGB:
        break;

      case ColorSpaceSRGB:
        convert_tile_srgb_to_linear_rgb(*record.m_tile_ptr.get_tile());
        break;

      case ColorSpaceCIEXYZ:
        convert_tile_ciexyz_to_linear_rgb(*record.m_tile_ptr.get_tile());
        break;

      assert_otherwise;
    }

    // Track the amount of memory used by the tile cache.
    m_memory_size += record.m_tile_ptr.get_tile()->get_memory_size();
    m_peak_memory_size = std::max(m_peak_memory_size, m_memory_size);

    if (m_params.m_track_store_size)
    {
        if (m_memory_size > m_params.m_memory_limit)
        {
            RENDERER_LOG_DEBUG(
                "texture store size is %s, exceeding capacity %s by %s.",
                pretty_size(m_memory_size).c_str(),
                pretty_size(m_params.m_memory_limit).c_str(),
                pretty_size(m_memory_size - m_params.m_memory_limit).c_str());
        }
        else
        {
            RENDERER_LOG_DEBUG(
                "texture store size is %s, below capacity %s by %s.",
                pretty_size(m_memory_size).c_str(),
                pretty_size(m_params.m_memory_limit).c_str(),
                pretty_size(m_params.m_memory_limit - m_memory_size).c_str());
        }
    }
}

bool TextureStore::TileSwapper::unload(const TileKey& key, TileRecord& record)
{
    // Cannot unload tiles that are still in use.
    if (atomic_read(&record.m_owners) > 0)
        return false;

    // Track the amount of memory used by the tile cache.
    const size_t tile_memory_size = record.m_tile_ptr.get_tile()->get_memory_size();
    assert(m_memory_size >= tile_memory_size);
    m_memory_size -= tile_memory_size;

    if (m_params.m_track_tile_unloading)
    {
        // Fetch the texture container.
        const TextureContainer& textures =
            key.m_assembly_uid == ~UniqueID(0)
            ? m_scene.textures()
            : m_assemblies[key.m_assembly_uid]->textures();

        // Fetch the texture.
        Texture* texture = textures.get_by_uid(key.m_texture_uid);

        if (texture != nullptr)
        {
            RENDERER_LOG_DEBUG(
                "unloading tile (" FMT_SIZE_T ", " FMT_SIZE_T ") "
                "from texture \"%s\"...",
                key.get_tile_x(),
                key.get_tile_y(),
                texture->get_path().c_str());
        }
        else
        {
            RENDERER_LOG_DEBUG(
                "unloading tile (" FMT_SIZE_T ", " FMT_SIZE_T ") "
                "from defunct texture...",
                key.get_tile_x(),
                key.get_tile_y());
        }
    }

    // Unload the tile.
    if (record.m_tile_ptr.has_ownership())
        delete record.m_tile_ptr.get_tile();

    // Successfully unloaded the tile.
    return true;
}

void TextureStore::TileSwapper::gather_assemblies(const AssemblyContainer& assemblies)
{
    for (const Assembly& assembly : assemblies)
    {
        m_assemblies[assembly.get_uid()] = &assembly;
        gather_assemblies(assembly.assemblies());
    }
}


//
// TextureStore::TileSwapper::Parameters class implementation.
//

TextureStore::TileSwapper::Parameters::Parameters(const ParamArray& params)
  : m_memory_limit(params.get_optional<size_t>("max_size", TextureStore::get_default_size()))
  , m_track_tile_loading(params.get_optional<bool>("track_tile_loading", false))
  , m_track_tile_unloading(params.get_optional<bool>("track_tile_unloading", false))
  , m_track_store_size(params.get_optional<bool>("track_store_size", false))
{
    assert(m_memory_limit > 0);
}

}   // namespace renderer
