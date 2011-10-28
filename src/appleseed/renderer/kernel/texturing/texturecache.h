
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

#ifndef APPLESEED_RENDERER_KERNEL_TEXTURING_TEXTURECACHE_H
#define APPLESEED_RENDERER_KERNEL_TEXTURING_TEXTURECACHE_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/image/tile.h"
#include "foundation/math/hash.h"
#include "foundation/utility/cache.h"

// Forward declarations.
namespace renderer      { class Scene; }

namespace renderer
{

//
// A thread-local cache of texture tiles.
//

class TextureCache
{
  public:
    // Constructor.
    TextureCache(
        const Scene&                scene,
        const size_t                memory_limit);

    // Destructor.
    ~TextureCache();

    // Get a tile from the cache.
    foundation::Tile& get(
        const foundation::UniqueID  assembly_uid,
        const size_t                texture_index,
        const size_t                tile_x,
        const size_t                tile_y);

    // Return the number of cache hits/misses in stage-0.
    foundation::uint64 get_stage0_hit_count() const;
    foundation::uint64 get_stage0_miss_count() const;

    // Return the number of cache hits/misses in stage-1.
    foundation::uint64 get_stage1_hit_count() const;
    foundation::uint64 get_stage1_miss_count() const;

  private:
    struct TileKey
    {
        foundation::UniqueID        m_assembly_uid;
        size_t                      m_texture_index;
        size_t                      m_tile_x;
        size_t                      m_tile_y;

        // Return an invalid key.
        static TileKey invalid();

        // Comparison operators.
        bool operator==(const TileKey& rhs) const;
        bool operator<(const TileKey& rhs) const;
    };

    struct TileKeyHasher
      : public foundation::NonCopyable
    {
        // Hash a key into an integer.
        size_t operator()(const TileKey& key) const;
    };

    typedef foundation::Tile* TilePtr;

    class TileSwapper
      : public foundation::NonCopyable
    {
      public:
        // Constructor.
        TileSwapper(
            const Scene&            scene,
            const size_t            memory_limit);

        // Load a cache line.
        void load(const TileKey& key, TilePtr& tile);

        // Unload a cache line.
        void unload(const TileKey& key, TilePtr& tile);

        // Return true if the cache is full, false otherwise.
        bool is_full(const size_t element_count) const;
            
      private:
        const Scene&                m_scene;
        const size_t                m_memory_limit;
        size_t                      m_memory_size;
    };

    typedef foundation::DualStageCache<
        TileKey,
        TileKeyHasher,
        TilePtr,
        TileSwapper,
        512,            // number of cache lines
        2               // number of ways
    > TileCache;

    TileKeyHasher       m_tile_key_hasher;
    TileSwapper         m_tile_swapper;
    TileCache           m_tile_cache;
};


//
//  TextureCache class implementation.
//

inline foundation::Tile& TextureCache::get(
    const foundation::UniqueID      assembly_uid,
    const size_t                    texture_index,
    const size_t                    tile_x,
    const size_t                    tile_y)
{
    // Construct the tile key.
    TileKey key;
    key.m_assembly_uid = assembly_uid;
    key.m_texture_index = texture_index;
    key.m_tile_x = tile_x;
    key.m_tile_y = tile_y;

    // Lookup the tile cache.
    TilePtr tile = m_tile_cache.get(key);
    assert(tile);

    return *tile;
}

inline foundation::uint64 TextureCache::get_stage0_hit_count() const
{
    return m_tile_cache.get_stage0_hit_count();
}

inline foundation::uint64 TextureCache::get_stage0_miss_count() const
{
    return m_tile_cache.get_stage0_miss_count();
}

inline foundation::uint64 TextureCache::get_stage1_hit_count() const
{
    return m_tile_cache.get_stage1_hit_count();
}

inline foundation::uint64 TextureCache::get_stage1_miss_count() const
{
    return m_tile_cache.get_stage1_miss_count();
}


//
// TextureCache::TileKey class implementation.
//

inline TextureCache::TileKey TextureCache::TileKey::invalid()
{
    TileKey key;
    key.m_assembly_uid = ~foundation::UniqueID(0);
    key.m_texture_index = ~size_t(0);
    key.m_tile_x = ~size_t(0);
    key.m_tile_y = ~size_t(0);
    return key;
}

inline bool TextureCache::TileKey::operator==(const TileKey& rhs) const
{
    return
        m_assembly_uid == rhs.m_assembly_uid &&
        m_texture_index == rhs.m_texture_index &&
        m_tile_x == rhs.m_tile_x &&
        m_tile_y == rhs.m_tile_y;
}

inline bool TextureCache::TileKey::operator<(const TileKey& rhs) const
{
    return
        m_assembly_uid < rhs.m_assembly_uid ? true :
        m_assembly_uid > rhs.m_assembly_uid ? false :
        m_texture_index < rhs.m_texture_index ? true :
        m_texture_index > rhs.m_texture_index ? false :
        m_tile_y < rhs.m_tile_y ? true :
        m_tile_y > rhs.m_tile_y ? false :
        m_tile_x < rhs.m_tile_x;
}


//
// TextureCache::TileKeyHasher class implementation.
//

inline size_t TextureCache::TileKeyHasher::operator()(const TileKey& key) const
{
    return foundation::mix32(
        static_cast<foundation::uint32>(key.m_assembly_uid),
        static_cast<foundation::uint32>(key.m_texture_index),
        static_cast<foundation::uint32>(key.m_tile_x),
        static_cast<foundation::uint32>(key.m_tile_y));
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_TEXTURING_TEXTURECACHE_H
