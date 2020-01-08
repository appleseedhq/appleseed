
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

#pragma once

// appleseed.renderer headers.
#include "renderer/kernel/texturing/texturestore.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/cache.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace foundation    { class Tile; }

namespace renderer
{

//
// A thread-local cache of texture tiles.
//

class TextureCache
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit TextureCache(TextureStore& store);

    // Get a tile from the cache.
    foundation::Tile& get(
        const foundation::UniqueID  assembly_uid,
        const foundation::UniqueID  texture_uid,
        const size_t                tile_x,
        const size_t                tile_y);

    // Retrieve performance statistics.
    foundation::StatisticsVector get_statistics() const;
    std::uint64_t get_hit_count() const;
    std::uint64_t get_miss_count() const;

  private:
    typedef TextureStore::TileKey TileKey;
    typedef TextureStore::TileKeyHasher TileKeyHasher;
    typedef TextureStore::TileRecord TileRecord;
    typedef TileRecord* TileRecordPtr;

    class TileRecordSwapper
      : public foundation::NonCopyable
    {
      public:
        // Constructor.
        explicit TileRecordSwapper(TextureStore& store);

        // Load a cache line.
        void load(const TileKey& key, TileRecordPtr& record);

        // Unload a cache line.
        void unload(const TileKey& key, TileRecordPtr& record);

      private:
        TextureStore& m_store;
    };

    typedef foundation::SACache<
        TileKey,
        TileKeyHasher,
        TileRecordPtr,
        TileRecordSwapper,
        512,                // number of cache lines
        4                   // number of ways
    > TileCache;

    TileKeyHasher           m_tile_key_hasher;
    TileRecordSwapper       m_tile_record_swapper;
    TileCache               m_tile_cache;
};


//
//  TextureCache class implementation.
//

inline TextureCache::TextureCache(TextureStore& store)
  : m_tile_record_swapper(store)
  , m_tile_cache(m_tile_key_hasher, m_tile_record_swapper, TileKey::invalid())
{
}

inline foundation::Tile& TextureCache::get(
    const foundation::UniqueID      assembly_uid,
    const foundation::UniqueID      texture_uid,
    const size_t                    tile_x,
    const size_t                    tile_y)
{
    const TileKey key(assembly_uid, texture_uid, tile_x, tile_y);
    return *m_tile_cache.get(key)->m_tile_ptr.get_tile();
}

inline foundation::StatisticsVector TextureCache::get_statistics() const
{
    return
        foundation::StatisticsVector::make(
            "texture cache statistics",
            foundation::make_single_stage_cache_stats(m_tile_cache));
}

inline std::uint64_t TextureCache::get_hit_count() const
{
    return m_tile_cache.get_hit_count();
}

inline std::uint64_t TextureCache::get_miss_count() const
{
    return m_tile_cache.get_miss_count();
}


//
// TextureCache::TileRecordSwapper class implementation.
//

inline TextureCache::TileRecordSwapper::TileRecordSwapper(TextureStore& store)
  : m_store(store)
{
}

inline void TextureCache::TileRecordSwapper::load(const TileKey& key, TileRecordPtr& record)
{
    record = &m_store.acquire(key);
}

inline void TextureCache::TileRecordSwapper::unload(const TileKey& key, TileRecordPtr& record)
{
    m_store.release(*record);
}

}   // namespace renderer
