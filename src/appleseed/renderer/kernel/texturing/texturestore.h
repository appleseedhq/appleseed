
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_TEXTURING_TEXTURESTORE_H
#define APPLESEED_RENDERER_KERNEL_TEXTURING_TEXTURESTORE_H

// appleseed.renderer headers.
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/hash.h"
#include "foundation/platform/atomic.h"
#include "foundation/platform/thread.h"
#include "foundation/platform/types.h"
#include "foundation/utility/cache.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <map>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class Statistics; }
namespace foundation    { class Tile; }
namespace renderer      { class Assemblies; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Scene; }

namespace renderer
{

//
// A shared store for texture tiles (the backend of the thread-local texture cache).
//

class TextureStore
  : public foundation::NonCopyable
{
  public:
    // This structure uniquely identifies a texture tile in a scene.
    struct TileKey
    {
        foundation::UniqueID    m_assembly_uid;
        foundation::UniqueID    m_texture_uid;
        foundation::uint32      m_tile_xy;

        TileKey();

        TileKey(
            const foundation::UniqueID  assembly_uid,
            const foundation::UniqueID  texture_uid,
            const size_t                tile_x,
            const size_t                tile_y);

        TileKey(
            const foundation::UniqueID  assembly_uid,
            const foundation::UniqueID  texture_uid,
            const foundation::uint32    tile_xy);

        TileKey(const TileKey& rhs);

        size_t get_tile_x() const;
        size_t get_tile_y() const;

        // Return an invalid key.
        static TileKey invalid();

        // Comparison operators.
        bool operator==(const TileKey& rhs) const;
        bool operator!=(const TileKey& rhs) const;
        bool operator<(const TileKey& rhs) const;
    };

    struct TileRecord
    {
        foundation::Tile*           m_tile;
        volatile foundation::uint32 m_owners;
    };

    // Constructor.
    TextureStore(
        const Scene&        scene,
        const ParamArray&   params = ParamArray());

    // Acquire an element from the cache. Thread-safe.
    TileRecord& acquire(const TileKey& key);

    // Release a previously-acquired element. Thread-safe.
    void release(TileRecord& record) const;

    // Retrieve performance statistics.
    foundation::StatisticsVector get_statistics() const;

    // Get the metadata dictionary describing the texture store params.
    static foundation::Dictionary get_params_metadata();

  private:
    struct TileKeyHasher
    {
        size_t operator()(const TileKey& key) const;
    };

    class TileSwapper
      : public foundation::NonCopyable
    {
      public:
        // Constructor.
        TileSwapper(
            const Scene&        scene,
            const ParamArray&   params);

        // Load a cache line.
        void load(const TileKey& key, TileRecord& record);

        // Unload a cache line.
        bool unload(const TileKey& key, TileRecord& record);

        // Return true if the cache is full, false otherwise.
        bool is_full(const size_t element_count) const;

        // Return the peak memory size in bytes of the tile cache.
        size_t get_peak_memory_size() const;

      private:
        struct Parameters
        {
            const size_t    m_memory_limit;
            const bool      m_track_tile_loading;
            const bool      m_track_tile_unloading;
            const bool      m_track_store_size;

            explicit Parameters(const ParamArray& params);
        };

        typedef std::map<foundation::UniqueID, const Assembly*> AssemblyMap;

        const Scene&        m_scene;
        const Parameters    m_params;
        size_t              m_memory_size;
        size_t              m_peak_memory_size;
        AssemblyMap         m_assemblies;

        void gather_assemblies(const AssemblyContainer& assemblies);
    };

    typedef foundation::LRUCache<
        TileKey,
        TileKeyHasher,
        TileRecord,
        TileSwapper
    > TileCache;

    boost::mutex            m_mutex;
    TileKeyHasher           m_tile_key_hasher;
    TileSwapper             m_tile_swapper;
    TileCache               m_tile_cache;
};


//
// TextureStore class implementation.
//

inline TextureStore::TileRecord& TextureStore::acquire(const TileKey& key)
{
    boost::mutex::scoped_lock lock(m_mutex);

    TileRecord& record = m_tile_cache.get(key);
    foundation::atomic_inc(&record.m_owners);

    return record;
}

inline void TextureStore::release(TileRecord& record) const
{
    assert(foundation::atomic_read(&record.m_owners) > 0);
    foundation::atomic_dec(&record.m_owners);
}


//
// TextureStore::TileKey class implementation.
//

inline TextureStore::TileKey::TileKey()
{
}

inline TextureStore::TileKey::TileKey(
    const foundation::UniqueID  assembly_uid,
    const foundation::UniqueID  texture_uid,
    const size_t                tile_x,
    const size_t                tile_y)
  : m_assembly_uid(assembly_uid)
  , m_texture_uid(texture_uid)
  , m_tile_xy(static_cast<foundation::uint32>((tile_y << 16) | tile_x))
{
    assert(tile_x < (1UL << 16));
    assert(tile_y < (1UL << 16));
}

inline TextureStore::TileKey::TileKey(
    const foundation::UniqueID  assembly_uid,
    const foundation::UniqueID  texture_uid,
    const foundation::uint32    tile_xy)
  : m_assembly_uid(assembly_uid)
  , m_texture_uid(texture_uid)
  , m_tile_xy(tile_xy)
{
}

inline TextureStore::TileKey::TileKey(const TileKey& rhs)
  : m_assembly_uid(rhs.m_assembly_uid)
  , m_texture_uid(rhs.m_texture_uid)
  , m_tile_xy(rhs.m_tile_xy)
{
}

inline size_t TextureStore::TileKey::get_tile_x() const
{
    return static_cast<size_t>(m_tile_xy & 0x0000FFFFUL);
}

inline size_t TextureStore::TileKey::get_tile_y() const
{
    return static_cast<size_t>(m_tile_xy >> 16);
}

inline TextureStore::TileKey TextureStore::TileKey::invalid()
{
    return TileKey(~0, ~0, ~0);
}

inline bool TextureStore::TileKey::operator==(const TileKey& rhs) const
{
    return
        m_tile_xy == rhs.m_tile_xy &&
        m_texture_uid == rhs.m_texture_uid &&
        m_assembly_uid == rhs.m_assembly_uid;
}

inline bool TextureStore::TileKey::operator!=(const TileKey& rhs) const
{
    return !operator==(rhs);
}

inline bool TextureStore::TileKey::operator<(const TileKey& rhs) const
{
    return
        m_assembly_uid == rhs.m_assembly_uid ?
            m_texture_uid == rhs.m_texture_uid ?
                m_tile_xy < rhs.m_tile_xy :
            m_texture_uid < rhs.m_texture_uid :
        m_assembly_uid < rhs.m_assembly_uid;
}


//
// TextureStore::TileKeyHasher class implementation.
//

inline size_t TextureStore::TileKeyHasher::operator()(const TileKey& key) const
{
    return foundation::mix_uint64(key.m_assembly_uid, key.m_texture_uid, key.m_tile_xy);
}


//
// TextureStore::TileSwapper class implementation.
//

inline bool TextureStore::TileSwapper::is_full(const size_t element_count) const
{
    return m_memory_size >= m_params.m_memory_limit;
}

inline size_t TextureStore::TileSwapper::get_peak_memory_size() const
{
    return m_peak_memory_size;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_TEXTURING_TEXTURESTORE_H
