
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

#ifndef APPLESEED_RENDERER_KERNEL_TEXTURING_TEXTURESTORE_H
#define APPLESEED_RENDERER_KERNEL_TEXTURING_TEXTURESTORE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/thread.h"
#include "foundation/platform/types.h"
#include "foundation/utility/cache.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <cstddef>

// Forward declarations.
namespace foundation    { class Tile; }
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
    struct TileKey
    {
        foundation::UniqueID            m_assembly_uid;
        foundation::UniqueID            m_texture_uid;
        foundation::uint32              m_tile_xy;

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
        volatile boost::uint32_t    m_owners;
    };

    // Constructor.
    TextureStore(
        const Scene&        scene,
        const size_t        memory_limit = 16 * 1024 * 1024);

    // Destructor.
    ~TextureStore();

    // Acquire an element from the cache. Thread-safe.
    TileRecord& acquire(const TileKey& key);

    // Release a previously-acquired element. Thread-safe.
    void release(TileRecord& record) const;

  private:
    struct TileSwapper
      : public foundation::NonCopyable
    {
        const Scene&        m_scene;
        const size_t        m_memory_limit;
        size_t              m_memory_size;
        size_t              m_max_memory_size;

        // Constructor.
        TileSwapper(
            const Scene&    scene,
            const size_t    memory_limit);

        // Load a cache line.
        void load(const TileKey& key, TileRecord& record);

        // Unload a cache line.
        bool unload(const TileKey& key, TileRecord& record);

        // Return true if the cache is full, false otherwise.
        bool is_full(const size_t element_count) const;
    };

    typedef foundation::LRUCache<
        TileKey,
        TileRecord,
        TileSwapper
    > TileCache;

    boost::mutex    m_mutex;
    TileSwapper     m_tile_swapper;
    TileCache       m_tile_cache;
};


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
// TextureStore class implementation.
//

inline TextureStore::TileRecord& TextureStore::acquire(const TileKey& key)
{
    boost::mutex::scoped_lock lock(m_mutex);

    TileRecord& record = m_tile_cache.get(key);

    boost_atomic::atomic_inc32(&record.m_owners);

    return record;
}

inline void TextureStore::release(TileRecord& record) const
{
    assert(boost_atomic::atomic_read32(&record.m_owners) > 0);

    boost_atomic::atomic_dec32(&record.m_owners);
}


//
// TextureStore::TileSwapper class implementation.
//

inline bool TextureStore::TileSwapper::is_full(const size_t element_count) const
{
    return m_memory_size >= m_memory_limit;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_TEXTURING_TEXTURESTORE_H
