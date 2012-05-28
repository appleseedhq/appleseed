
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
#include "foundation/utility/cache.h"
#include "foundation/utility/uid.h"

// boost headers.
#include "boost/interprocess/detail/atomic.hpp"

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
        foundation::UniqueID        m_assembly_uid;
        size_t                      m_texture_index;
        size_t                      m_tile_x;
        size_t                      m_tile_y;

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

inline TextureStore::TileKey TextureStore::TileKey::invalid()
{
    TileKey key;
    key.m_assembly_uid = ~0;
    key.m_texture_index = ~0;
    key.m_tile_x = ~0;
    key.m_tile_y = ~0;
    return key;
}

inline bool TextureStore::TileKey::operator==(const TileKey& rhs) const
{
    return
        m_assembly_uid == rhs.m_assembly_uid &&
        m_texture_index == rhs.m_texture_index &&
        m_tile_x == rhs.m_tile_x &&
        m_tile_y == rhs.m_tile_y;
}

inline bool TextureStore::TileKey::operator!=(const TileKey& rhs) const
{
    return !operator==(rhs);
}

inline bool TextureStore::TileKey::operator<(const TileKey& rhs) const
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
// TextureStore class implementation.
//

inline TextureStore::TileRecord& TextureStore::acquire(const TileKey& key)
{
    boost::mutex::scoped_lock lock(m_mutex);

    TileRecord& record = m_tile_cache.get(key);

    boost::interprocess::detail::atomic_inc32(&record.m_owners);

    return record;
}

inline void TextureStore::release(TileRecord& record) const
{
    assert(boost::interprocess::detail::atomic_read32(&record.m_owners) > 0);

    boost::interprocess::detail::atomic_dec32(&record.m_owners);
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
