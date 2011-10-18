
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

#ifndef APPLESEED_FOUNDATION_UTILITY_CACHE_H
#define APPLESEED_FOUNDATION_UTILITY_CACHE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>
#include <list>
#include <map>
#include <string>
#include <vector>

namespace foundation
{

namespace cache_impl
{

    //
    // Base class for all caches.
    //

    class CacheBase
      : public NonCopyable
    {
      public:
        // Constructor.
        CacheBase()
        {
            clear_statistics();
        }

        // Reset the cache performance statistics.
        void clear_statistics()
        {
            m_hit_count = 0;
            m_miss_count = 0;
        }

        // Return the number of cache hits.
        uint64 get_hit_count() const
        {
            return m_hit_count;
        }

        // Return the number of cache misses.
        uint64 get_miss_count() const
        {
            return m_miss_count;
        }

      protected:
        uint64  m_hit_count;
        uint64  m_miss_count;
    };


    //
    // Timestamp type.
    //

    typedef size_t Timestamp;


    //
    // Timestamp source.
    //

    class Timestamper
      : public NonCopyable
    {
      public:
        // Constructor.
        Timestamper()
          : m_timestamp(0)
        {
        }

        // Return the next timestamp value.
        Timestamp next()
        {
            return m_timestamp++;
        }

      private:
        Timestamp m_timestamp;
    };


    //
    // Single cache entry for set associative caches.
    //

    template <typename Key, typename Element, bool WithTimestamp>
    struct SACacheEntry;

    template <typename Key, typename Element>
    struct SACacheEntry<Key, Element, false>
      : public NonCopyable
    {
        Key         m_key;
        Element     m_element;
    };

    template <typename Key, typename Element>
    struct SACacheEntry<Key, Element, true>
      : public NonCopyable
    {
        Key         m_key;
        Element     m_element;
        Timestamp   m_timestamp;
    };


    //
    // Single cache line for a generic N-way set associative cache.
    //

    template <typename Key, typename Element, size_t Ways>
    class SACacheLine
      : public NonCopyable
    {
      public:
        // Types.
        typedef Key KeyType;
        typedef Element ElementType;
        typedef SACacheEntry<Key, Element, true> EntryType;

        // Invalidate all entries of the cache line.
        void invalidate(const KeyType& invalid_key)
        {
            for (size_t i = 0; i < Ways; ++i)
            {
                m_entries[i].m_key = invalid_key;
                m_entries[i].m_timestamp = 0;
            }
        }

        // Return a pointer to the entry corresponding to a given key, or 0 if this key was not found.
        EntryType* find_entry(const KeyType& key)
        {
            for (size_t i = 0; i < Ways; ++i)
            {
                if (m_entries[i].m_key == key)
                    return &m_entries[i];
            }

            return 0;
        }

        // Update the timestamp of a given entry in this cache line.
        void touch_entry(EntryType* entry, Timestamper& timestamper)
        {
            entry->m_timestamp = timestamper.next();
        }

        // Find an entry to replace in this cache line.
        EntryType* find_eviction_candidate()
        {
            Timestamp timestamp = std::numeric_limits<Timestamp>::max();
            size_t oldest = 0;

            for (size_t i = 0; i < Ways; ++i)
            {
                if (timestamp > m_entries[i].m_timestamp)
                {
                    timestamp = m_entries[i].m_timestamp;
                    oldest = i;
                }
            }
            
            return &m_entries[oldest];
        }

        // Check the integrity of the cache line. For debug purposes only.
        template <typename IntegrityChecker>
        void check_integrity(IntegrityChecker& checker) const
        {
            for (size_t i = 0; i < Ways; ++i)
                checker(m_entries[i].m_key, m_entries[i].m_element);
        }

      private:
        EntryType m_entries[Ways];
    };


    //
    // Specialization of SACacheLine for 1-way (direct-mapped) caches.
    //

    template <typename Key, typename Element>
    class SACacheLine<Key, Element, 1>
      : public NonCopyable
    {
      public:
        // Types.
        typedef Key KeyType;
        typedef Element ElementType;
        typedef SACacheEntry<Key, Element, false> EntryType;

        // Invalidate all entries of the cache line.
        void invalidate(const KeyType& invalid_key)
        {
            m_entry.m_key = invalid_key;
        }

        // Return a pointer to the entry corresponding to a given key, or 0 if this key was not found.
        EntryType* find_entry(const KeyType& key)
        {
            return m_entry.m_key == key ? &m_entry : 0;
        }

        // Update the timestamp of a given entry in this cache line.
        void touch_entry(EntryType* entry, Timestamper& timestamper)
        {
        }

        // Find an entry to replace in this cache line.
        EntryType* find_eviction_candidate()
        {
            return &m_entry;
        }

        // Check the integrity of the cache line. For debug purposes only.
        template <typename IntegrityChecker>
        void check_integrity(IntegrityChecker& checker) const
        {
            checker(m_entry.m_key, m_entry.m_element);
        }

      private:
        EntryType m_entry;
    };


    //
    // Specialization of SACacheLine for 2-way set associative caches.
    //

    template <typename Key, typename Element>
    class SACacheLine<Key, Element, 2>
      : public NonCopyable
    {
      public:
        // Types.
        typedef Key KeyType;
        typedef Element ElementType;
        typedef SACacheEntry<Key, Element, false> EntryType;

        // Invalidate all entries of the cache line.
        void invalidate(const KeyType& invalid_key)
        {
            m_entries[0].m_key = invalid_key;
            m_entries[1].m_key = invalid_key;
            m_oldest = 0;
        }

        // Return a pointer to the entry corresponding to a given key, or 0 if this key was not found.
        EntryType* find_entry(const KeyType& key)
        {
            return
                m_entries[0].m_key == key ? &m_entries[0] :
                m_entries[1].m_key == key ? &m_entries[1] : 0;
        }

        // Update the timestamp of a given entry in this cache line.
        void touch_entry(EntryType* entry, Timestamper& timestamper)
        {
            m_oldest = 1 - (entry - m_entries);
        }

        // Find an entry to replace in this cache line.
        EntryType* find_eviction_candidate()
        {
            return &m_entries[m_oldest];
        }

        // Check the integrity of the cache line. For debug purposes only.
        template <typename IntegrityChecker>
        void check_integrity(IntegrityChecker& checker) const
        {
            checker(m_entries[0].m_key, m_entries[0].m_element);
            checker(m_entries[1].m_key, m_entries[1].m_element);
        }

      private:
        EntryType   m_entries[2];
        size_t      m_oldest;
    };

}   // namespace cache_impl


//
// Set associative cache.
//
// If the template parameter Ways equals 1, this is a direct-mapped cache.
//
// The KeyHasher class must conform to the following prototype:
//
//      class KeyHasher
//        : public foundation::NonCopyable
//      {
//        public:
//          // Hash a key into an integer.
//          size_t operator()(const Key key);
//      };
//
// The ElementSwapper class must conform to the following prototype:
//
//      class ElementSwapper
//        : public foundation::NonCopyable
//      {
//        public:
//          // Load a cache line.
//          void load(const Key key, Element& element);
//
//          // Unload a cache line.
//          void unload(const Key key, Element& element);
//      };
//

template <
    typename    Key,
    typename    KeyHasher,
    typename    Element,
    typename    ElementSwapper,
    size_t      Lines_,
    size_t      Ways_ = 1
>
class SACache
  : public cache_impl::CacheBase
{
  public:
    // Types.
    typedef Key             KeyType;
    typedef KeyHasher       KeyHasherType;
    typedef Element         ElementType;
    typedef ElementSwapper  ElementSwapperType;

    // Line and way count.
    static const size_t Lines = Lines_;
    static const size_t Ways = Ways_;

    // Constructor.
    SACache(
        KeyHasherType&      key_hasher,
        ElementSwapperType& element_swapper,
        const KeyType&      invalid_key);

    // Clear the cache.
    void clear();

    // Get an element from the cache.
    ElementType& get(const KeyType& key);

    // Invalidate a cache entry.
    void invalidate(const KeyType& key);

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

    // Check the integrity of the cache. For debug purposes only.
    template <typename IntegrityChecker>
    void check_integrity(IntegrityChecker& checker) const;

  private:
    // Cache line type.
    typedef cache_impl::SACacheLine<
        KeyType,
        ElementType,
        Ways
    > LineType;

    // Entry type.
    typedef typename LineType::EntryType EntryType;

    KeyHasherType&          m_key_hasher;
    ElementSwapperType&     m_element_swapper;
    const KeyType           m_invalid_key;
    cache_impl::Timestamper m_timestamper;
    LineType                m_lines[Lines];             // cache storage
};


//
// LRU cache.
//
// The Element type must implement the foundation::dynamic_sizeof<>()
// operator.
//
// The ElementSwapper class must conform to the following prototype:
//
//      class ElementSwapper
//        : public foundation::NonCopyable
//      {
//        public:
//          // Load a cache line.
//          void load(const Key key, Element& element);
//
//          // Unload a cache line.
//          void unload(const Key key, Element& element);
//
//          // This method must return true if the cache is considered
//          // to be full, given the number of elements already stored
//          // in the cache and the total amount of memory they occupy.
//          // If true is returned, the least recently used element will
//          // be replaced next time an element needs to be loaded into
//          // the cache. If false is returned, the cache will grow in
//          // order to accommodate the new element.
//          bool is_full(
//              const size_t    element_count,
//              const size_t    memory_size) const;     // in bytes
//      };
//

template <
    typename    Key,
    typename    Element,
    typename    ElementSwapper
>
class LRUCache
  : public cache_impl::CacheBase
{
  public:
    // Types.
    typedef Key             KeyType;
    typedef Element         ElementType;
    typedef ElementSwapper  ElementSwapperType;

    // Constructor.
    explicit LRUCache(
        ElementSwapperType& element_swapper);

    // Clear the cache.
    void clear();

    // Get an element from the cache.
    ElementType& get(const KeyType& key);

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

    // Check the integrity of the cache. For debug purposes only.
    template <typename IntegrityChecker>
    void check_integrity(IntegrityChecker& checker) const;

  private:
    // Cache line.
    struct Line
    {
        KeyType             m_key;
        ElementType         m_element;
    };

    // Cache queue.
    typedef std::list<size_t> Queue;
    typedef typename Queue::iterator QueueIterator;

    // Cache index.
    struct IndexEntry
    {
        size_t              m_line_index;
        QueueIterator       m_queue_it;
    };
    typedef std::map<KeyType, IndexEntry> Index;

    std::vector<Line>       m_lines;                    // cache storage
    Index                   m_index;                    // cache index
    Queue                   m_queue;                    // LRU queue
    ElementSwapperType&     m_element_swapper;          // element swapper
    size_t                  m_memory_size;              // total size of the elements in the cache, in bytes
};


//
// Dual stage cache: the front end is a set associative cache, and the
// back-end is a LRU cache.
//
// The Element type must implement the foundation::dynamic_sizeof<>()
// operator.
//
// The KeyHasher class must conform to the following prototype:
//
//      class KeyHasher
//        : public foundation::NonCopyable
//      {
//        public:
//          // Hash a key into an integer.
//          size_t operator()(const Key key);
//      };
//
// The ElementSwapper class must conform to the following prototype:
//
//      class ElementSwapper
//        : public foundation::NonCopyable
//      {
//        public:
//          // Load a cache line.
//          void load(const Key key, Element& element);
//
//          // Unload a cache line.
//          void unload(const Key key, Element& element);
//
//          // This method must return true if the cache is considered
//          // to be full, given the number of elements already stored
//          // in the cache and the total amount of memory they occupy.
//          // If true is returned, the least recently used element will
//          // be replaced next time an element needs to be loaded into
//          // the cache. If false is returned, the cache will grow in
//          // order to accommodate the new element.
//          bool is_full(
//              const size_t    element_count,
//              const size_t    memory_size) const;     // in bytes
//      };
//

template <
    typename    Key,
    typename    KeyHasher,
    typename    Element,
    typename    ElementSwapper,
    size_t      Lines_,             // number of lines of the set associative cache
    size_t      Ways_ = 1           // number of ways of the set associate cache
>
class DualStageCache
  : public NonCopyable
{
  public:
    // Types.
    typedef Key             KeyType;
    typedef KeyHasher       KeyHasherType;
    typedef Element         ElementType;
    typedef ElementSwapper  ElementSwapperType;

    // Line and way count.
    static const size_t Lines = Lines_;
    static const size_t Ways = Ways_;

    // Constructor.
    DualStageCache(
        KeyHasherType&      key_hasher,
        ElementSwapperType& element_swapper,
        const KeyType&      invalid_key);

    // Clear the cache.
    void clear();

    // Get an element from the cache.
    ElementType& get(const KeyType& key);

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

    // Reset the cache performance statistics.
    void clear_statistics();

    // Return the number of cache hits/misses in stage-0.
    uint64 get_stage0_hit_count() const;
    uint64 get_stage0_miss_count() const;

    // Return the number of cache hits/misses in stage-1.
    uint64 get_stage1_hit_count() const;
    uint64 get_stage1_miss_count() const;

    // Check the integrity of the cache. For debug purposes only.
    template <typename IntegrityChecker>
    void check_integrity(IntegrityChecker& checker) const;

  private:
    // Stage-0 cache type.
    class S0ElementSwapper;
    typedef SACache<
        KeyType,
        KeyHasherType,
        ElementType,
        S0ElementSwapper,
        Lines,
        Ways> S0Cache;

    // Stage-1 cache type.
    class S1ElementSwapper;
    typedef LRUCache<
        KeyType,
        ElementType,
        S1ElementSwapper> S1Cache;

    // Stage-0 cache element swapper.
    class S0ElementSwapper
    {
      public:
        explicit S0ElementSwapper(S1Cache& s1_cache)
          : m_s1_cache(s1_cache)
        {
        }

        void load(const KeyType& key, ElementType& element)
        {
            element = m_s1_cache.get(key);
        }

        void unload(const KeyType& key, ElementType& element)
        {
        }

      private:
        S1Cache& m_s1_cache;
    };

    // Stage-1 cache element swapper, wraps the user swapper.
    class S1ElementSwapper
    {
      public:
        S1ElementSwapper(S0Cache& s0_cache, ElementSwapperType& swapper)
          : m_s0_cache(s0_cache)
          , m_swapper(swapper)
        {
        }

        void load(const KeyType& key, ElementType& element)
        {
            m_swapper.load(key, element);
        }

        void unload(const KeyType& key, ElementType& element)
        {
            m_s0_cache.invalidate(key);

            m_swapper.unload(key, element);
        }

        bool is_full(
          const size_t    element_count,
          const size_t    memory_size) const
        {
            return m_swapper.is_full(element_count, memory_size);
        }

      private:
        S0Cache&            m_s0_cache;
        ElementSwapperType& m_swapper;
    };

    S1ElementSwapper    m_s1_element_swapper;
    S1Cache             m_s1_cache;

    S0ElementSwapper    m_s0_element_swapper;
    S0Cache             m_s0_cache;
};


//
// Utility functions to query and format cache statistics.
//

// Format cache performance statistics into a string.
std::string format_cache_stats(const uint64 hits, const uint64 misses);

// Format performance statistics of a given cache into a string.
template <typename Cache>
std::string format_cache_stats(const Cache& cache);


//
// SACache class implementation.
//

#define FOUNDATION_SACACHE_TEMPLATE_DEF(MiddleDecl)     \
    template <                                          \
        typename    Key,                                \
        typename    KeyHasher,                          \
        typename    Element,                            \
        typename    ElementSwapper,                     \
        size_t      Lines_,                             \
        size_t      Ways_                               \
    >                                                   \
    MiddleDecl                                          \
    SACache<                                            \
        Key,                                            \
        KeyHasher,                                      \
        Element,                                        \
        ElementSwapper,                                 \
        Lines_,                                         \
        Ways_                                           \
    >::

FOUNDATION_SACACHE_TEMPLATE_DEF(FOUNDATION_EMPTY)
SACache(
    KeyHasherType&      key_hasher,
    ElementSwapperType& element_swapper,
    const KeyType&      invalid_key)
  : m_key_hasher(key_hasher)
  , m_element_swapper(element_swapper)
  , m_invalid_key(invalid_key)
{
    clear();
}

FOUNDATION_SACACHE_TEMPLATE_DEF(void)
clear()
{
    for (size_t i = 0; i < Lines; ++i)
        m_lines[i].invalidate(m_invalid_key);
}

FOUNDATION_SACACHE_TEMPLATE_DEF(inline Element&)
get(const KeyType& key)
{
    // Find the cache line that might contain this key.
    const size_t index = m_key_hasher(key);
    LineType& line = m_lines[index % Lines];

    // Look for this key inside the cache line.
    EntryType* entry = line.find_entry(key);

    if (entry)
    {
        // The key was found in this cache line: cache hit.
        ++m_hit_count;
    }
    else
    {
        // The key was not found in this cache line: cache miss.
        ++m_miss_count;

        // Find which entry to replace in this cache line.
        entry = line.find_eviction_candidate();

        // Unload the old element.
        m_element_swapper.unload(entry->m_key, entry->m_element);

        // Load the new element.
        entry->m_key = key;
        m_element_swapper.load(entry->m_key, entry->m_element);
    }

    // Update the timestamp of this entry.
    line.touch_entry(entry, m_timestamper);

    // Return the corresponding element.
    return entry->m_element;
}

FOUNDATION_SACACHE_TEMPLATE_DEF(inline void)
invalidate(const KeyType& key)
{
    // Find the cache line that might contain this key.
    const size_t index = m_key_hasher(key);
    LineType& line = m_lines[index % Lines];

    // Look for this key inside the cache line.
    EntryType* entry = line.find_entry(key);

    if (entry)
    {
        // Unload the element.
        m_element_swapper.unload(entry->m_key, entry->m_element);

        // Mark the element as invalidated.
        entry->m_key = m_invalid_key;
    }
}

FOUNDATION_SACACHE_TEMPLATE_DEF(inline size_t)
get_memory_size() const
{
    return sizeof(*this);
}

FOUNDATION_SACACHE_TEMPLATE_DEF(template <typename IntegrityChecker> void)
check_integrity(IntegrityChecker& checker) const
{
    for (size_t i = 0; i < Lines; ++i)
        m_lines[i].check_integrity(checker);
}

#undef FOUNDATION_SACACHE_TEMPLATE_DEF


//
// LRUCache class implementation.
//

#define FOUNDATION_LRUCACHE_TEMPLATE_DEF(MiddleDecl)    \
    template <                                          \
        typename    Key,                                \
        typename    Element,                            \
        typename    ElementSwapper                      \
    >                                                   \
    MiddleDecl                                          \
    LRUCache<                                           \
        Key,                                            \
        Element,                                        \
        ElementSwapper                                  \
    >::

FOUNDATION_LRUCACHE_TEMPLATE_DEF(FOUNDATION_EMPTY)
LRUCache(ElementSwapperType& element_swapper)
  : m_element_swapper(element_swapper)
{
    clear();
}

FOUNDATION_LRUCACHE_TEMPLATE_DEF(void)
clear()
{
    m_lines.clear();
    m_index.clear();
    m_queue.clear();
    m_memory_size = 0;
}

FOUNDATION_LRUCACHE_TEMPLATE_DEF(inline Element&)
get(const KeyType& key)
{
    // Search for this key in the index.
    typename Index::iterator index_it = m_index.find(key);

    if (index_it != m_index.end())
    {
        // The key was found in the index: cache hit.
        ++m_hit_count;

        if (m_lines.size() > 1)
        {
            // Move the element to the front of the queue.
            m_queue.splice(
                m_queue.begin(),
                m_queue,
                index_it->second.m_queue_it);

            // Update the queue iterator in the index.
            index_it->second.m_queue_it = m_queue.begin();
        }

        // Return the element.
        return m_lines[index_it->second.m_line_index].m_element;
    }
    else
    {
        // The key was not found in the index: cache miss.
        ++m_miss_count;

        // Let the element swapper decide whether to replace the least
        // recently used element, or to simply insert the element into
        // the cache.
        const bool is_full =
            m_element_swapper.is_full(
                m_lines.size(),
                m_memory_size);

        size_t line_index;
        if (is_full)
        {
            // Locate the least recently used element.
            line_index = m_queue.back();

            // Unload the least recently used element.
            m_memory_size -= dynamic_sizeof(m_lines[line_index].m_element);
            m_element_swapper.unload(
                m_lines[line_index].m_key,
                m_lines[line_index].m_element);

            // Remove the least recently used element from the queue.
            m_queue.pop_back();

            // Remove the least recently used element from the index.
            m_index.erase(m_lines[line_index].m_key);
        }
        else
        {
            // Insert a new line into the cache storage.
            line_index = m_lines.size();
            m_lines.push_back(Line());
        }

        // Load the new element.
        m_lines[line_index].m_key = key;
        m_element_swapper.load(
            m_lines[line_index].m_key,
            m_lines[line_index].m_element);
        m_memory_size += dynamic_sizeof(m_lines[line_index].m_element);

        // Insert the new element into the queue.
        m_queue.push_front(line_index);

        // Insert the new element into the index.
        IndexEntry index_entry;
        index_entry.m_line_index = line_index;
        index_entry.m_queue_it = m_queue.begin();
        m_index[key] = index_entry;

        // Return the element.
        return m_lines[line_index].m_element;
    }
}

FOUNDATION_LRUCACHE_TEMPLATE_DEF(inline size_t)
get_memory_size() const
{
    return
          sizeof(*this)
        + sizeof(Line) * m_lines.capacity()
        + sizeof(KeyType) * m_index.size()
        + sizeof(IndexEntry*) * m_index.size()
        + sizeof(size_t) * m_lines.size()               // m_queue.size() == m_lines.size()
        + m_memory_size;
}

FOUNDATION_LRUCACHE_TEMPLATE_DEF(template <typename IntegrityChecker> void)
check_integrity(IntegrityChecker& checker) const
{
    for (size_t i = 0; i < m_lines.size(); ++i)
        checker(m_lines[i].m_key, m_lines[i].m_element);
}

#undef FOUNDATION_LRUCACHE_TEMPLATE_DEF


//
// DualStageCache class implementation.
//

#define FOUNDATION_DSCACHE_TEMPLATE_DEF(MiddleDecl)     \
    template <                                          \
        typename    Key,                                \
        typename    KeyHasher,                          \
        typename    Element,                            \
        typename    ElementSwapper,                     \
        size_t      Lines_,                             \
        size_t      Ways_                               \
    >                                                   \
    MiddleDecl                                          \
    DualStageCache<                                     \
        Key,                                            \
        KeyHasher,                                      \
        Element,                                        \
        ElementSwapper,                                 \
        Lines_,                                         \
        Ways_                                           \
    >::

FOUNDATION_DSCACHE_TEMPLATE_DEF(FOUNDATION_EMPTY)
DualStageCache(
    KeyHasherType&      key_hasher,
    ElementSwapperType& element_swapper,
    const KeyType&      invalid_key)
  : m_s1_element_swapper(m_s0_cache, element_swapper)   // warning: referring to an uninitialized member
  , m_s1_cache(m_s1_element_swapper)
  , m_s0_element_swapper(m_s1_cache)
  , m_s0_cache(key_hasher, m_s0_element_swapper, invalid_key)
{
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(void)
clear()
{
    m_s0_cache.clear();
    m_s1_cache.clear();
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline Element&)
get(const KeyType& key)
{
    return m_s0_cache.get(key);
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline size_t)
get_memory_size() const
{
    return
          sizeof(*this)
        + m_s0_cache.get_memory_size()
        + m_s1_cache.get_memory_size();
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline void)
clear_statistics()
{
    m_s0_cache.clear_statistics();
    m_s1_cache.clear_statistics();
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline uint64)
get_stage0_hit_count() const
{
    return m_s0_cache.get_hit_count();
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline uint64)
get_stage0_miss_count() const
{
    return m_s0_cache.get_miss_count();
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline uint64)
get_stage1_hit_count() const
{
    return m_s1_cache.get_hit_count();
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline uint64)
get_stage1_miss_count() const
{
    return m_s1_cache.get_miss_count();
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(template <typename IntegrityChecker> void)
check_integrity(IntegrityChecker& checker) const
{
    m_s0_cache.check_integrity(checker);
    m_s1_cache.check_integrity(checker);
}

#undef FOUNDATION_DSCACHE_TEMPLATE_DEF


//
// Utility functions implementation.
//

template <typename Cache>
std::string format_cache_stats(const Cache& cache)
{
    return
        format_cache_stats(
            cache.get_hit_count(),
            cache.get_miss_count());
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_CACHE_H
