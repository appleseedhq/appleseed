
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

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iterators.h"
#include "foundation/utility/statistics.h"

// Boost headers.
#include "boost/unordered_map.hpp"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <list>
#include <memory>
#include <string>
#include <utility>

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
        std::uint64_t get_hit_count() const
        {
            return m_hit_count;
        }

        // Return the number of cache misses.
        std::uint64_t get_miss_count() const
        {
            return m_miss_count;
        }

      protected:
        std::uint64_t   m_hit_count;
        std::uint64_t   m_miss_count;
    };


    //
    // Timestamp type.
    //

    typedef size_t Timestamp;


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

    template <typename Key, typename Element, size_t Ways_>
    class SACacheLine
      : public NonCopyable
    {
      public:
        typedef Key KeyType;
        typedef Element ElementType;
        typedef SACacheEntry<Key, Element, true> EntryType;
        static const size_t Ways = Ways_;

        // Return a given entry.
        EntryType& get_entry(const size_t i)
        {
            assert(i < Ways);
            return m_entries[i];
        }
        const EntryType& get_entry(const size_t i) const
        {
            assert(i < Ways);
            return m_entries[i];
        }

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

            return nullptr;
        }

        // Update the timestamp of a given entry in this cache line.
        void touch_entry(EntryType* entry, const Timestamp timestamp)
        {
            entry->m_timestamp = timestamp;
        }

        // Find an entry to replace in this cache line.
        EntryType* find_eviction_candidate()
        {
            size_t oldest_index = 0;
            Timestamp oldest_timestamp = m_entries[0].m_timestamp;

            for (size_t i = 1; i < Ways; ++i)
            {
                if (oldest_timestamp > m_entries[i].m_timestamp)
                {
                    oldest_index = i;
                    oldest_timestamp = m_entries[i].m_timestamp;
                }
            }

            return &m_entries[oldest_index];
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
        typedef Key KeyType;
        typedef Element ElementType;
        typedef SACacheEntry<Key, Element, false> EntryType;
        static const size_t Ways = 1;

        // Return a given entry.
        EntryType& get_entry(const size_t i)
        {
            assert(i < Ways);
            return m_entry;
        }
        const EntryType& get_entry(const size_t i) const
        {
            assert(i < Ways);
            return m_entry;
        }

        // Invalidate all entries of the cache line.
        void invalidate(const KeyType& invalid_key)
        {
            m_entry.m_key = invalid_key;
        }

        // Return a pointer to the entry corresponding to a given key, or 0 if this key was not found.
        EntryType* find_entry(const KeyType& key)
        {
            return m_entry.m_key == key ? &m_entry : nullptr;
        }

        // Update the timestamp of a given entry in this cache line.
        void touch_entry(EntryType* entry, const Timestamp timestamp)
        {
        }

        // Find an entry to replace in this cache line.
        EntryType* find_eviction_candidate()
        {
            return &m_entry;
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
        typedef Key KeyType;
        typedef Element ElementType;
        typedef SACacheEntry<Key, Element, false> EntryType;
        static const size_t Ways = 2;

        // Return a given entry.
        EntryType& get_entry(const size_t i)
        {
            assert(i < Ways);
            return m_entries[i];
        }
        const EntryType& get_entry(const size_t i) const
        {
            assert(i < Ways);
            return m_entries[i];
        }

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
                m_entries[1].m_key == key ? &m_entries[1] : nullptr;
        }

        // Update the timestamp of a given entry in this cache line.
        void touch_entry(EntryType* entry, const Timestamp timestamp)
        {
            m_oldest = 1 - (entry - m_entries);
        }

        // Find an entry to replace in this cache line.
        EntryType* find_eviction_candidate()
        {
            return &m_entries[m_oldest];
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

    // Destructor.
    ~SACache();

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
    cache_impl::Timestamp   m_timestamp;
    LineType                m_lines[Lines];             // cache storage
};


//
// LRU cache.
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
//          // Unload a cache line. Return true if unloading succeeded,
//          // false if the element could not be unloaded.
//          bool unload(const Key key, Element& element);
//
//          // Return true if the cache is full, false otherwise.
//          // If true is returned, the least recently used element
//          // will be replaced next time an element needs to be
//          // loaded into the cache. If false is returned, the cache
//          // will grow in order to accommodate the new element.
//          bool is_full(const size_t element_count);
//      };
//

template <
    typename    Key,
    typename    KeyHasher,
    typename    Element,
    typename    ElementSwapper,
    typename    Allocator = std::allocator<void>
>
class LRUCache
  : public cache_impl::CacheBase
{
  public:
    // Types.
    typedef Key             KeyType;
    typedef KeyHasher       KeyHasherType;
    typedef Element         ElementType;
    typedef ElementSwapper  ElementSwapperType;
    typedef Allocator       AllocatorType;

    // Constructor.
    LRUCache(
        KeyHasherType&      key_hasher,
        ElementSwapperType& element_swapper,
        AllocatorType       allocator = AllocatorType());

    // Destructor.
    ~LRUCache();

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

    // Queue: stores cache lines ordered from MRU to LRU.
    typedef std::list<
        Line,
        typename AllocatorType::template rebind<Line>::other
    > Queue;
    typedef typename Queue::iterator QueueIterator;

    // Index: given a key, find the cache line in the queue.
    typedef boost::unordered_map<
        KeyType,
        QueueIterator,
        KeyHasherType,
        std::equal_to<KeyType>,
        typename AllocatorType::template rebind<
            std::pair<const KeyType, QueueIterator>
        >::other
    > Index;

    Index                   m_index;
    Queue                   m_queue;
    size_t                  m_queue_size;
    ElementSwapperType&     m_element_swapper;
};


//
// Dual stage cache: the front end is a set associative cache, and the
// back-end is a LRU cache.
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
//          // Unload a cache line. Return true if unloading succeeded,
//          // false if the element could not be unloaded.
//          bool unload(const Key key, Element& element);
//
//          // Return true if the cache is full, false otherwise.
//          // If true is returned, the least recently used element
//          // will be replaced next time an element needs to be
//          // loaded into the cache. If false is returned, the cache
//          // will grow in order to accommodate the new element.
//          bool is_full(const size_t element_count);
//      };
//

template <
    typename    Key,
    typename    KeyHasher,
    typename    Element,
    typename    ElementSwapper,
    size_t      Lines_,             // number of lines of the set associative cache
    size_t      Ways_ = 1,          // number of ways of the set associate cache
    typename    Allocator = std::allocator<void>
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
    typedef Allocator       AllocatorType;

    // Line and way count.
    static const size_t Lines = Lines_;
    static const size_t Ways = Ways_;

    // Constructor.
    DualStageCache(
        KeyHasherType&      key_hasher,
        ElementSwapperType& element_swapper,
        const KeyType&      invalid_key,
        AllocatorType       allocator = AllocatorType());

    // Clear the cache.
    void clear();

    // Get an element from the cache.
    ElementType& get(const KeyType& key);

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

    // Reset the cache performance statistics.
    void clear_statistics();

    // Return the number of cache hits/misses in stage-0.
    std::uint64_t get_stage0_hit_count() const;
    std::uint64_t get_stage0_miss_count() const;

    // Return the number of cache hits/misses in stage-1.
    std::uint64_t get_stage1_hit_count() const;
    std::uint64_t get_stage1_miss_count() const;

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
        Ways
    > S0Cache;

    // Stage-1 cache type.
    class S1ElementSwapper;
    typedef LRUCache<
        KeyType,
        KeyHasherType,
        ElementType,
        S1ElementSwapper,
        AllocatorType
    > S1Cache;

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

        bool unload(const KeyType& key, ElementType& element)
        {
            if (m_swapper.unload(key, element))
            {
                m_s0_cache.invalidate(key);
                return true;
            }
            else return false;
        }

        bool is_full(const size_t element_count) const
        {
            return m_swapper.is_full(element_count);
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

template <typename Cache>
Statistics make_single_stage_cache_stats(const Cache& cache);

template <typename Cache>
Statistics make_dual_stage_cache_stats(const Cache& cache);


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

FOUNDATION_SACACHE_TEMPLATE_DEF(APPLESEED_EMPTY)
SACache(
    KeyHasherType&      key_hasher,
    ElementSwapperType& element_swapper,
    const KeyType&      invalid_key)
  : m_key_hasher(key_hasher)
  , m_element_swapper(element_swapper)
  , m_invalid_key(invalid_key)
  , m_timestamp(0)
{
    clear();
}

FOUNDATION_SACACHE_TEMPLATE_DEF(APPLESEED_EMPTY)
~SACache()
{
    for (size_t i = 0; i < Lines; ++i)
    {
        for (size_t j = 0; j < LineType::Ways; ++j)
        {
            EntryType& entry = m_lines[i].get_entry(j);
            if (entry.m_key != m_invalid_key)
                m_element_swapper.unload(entry.m_key, entry.m_element);
        }
    }
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
        if (entry->m_key != m_invalid_key)
            m_element_swapper.unload(entry->m_key, entry->m_element);

        // Load the new element.
        m_element_swapper.load(key, entry->m_element);

        // Set the entry's key only after loading succeeded (it might have failed with an exception).
        entry->m_key = key;
    }

    // Update the timestamp of this entry.
    line.touch_entry(entry, m_timestamp++);

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
    {
        for (size_t j = 0; j < LineType::Ways; ++j)
        {
            const EntryType& entry = m_lines[i].get_entry(j);
            checker(entry.m_key, entry.m_element);
        }
    }
}

#undef FOUNDATION_SACACHE_TEMPLATE_DEF


//
// LRUCache class implementation.
//

#define FOUNDATION_LRUCACHE_TEMPLATE_DEF(MiddleDecl)    \
    template <                                          \
        typename    Key,                                \
        typename    KeyHasher,                          \
        typename    Element,                            \
        typename    ElementSwapper,                     \
        typename    Allocator                           \
    >                                                   \
    MiddleDecl                                          \
    LRUCache<                                           \
        Key,                                            \
        KeyHasher,                                      \
        Element,                                        \
        ElementSwapper,                                 \
        Allocator                                       \
    >::

FOUNDATION_LRUCACHE_TEMPLATE_DEF(APPLESEED_EMPTY)
LRUCache(
    KeyHasherType&      key_hasher,
    ElementSwapperType& element_swapper,
    AllocatorType       allocator)
  : m_index(4, key_hasher, typename Index::key_equal(), allocator)
  , m_queue(allocator)
  , m_queue_size(0)
  , m_element_swapper(element_swapper)
{
}

FOUNDATION_LRUCACHE_TEMPLATE_DEF(APPLESEED_EMPTY)
~LRUCache()
{
    clear();
}

FOUNDATION_LRUCACHE_TEMPLATE_DEF(void)
clear()
{
    for (each<Queue> i = m_queue; i; ++i)
    {
#ifndef NDEBUG
        const bool success =
#endif
        m_element_swapper.unload(i->m_key, i->m_element);
        assert(success);
    }

    m_index.clear();
    m_queue.clear();
    m_queue_size = 0;
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

        if (m_queue_size > 1)
        {
            // Move the element to the front of the queue.
            m_queue.splice(
                m_queue.begin(),
                m_queue,
                index_it->second);

            // Update the queue iterator in the index.
            index_it->second = m_queue.begin();
        }

        // Return the element.
        return index_it->second->m_element;
    }
    else
    {
        // The key was not found in the index: cache miss.
        ++m_miss_count;

        // Load the new element.
        Line line;
        line.m_key = key;
        m_element_swapper.load(line.m_key, line.m_element);

        // Insert the new element into the queue.
        m_queue.push_front(line);
        ++m_queue_size;

        // Insert the new element into the index.
        m_index[key] = m_queue.begin();

        typename Queue::reverse_iterator i = m_queue.rbegin();

        while (m_element_swapper.is_full(m_queue_size) && i != pred(m_queue.rend()))
        {
            // Try to unload this element.
            if (m_element_swapper.unload(i->m_key, i->m_element))
            {
                // Remove this element from the index.
                m_index.erase(i->m_key);

                // Remove this element from the queue.
                // http://stackoverflow.com/questions/1830158/how-to-call-erase-with-a-reverse-iterator
                m_queue.erase(succ(i).base());
                --m_queue_size;
            }
            else
            {
                // Unloading this element failed, try the next one.
                ++i;
            }
        }

        // Return the element.
        return m_queue.front().m_element;
    }
}

FOUNDATION_LRUCACHE_TEMPLATE_DEF(inline size_t)
get_memory_size() const
{
    return
          sizeof(*this)
        + sizeof(typename Index::value_type) * m_index.size()
        + sizeof(Line) * m_queue_size;
}

FOUNDATION_LRUCACHE_TEMPLATE_DEF(template <typename IntegrityChecker> void)
check_integrity(IntegrityChecker& checker) const
{
    for (const_each<Queue> i = m_queue; i; ++i)
        checker(i->m_key, i->m_element);
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
        size_t      Ways_,                              \
        typename    Allocator                           \
    >                                                   \
    MiddleDecl                                          \
    DualStageCache<                                     \
        Key,                                            \
        KeyHasher,                                      \
        Element,                                        \
        ElementSwapper,                                 \
        Lines_,                                         \
        Ways_,                                          \
        Allocator                                       \
    >::

FOUNDATION_DSCACHE_TEMPLATE_DEF(APPLESEED_EMPTY)
DualStageCache(
    KeyHasherType&      key_hasher,
    ElementSwapperType& element_swapper,
    const KeyType&      invalid_key,
    AllocatorType       allocator)
  : m_s1_element_swapper(m_s0_cache, element_swapper)   // warning: referring to an uninitialized member
  , m_s1_cache(key_hasher, m_s1_element_swapper, allocator)
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

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline std::uint64_t)
get_stage0_hit_count() const
{
    return m_s0_cache.get_hit_count();
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline std::uint64_t)
get_stage0_miss_count() const
{
    return m_s0_cache.get_miss_count();
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline std::uint64_t)
get_stage1_hit_count() const
{
    return m_s1_cache.get_hit_count();
}

FOUNDATION_DSCACHE_TEMPLATE_DEF(inline std::uint64_t)
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

namespace cache_impl
{
    struct CacheStatisticsEntry
      : public Statistics::Entry
    {
        std::uint64_t   m_hit_count;
        std::uint64_t   m_miss_count;

        CacheStatisticsEntry(
            const std::string&      name,
            const std::uint64_t     hit_count,
            const std::uint64_t     miss_count);

        std::unique_ptr<Entry> clone() const override;
        void merge(const Entry* other) override;
        std::string to_string() const override;
    };
}

template <typename Cache>
Statistics make_single_stage_cache_stats(const Cache& cache)
{
    Statistics stats;

    stats.insert(
        std::unique_ptr<cache_impl::CacheStatisticsEntry>(
            new cache_impl::CacheStatisticsEntry(
                "performance",
                cache.get_hit_count(),
                cache.get_miss_count())));

    return stats;
}

template <typename Cache>
Statistics make_dual_stage_cache_stats(const Cache& cache)
{
    Statistics stats;

    stats.insert(
        std::unique_ptr<cache_impl::CacheStatisticsEntry>(
            new cache_impl::CacheStatisticsEntry(
                "combined",
                cache.get_stage0_hit_count() + cache.get_stage1_hit_count(),
                cache.get_stage1_miss_count())));

    stats.insert(
        std::unique_ptr<cache_impl::CacheStatisticsEntry>(
            new cache_impl::CacheStatisticsEntry(
                "stage-0",
                cache.get_stage0_hit_count(),
                cache.get_stage0_miss_count())));

    stats.insert(
        std::unique_ptr<cache_impl::CacheStatisticsEntry>(
            new cache_impl::CacheStatisticsEntry(
                "stage-1",
                cache.get_stage1_hit_count(),
                cache.get_stage1_miss_count())));

    return stats;
}

}   // namespace foundation
