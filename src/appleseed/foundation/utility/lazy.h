
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_LAZY_H
#define APPLESEED_FOUNDATION_UTILITY_LAZY_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/core/exceptions/exception.h"
#include "foundation/platform/thread.h"
#include "foundation/platform/types.h"
#include "foundation/utility/cache.h"
#include "foundation/utility/typetraits.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>

namespace foundation
{

//
// A factory interface for lazy object construction.
//

template <typename Object>
class ILazyFactory
  : public NonCopyable
{
  public:
    // Object type.
    typedef Object ObjectType;

    // Destructor.
    virtual ~ILazyFactory() {}

    // Create the object.
    virtual std::auto_ptr<Object> create() = 0;
};


//
// A lazily constructed object.
//

template <typename Object>
class Lazy
  : public NonCopyable
{
  public:
    // Object, lazy object and object factory types.
    typedef Object ObjectType;
    typedef Lazy<Object> LazyType;
    typedef ILazyFactory<Object> FactoryType;

    // Construct a lazy object with a factory to create the actual
    // object the first time it is accessed.
    explicit Lazy(std::auto_ptr<FactoryType> factory);

    // Construct a lazy object that simply wraps an existing source object,
    // effectively bypassing lazy object construction altogether.
    explicit Lazy(ObjectType* source_object);

    // Destructor, deletes the factory, as well as the object if
    // it is owned by the lazy object.
    ~Lazy();

    // Return the factory associated with that lazy object, if any.
    FactoryType* get_factory() const;

    // Return the source object associated with that lazy object, if any.
    ObjectType* get_source_object() const;

  private:
    template <typename> friend class Access;

    boost::mutex    m_mutex;
    int             m_reference_count;

    FactoryType*    m_factory;
    ObjectType*     m_source_object;
    ObjectType*     m_object;
    const bool      m_own_object;
};


//
// Thread-safe read/write access to a lazily constructed object.
//

template <typename Object>
class Access
{
  public:
    // Object and lazy object types.
    typedef Object ObjectType;
    typedef Lazy<Object> LazyType;

    // Constructor, acquires access to a lazy object.
    explicit Access(LazyType* lazy = 0);

    // Copy constructor.
    Access(const Access& rhs);

    // Assignment operator.
    Access<Object>& operator=(const Access& rhs);

    // Destructor.
    ~Access();

    // Acquire access to another lazy object.
    // Calling reset(0) releases access to the current lazy object,
    // if any. Note that releasing access to a lazy object does not
    // imply that the object is deleted, even if the reference count
    // on this object has reached 0. An object is deleted only if it
    // is garbage-collected.
    void reset(LazyType* lazy);

    // Get the object pointer.
    ObjectType* get() const;
    ObjectType& ref() const;

    // Access the object.
    ObjectType* operator->() const;
    ObjectType& operator*() const;

  private:
    LazyType*   m_lazy;
};


//
// Access cache.
//

template <
    typename    Object,
    size_t      Lines,
    size_t      Ways = 1,
    typename    Allocator = std::allocator<void>
>
class AccessCache
  : public NonCopyable
{
  public:
    // Types.
    typedef UniqueID        KeyType;
    typedef Object          ObjectType;
    typedef Lazy<Object>    LazyType;
    typedef Access<Object>  AccessType;
    typedef Allocator       AllocatorType;

    // Constructor.
    AccessCache();

    // Access the object corresponding to a given key.
    const ObjectType* access(
        const KeyType&      key,
        LazyType&           lazy) const;

    // Reset the cache performance statistics.
    void clear_statistics();

    // Return the number of cache hits/misses in stage-0.
    uint64 get_stage0_hit_count() const;
    uint64 get_stage0_miss_count() const;

    // Return the number of cache hits/misses in stage-1.
    uint64 get_stage1_hit_count() const;
    uint64 get_stage1_miss_count() const;

  private:
    // Key hasher.
    struct KeyHasher
    {
        // Hash a key.
        size_t operator()(const KeyType& key) const;
    };

    // Object swapper.
    struct ObjectSwapper
    {
        LazyType* m_lazy;

        // Constructor.
        ObjectSwapper();

        // Set the lazy object to access in case of a cache miss.
        void set_lazy(LazyType& lazy);

        // Load a cache line.
        void load(
            const KeyType&  key,
            AccessType&     access);

        // Unload a cache line.
        bool unload(
            const KeyType&  key,
            AccessType&     access);

        // Return true if the cache is full.
        bool is_full(const size_t element_count) const;
    };

    // Cache type.
    typedef DualStageCache<
        KeyType,
        KeyHasher,
        AccessType,
        ObjectSwapper,
        Lines,
        Ways,
        AllocatorType
    > CacheType;

    KeyHasher               m_key_hasher;
    mutable ObjectSwapper   m_object_swapper;
    mutable CacheType       m_cache;
};

template <
    typename    ObjectMap,
    size_t      Lines,
    size_t      Ways = 1,
    typename    Allocator = std::allocator<void>
>
class AccessCacheMap
  : public NonCopyable
{
  public:
    // Types.
    typedef UniqueID                                KeyType;
    typedef typename ObjectMap::mapped_type         LazyPtrType;
    typedef typename PointerDeref<LazyPtrType>::R   LazyType;
    typedef typename LazyType::ObjectType           ObjectType;
    typedef Access<ObjectType>                      AccessType;
    typedef Allocator                               AllocatorType;

    // Exception thrown when looking for an object that does not exist.
    struct ExceptionObjectNotFound : public Exception {};

    // Constructor.
    AccessCacheMap();

    // Access the object corresponding to a given key.
    const ObjectType* access(
        const KeyType&      key,
        const ObjectMap&    object_map) const;

    // Reset the cache performance statistics.
    void clear_statistics();

    // Return the number of cache hits/misses in stage-0.
    uint64 get_stage0_hit_count() const;
    uint64 get_stage0_miss_count() const;

    // Return the number of cache hits/misses in stage-1.
    uint64 get_stage1_hit_count() const;
    uint64 get_stage1_miss_count() const;

  private:
    // Key hasher.
    struct KeyHasher
    {
        // Hash a key.
        size_t operator()(const KeyType& key) const;
    };

    // Object swapper.
    struct ObjectSwapper
    {
        // The key -> object map to look objects up.
        const ObjectMap* m_object_map;

        // Constructor.
        ObjectSwapper();

        // Set the key -> lazy object map to look objects up.
        void set_object_map(const ObjectMap* object_map);

        // Load a cache line.
        void load(
            const KeyType&  key,
            AccessType&     access);

        // Unload a cache line.
        bool unload(
            const KeyType&  key,
            AccessType&     access);

        // Return true if the cache is full.
        bool is_full(const size_t element_count) const;
    };

    // Cache type.
    typedef DualStageCache<
        KeyType,
        KeyHasher,
        AccessType,
        ObjectSwapper,
        Lines,
        Ways,
        AllocatorType
    > CacheType;

    KeyHasher               m_key_hasher;
    mutable ObjectSwapper   m_object_swapper;
    mutable CacheType       m_cache;
};


//
// Lazy class implementation.
//

template <typename Object>
Lazy<Object>::Lazy(std::auto_ptr<FactoryType> factory)
  : m_reference_count(0)
  , m_factory(factory.release())
  , m_source_object(0)
  , m_object(0)
  , m_own_object(true)
{
    assert(m_factory);
}

template <typename Object>
Lazy<Object>::Lazy(ObjectType* source_object)
  : m_reference_count(0)
  , m_factory(0)
  , m_source_object(source_object)
  , m_object(0)
  , m_own_object(false)
{
    assert(m_source_object);
}

template <typename Object>
Lazy<Object>::~Lazy()
{
    boost::mutex::scoped_lock lock(m_mutex);
    assert(m_reference_count == 0);

    delete m_factory;

    if (m_own_object)
        delete m_object;
}

template <typename Object>
inline typename Lazy<Object>::FactoryType* Lazy<Object>::get_factory() const
{
    return m_factory;
}

template <typename Object>
inline typename Lazy<Object>::ObjectType* Lazy<Object>::get_source_object() const
{
    return m_source_object;
}


//
// Access class implementation.
//

template <typename Object>
inline Access<Object>::Access(LazyType* lazy)
  : m_lazy(0)
{
    reset(lazy);
}

template <typename Object>
inline Access<Object>::Access(const Access& rhs)
  : m_lazy(0)
{
    reset(rhs.m_lazy);
}

template <typename Object>
inline Access<Object>& Access<Object>::operator=(const Access& rhs)
{
    reset(rhs.m_lazy);
    return *this;
}

template <typename Object>
inline Access<Object>::~Access()
{
    reset(0);
}

template <typename Object>
void Access<Object>::reset(LazyType* lazy)
{
    // Release access to the current lazy object, if any.
    if (m_lazy)
    {
        boost::mutex::scoped_lock lock(m_lazy->m_mutex);
        assert(m_lazy->m_reference_count > 0);
        --m_lazy->m_reference_count;
    }

    m_lazy = lazy;

    // Acquire access to the new lazy object.
    if (m_lazy)
    {
        boost::mutex::scoped_lock lock(m_lazy->m_mutex);
        ++m_lazy->m_reference_count;

        // Create the object if it doesn't exist yet.
        if (m_lazy->m_object == 0)
        {
            if (m_lazy->m_factory)
                m_lazy->m_object = m_lazy->m_factory->create().release();
            else m_lazy->m_object = m_lazy->m_source_object;
        }
    }
}

template <typename Object>
inline Object* Access<Object>::get() const
{
    return m_lazy ? m_lazy->m_object : 0;
}

template <typename Object>
inline Object& Access<Object>::ref() const
{
    assert(m_lazy);
    assert(m_lazy->m_object);
    return *m_lazy->m_object;
}

template <typename Object>
inline Object* Access<Object>::operator->() const
{
    return m_lazy ? m_lazy->m_object : 0;
}

template <typename Object>
inline Object& Access<Object>::operator*() const
{
    assert(m_lazy);
    assert(m_lazy->m_object);
    return *m_lazy->m_object;
}


//
// AccessCache class implementation.
//

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
AccessCache<Object, Lines, Ways, Allocator>::AccessCache()
  : m_cache(m_key_hasher, m_object_swapper, ~0)
{
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
inline const Object* AccessCache<Object, Lines, Ways, Allocator>::access(
    const KeyType&      key,
    LazyType&           lazy) const
{
    m_object_swapper.set_lazy(lazy);
    return m_cache.get(key).get();
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
void AccessCache<Object, Lines, Ways, Allocator>::clear_statistics()
{
    return m_cache.clear_statistics();
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
inline uint64 AccessCache<Object, Lines, Ways, Allocator>::get_stage0_hit_count() const
{
    return m_cache.get_stage0_hit_count();
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
inline uint64 AccessCache<Object, Lines, Ways, Allocator>::get_stage0_miss_count() const
{
    return m_cache.get_stage0_miss_count();
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
inline uint64 AccessCache<Object, Lines, Ways, Allocator>::get_stage1_hit_count() const
{
    return m_cache.get_stage1_hit_count();
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
inline uint64 AccessCache<Object, Lines, Ways, Allocator>::get_stage1_miss_count() const
{
    return m_cache.get_stage1_miss_count();
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
inline size_t AccessCache<Object, Lines, Ways, Allocator>::KeyHasher::operator()(
    const KeyType&      key) const
{
    return static_cast<size_t>(key);
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
AccessCache<Object, Lines, Ways, Allocator>::ObjectSwapper::ObjectSwapper()
  : m_lazy(0)
{
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
inline void AccessCache<Object, Lines, Ways, Allocator>::ObjectSwapper::set_lazy(
    LazyType&           lazy)
{
    m_lazy = &lazy;
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
inline void AccessCache<Object, Lines, Ways, Allocator>::ObjectSwapper::load(
    const KeyType&      key,
    AccessType&         access)
{
    access.reset(m_lazy);
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
inline bool AccessCache<Object, Lines, Ways, Allocator>::ObjectSwapper::unload(
    const KeyType&      key,
    AccessType&         access)
{
    access.reset(0);
    return true;
}

template <typename Object, size_t Lines, size_t Ways, typename Allocator>
inline bool AccessCache<Object, Lines, Ways, Allocator>::ObjectSwapper::is_full(
    const size_t        element_count) const
{
    assert(element_count <= Lines * Ways);
    return element_count == Lines * Ways;
}


//
// AccessCacheMap class implementation.
//

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::AccessCacheMap()
  : m_cache(m_key_hasher, m_object_swapper, ~0)
{
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
inline const typename AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::ObjectType*
AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::access(
    const KeyType&      key,
    const ObjectMap&    object_map) const
{
    m_object_swapper.set_object_map(&object_map);
    return m_cache.get(key).get();
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
void AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::clear_statistics()
{
    return m_cache.clear_statistics();
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
inline uint64 AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::get_stage0_hit_count() const
{
    return m_cache.get_stage0_hit_count();
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
inline uint64 AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::get_stage0_miss_count() const
{
    return m_cache.get_stage0_miss_count();
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
inline uint64 AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::get_stage1_hit_count() const
{
    return m_cache.get_stage1_hit_count();
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
inline uint64 AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::get_stage1_miss_count() const
{
    return m_cache.get_stage1_miss_count();
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
inline size_t AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::KeyHasher::operator()(
    const KeyType&      key) const
{
    return static_cast<size_t>(key);
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::ObjectSwapper::ObjectSwapper()
  : m_object_map(0)
{
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
inline void AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::ObjectSwapper::set_object_map(
    const ObjectMap*    object_map)
{
    assert(object_map);
    m_object_map = object_map;
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
inline void AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::ObjectSwapper::load(
    const KeyType&      key,
    AccessType&         access)
{
    const typename ObjectMap::const_iterator i = m_object_map->find(key);
    access.reset(i != m_object_map->end() ? i->second : 0);
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
inline bool AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::ObjectSwapper::unload(
    const KeyType&      key,
    AccessType&         access)
{
    access.reset(0);
    return true;
}

template <typename ObjectMap, size_t Lines, size_t Ways, typename Allocator>
inline bool AccessCacheMap<ObjectMap, Lines, Ways, Allocator>::ObjectSwapper::is_full(
    const size_t        element_count) const
{
    assert(element_count <= Lines * Ways);
    return element_count == Lines * Ways;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_LAZY_H
