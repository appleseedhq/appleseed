
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

    // Construct a lazy object that simply wraps an existing object,
    // effectively bypassing lazy object construction altogether.
    explicit Lazy(const ObjectType* object);

    // Destructor, deletes the factory, as well as the object if
    // it is owned by the lazy object.
    ~Lazy();

  private:
    template <typename>
    friend class Access;

    boost::mutex        m_mutex;
    int                 m_reference_count;

    FactoryType*        m_factory;
    const ObjectType*   m_object;
    const bool          m_own_object;
};


//
// Thread-safe access to a lazily constructed object.
//

template <typename Object>
class Access
  : public NonCopyable
{
  public:
    // Object and lazy object types.
    typedef Object ObjectType;
    typedef Lazy<Object> LazyType;

    // Constructor, acquires access to a lazy object.
    explicit Access(LazyType* lazy = 0);

    // Destructor.
    ~Access();

    // Acquire access to another lazy object.
    // Calling reset(0) releases access to the current lazy object,
    // if any. Note that releasing access to a lazy object does not
    // imply that the object is deleted, even if the reference count
    // on this object has reached 0. An object is deleted only if it
    // is garbage collected.
    void reset(LazyType* lazy);

    // Get the object pointer.
    const ObjectType* get() const;

    // Access the object. The object must exist.
    const ObjectType* operator->() const;
    const ObjectType& operator*() const;

  private:
    LazyType*   m_lazy;
};


//
// Access cache.
//

template <typename Object, size_t Size = 16>
class AccessCache
  : public NonCopyable
{
  public:
    // Types.
    typedef foundation::UniqueID KeyType;
    typedef Object ObjectType;
    typedef Lazy<Object> LazyType;
    typedef Access<Object> AccessType;

    // Constructor.
    AccessCache();

    // Access the object corresponding to a given key.
    const ObjectType* access(
        const KeyType&      key,
        LazyType&           lazy) const;

    // Reset the cache performance statistics.
    void clear_statistics();

    // Return the number of cache hits since creation.
    uint64 get_hit_count() const;

    // Return the number of cache misses since creation.
    uint64 get_miss_count() const;

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

        // Set the lazy object to access, in case we would have
        // to load a cache line.
        void set_lazy(LazyType& lazy);

        // Load a cache line.
        void load(
            const KeyType&  key,
            AccessType&     access);

        // Unload a cache line.
        void unload(
            const KeyType&  key,
            AccessType&     access);
    };

    // Cache type.
    typedef SACache<
        KeyType,
        KeyHasher,
        AccessType,
        ObjectSwapper,
        Size
    > CacheType;

    KeyHasher               m_key_hasher;
    mutable ObjectSwapper   m_object_swapper;
    mutable CacheType       m_cache;
};

template <typename ObjectMap, size_t Size = 16>
class AccessCacheMap
  : public NonCopyable
{
  public:
    // Types.
    typedef foundation::UniqueID KeyType;
    typedef typename ObjectMap::mapped_type LazyPtrType;
    typedef typename PointerDeref<LazyPtrType>::R LazyType;
    typedef typename LazyType::ObjectType ObjectType;
    typedef Access<ObjectType> AccessType;

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

    // Return the number of cache hits since creation.
    uint64 get_hit_count() const;

    // Return the number of cache misses since creation.
    uint64 get_miss_count() const;

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
        void unload(
            const KeyType&  key,
            AccessType&     access);
    };

    // Cache type.
    typedef SACache<
        KeyType,
        KeyHasher,
        AccessType,
        ObjectSwapper,
        Size
    > CacheType;

    KeyHasher               m_key_hasher;
    mutable ObjectSwapper   m_object_swapper;
    mutable CacheType       m_cache;
};


//
// Lazy class implementation.
//

// Constructors.
template <typename Object>
Lazy<Object>::Lazy(std::auto_ptr<FactoryType> factory)
  : m_reference_count(0)
  , m_factory(factory.release())
  , m_object(0)
  , m_own_object(true)
{
    assert(m_factory);
}
template <typename Object>
Lazy<Object>::Lazy(const ObjectType* object)
  : m_reference_count(0)
  , m_factory(0)
  , m_object(object)
  , m_own_object(false)
{
}

// Destructor.
template <typename Object>
Lazy<Object>::~Lazy()
{
    boost::mutex::scoped_lock lock(m_mutex);
    assert(m_reference_count == 0);

    delete m_factory;
    m_factory = 0;

    if (m_own_object)
        delete m_object;
    m_object = 0;
}


//
// Access class implementation.
//

// Constructor.
template <typename Object>
inline Access<Object>::Access(LazyType* lazy)
  : m_lazy(0)
{
    reset(lazy);
}

// Destructor.
template <typename Object>
inline Access<Object>::~Access()
{
    reset(0);
}

// Acquire access to another lazy object.
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

    // Acquire access to the new lazy object.
    m_lazy = lazy;
    if (m_lazy)
    {
        boost::mutex::scoped_lock lock(m_lazy->m_mutex);
        ++m_lazy->m_reference_count;

        // Create the object if it doesn't exist yet.
        if (m_lazy->m_object == 0)
        {
            assert(m_lazy->m_factory);
            m_lazy->m_object = m_lazy->m_factory->create().release();
        }
    }
}

// Get the object pointer.
template <typename Object>
inline const Object* Access<Object>::get() const
{
    assert(m_lazy);
    return m_lazy->m_object;
}

// Access the object.
template <typename Object>
inline const Object* Access<Object>::operator->() const
{
    assert(m_lazy);
    assert(m_lazy->m_object);
    return m_lazy->m_object;
}
template <typename Object>
inline const Object& Access<Object>::operator*() const
{
    assert(m_lazy);
    assert(m_lazy->m_object);
    return *m_lazy->m_object;
}


//
// AccessCache class implementation.
//

// Constructor
template <typename Object, size_t Size>
AccessCache<Object, Size>::AccessCache()
  : m_cache(m_key_hasher, m_object_swapper, ~KeyType(0))
{
}

// Access the object corresponding to a given key.
template <typename Object, size_t Size>
inline const Object* AccessCache<Object, Size>::access(
    const KeyType&      key,
    LazyType&           lazy) const
{
    m_object_swapper.set_lazy(lazy);
    return m_cache.get(key).get();
}

// Reset the cache performance statistics.
template <typename Object, size_t Size>
void AccessCache<Object, Size>::clear_statistics()
{
    return m_cache.clear_statistics();
}

// Return the number of cache hits since creation.
template <typename Object, size_t Size>
inline uint64 AccessCache<Object, Size>::get_hit_count() const
{
    return m_cache.get_hit_count();
}

// Return the number of cache misses since creation.
template <typename Object, size_t Size>
inline uint64 AccessCache<Object, Size>::get_miss_count() const
{
    return m_cache.get_miss_count();
}

// Hash a key.
template <typename Object, size_t Size>
inline size_t AccessCache<Object, Size>::KeyHasher::operator()(
    const KeyType&      key) const
{
    return static_cast<size_t>(key);
}

// Constructor.
template <typename Object, size_t Size>
AccessCache<Object, Size>::ObjectSwapper::ObjectSwapper()
  : m_lazy(0)
{
}

// Set the lazy object to access, in case we would have to load a cache line.
template <typename Object, size_t Size>
inline void AccessCache<Object, Size>::ObjectSwapper::set_lazy(
    LazyType&           lazy)
{
    m_lazy = &lazy;
}

// Load a cache line.
template <typename Object, size_t Size>
inline void AccessCache<Object, Size>::ObjectSwapper::load(
    const KeyType&      key,
    AccessType&         access)
{
    access.reset(m_lazy);
}

// Unload a cache line.
template <typename Object, size_t Size>
inline void AccessCache<Object, Size>::ObjectSwapper::unload(
    const KeyType&      key,
    AccessType&         access)
{
    access.reset(0);
}


//
// AccessCacheMap class implementation.
//

// Constructor
template <typename ObjectMap, size_t Size>
AccessCacheMap<ObjectMap, Size>::AccessCacheMap()
  : m_cache(m_key_hasher, m_object_swapper, ~KeyType(0))
{
}

// Access the object corresponding to a given key.
template <typename ObjectMap, size_t Size>
inline const typename AccessCacheMap<ObjectMap, Size>::ObjectType*
AccessCacheMap<ObjectMap, Size>::access(
    const KeyType&      key,
    const ObjectMap&    object_map) const
{
    m_object_swapper.set_object_map(&object_map);
    return m_cache.get(key).get();
}

// Reset the cache performance statistics.
template <typename ObjectMap, size_t Size>
void AccessCacheMap<ObjectMap, Size>::clear_statistics()
{
    return m_cache.clear_statistics();
}

// Return the number of cache hits since creation.
template <typename ObjectMap, size_t Size>
inline uint64 AccessCacheMap<ObjectMap, Size>::get_hit_count() const
{
    return m_cache.get_hit_count();
}

// Return the number of cache misses since creation.
template <typename ObjectMap, size_t Size>
inline uint64 AccessCacheMap<ObjectMap, Size>::get_miss_count() const
{
    return m_cache.get_miss_count();
}

// Hash a key.
template <typename ObjectMap, size_t Size>
inline size_t AccessCacheMap<ObjectMap, Size>::KeyHasher::operator()(
    const KeyType&      key) const
{
    return static_cast<size_t>(key);
}

// Constructor.
template <typename ObjectMap, size_t Size>
AccessCacheMap<ObjectMap, Size>::ObjectSwapper::ObjectSwapper()
  : m_object_map(0)
{
}

// Set the key -> lazy object map to look objects up.
template <typename ObjectMap, size_t Size>
inline void AccessCacheMap<ObjectMap, Size>::ObjectSwapper::set_object_map(
    const ObjectMap*    object_map)
{
    assert(object_map);
    m_object_map = object_map;
}

// Load a cache line.
template <typename ObjectMap, size_t Size>
inline void AccessCacheMap<ObjectMap, Size>::ObjectSwapper::load(
    const KeyType&      key,
    AccessType&         access)
{
    const typename ObjectMap::const_iterator i = m_object_map->find(key);
    if (i == m_object_map->end())
        throw ExceptionObjectNotFound();
    access.reset(i->second);
}

// Unload a cache line.
template <typename ObjectMap, size_t Size>
inline void AccessCacheMap<ObjectMap, Size>::ObjectSwapper::unload(
    const KeyType&      key,
    AccessType&         access)
{
    access.reset(0);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_LAZY_H
