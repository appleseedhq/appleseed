
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
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

namespace renderer
{

//
// An entity container providing insertion of deletion of entities.
//

class APPLESEED_DLLSYMBOL EntityMap
  : public foundation::NonCopyable
{
  public:
    // Mutable iterator.
    class APPLESEED_DLLSYMBOL iterator
    {
      public:
        // Iterator category.
        typedef std::bidirectional_iterator_tag iterator_category;

        // Value type.
        typedef Entity value_type;
        typedef value_type& reference;
        typedef value_type* pointer;
        typedef std::ptrdiff_t difference_type;

        // Constructors.
        iterator();
        iterator(const iterator& rhs);

        // Destructor.
        ~iterator();

        // Assignment operator.
        iterator& operator=(const iterator& rhs);

        // Equality and inequality tests.
        bool operator==(const iterator& rhs) const;
        bool operator!=(const iterator& rhs) const;

        // Preincrement and predecrement operators.
        iterator& operator++();
        iterator& operator--();

        // Postincrement and postdecrement operators.
        iterator operator++(int);
        iterator operator--(int);

        // Dereference operators.
        reference operator*() const;
        pointer operator->() const;

      private:
        friend class EntityMap;

        struct Impl;
        Impl* impl;
    };

    // Constant iterator.
    class APPLESEED_DLLSYMBOL const_iterator
    {
      public:
        // Iterator category.
        typedef std::bidirectional_iterator_tag iterator_category;

        // Value type.
        typedef Entity value_type;
        typedef const value_type& reference;
        typedef const value_type* pointer;
        typedef std::ptrdiff_t difference_type;

        // Constructors.
        const_iterator();
        const_iterator(const const_iterator& rhs);

        // Destructor.
        ~const_iterator();

        // Assignment operator.
        const_iterator& operator=(const const_iterator& rhs);

        // Equality and inequality tests.
        bool operator==(const const_iterator& rhs) const;
        bool operator!=(const const_iterator& rhs) const;

        // Preincrement and predecrement operators.
        const_iterator& operator++();
        const_iterator& operator--();

        // Postincrement and postdecrement operators.
        const_iterator operator++(int);
        const_iterator operator--(int);

        // Dereference operators.
        reference operator*() const;
        pointer operator->() const;

      private:
        friend class EntityMap;

        struct Impl;
        Impl* impl;
    };

    // Constructor.
    explicit EntityMap(Entity* parent = nullptr);

    // Destructor.
    ~EntityMap();

    // Swap the content of this container with another container.
    void swap(EntityMap& rhs);

    // Remove all entities from the container.
    void clear();

    // Return the number of entities in the container.
    size_t size() const;

    // Return true if the container is empty.
    bool empty() const;

    // Insert an entity into the container.
    void insert(foundation::auto_release_ptr<Entity> entity);

    // Remove an entity from the container.
    // Note that if nothing is done with the return value, the entity will be deleted.
    foundation::auto_release_ptr<Entity> remove(const foundation::UniqueID id);
    foundation::auto_release_ptr<Entity> remove(Entity* entity);

    // Return a given entity.
    // Return nullptr if the requested entity does not exist.
    Entity* get_by_uid(const foundation::UniqueID id) const;
    Entity* get_by_name(const char* name) const;

    // Return mutable begin and end entity iterators.
    iterator begin();
    iterator end();

    // Return constant begin and end entity iterators.
    const_iterator begin() const;
    const_iterator end() const;

  private:
    struct Impl;
    Impl* impl;

    Entity* m_parent;
};

inline void swap(EntityMap& lhs, EntityMap& rhs)
{
    lhs.swap(rhs);
}


//
// A typed facade to EntityMap.
//

template <typename T>
class TypedEntityMap
  : public EntityMap
{
  public:
    // Value type.
    typedef T value_type;

    // Mutable iterator.
    class iterator
      : public EntityMap::iterator
    {
      public:
        // Types.
        typedef T value_type;
        typedef T& reference;
        typedef T* pointer;
        typedef std::ptrdiff_t difference_type;

        // Constructor.
        iterator(const EntityMap::iterator& rhs);

        // Dereference operators.
        value_type& operator*() const;
        value_type* operator->() const;
    };

    // Constant iterator.
    class const_iterator
      : public EntityMap::const_iterator
    {
      public:
        // Types.
        typedef T value_type;
        typedef const T& reference;
        typedef const T* pointer;

        // Constructor.
        const_iterator(const EntityMap::const_iterator& rhs);

        // Dereference operators.
        const value_type& operator*() const;
        const value_type* operator->() const;
    };

    // Constructor.
    explicit TypedEntityMap(Entity* parent = nullptr);

    // Insert an entity into the container and return its index.
    void insert(foundation::auto_release_ptr<T> entity);

    // Remove an entity from the container.
    // Note that if nothing is done with the return value, the entity will be deleted.
    foundation::auto_release_ptr<T> remove(const foundation::UniqueID id);
    foundation::auto_release_ptr<T> remove(T* entity);

    // Return a given entity.
    // Return nullptr if the requested entity does not exist.
    T* get_by_uid(const foundation::UniqueID id) const;
    T* get_by_name(const char* name) const;

    // Return mutable begin and end entity iterators.
    iterator begin();
    iterator end();

    // Return constant begin and end entity iterators.
    const_iterator begin() const;
    const_iterator end() const;
};


//
// EntityMap class implementation.
//

inline foundation::auto_release_ptr<Entity> EntityMap::remove(Entity* entity)
{
    return remove(entity->get_uid());
}


//
// TypedEntityMap::iterator class implementation.
//

template <typename T>
inline TypedEntityMap<T>::iterator::iterator(const EntityMap::iterator& rhs)
  : EntityMap::iterator(rhs)
{
}

template <typename T>
inline T& TypedEntityMap<T>::iterator::operator*() const
{
    return reinterpret_cast<T&>(EntityMap::iterator::operator*());
}

template <typename T>
inline T* TypedEntityMap<T>::iterator::operator->() const
{
    return reinterpret_cast<T*>(EntityMap::iterator::operator->());
}


//
// TypedEntityMap::const_iterator class implementation.
//

template <typename T>
inline TypedEntityMap<T>::const_iterator::const_iterator(const EntityMap::const_iterator& rhs)
  : EntityMap::const_iterator(rhs)
{
}

template <typename T>
inline const T& TypedEntityMap<T>::const_iterator::operator*() const
{
    return reinterpret_cast<const T&>(EntityMap::const_iterator::operator*());
}

template <typename T>
inline const T* TypedEntityMap<T>::const_iterator::operator->() const
{
    return reinterpret_cast<const T*>(EntityMap::const_iterator::operator->());
}


//
// TypedEntityMap class implementation.
//

template <typename T>
inline TypedEntityMap<T>::TypedEntityMap(Entity* parent)
  : EntityMap(parent)
{
}

template <typename T>
inline void TypedEntityMap<T>::insert(foundation::auto_release_ptr<T> entity)
{
    EntityMap::insert(
        foundation::auto_release_ptr<Entity>(entity.release()));
}

template <typename T>
inline foundation::auto_release_ptr<T> TypedEntityMap<T>::remove(const foundation::UniqueID id)
{
    return
        foundation::auto_release_ptr<T>(
            reinterpret_cast<T*>(EntityMap::remove(id).release()));
}

template <typename T>
inline foundation::auto_release_ptr<T> TypedEntityMap<T>::remove(T* entity)
{
    return
        foundation::auto_release_ptr<T>(
            reinterpret_cast<T*>(EntityMap::remove(entity).release()));
}

template <typename T>
inline T* TypedEntityMap<T>::get_by_uid(const foundation::UniqueID id) const
{
    return static_cast<T*>(EntityMap::get_by_uid(id));
}

template <typename T>
inline T* TypedEntityMap<T>::get_by_name(const char* name) const
{
    return static_cast<T*>(EntityMap::get_by_name(name));
}

template <typename T>
inline typename TypedEntityMap<T>::iterator TypedEntityMap<T>::begin()
{
    return EntityMap::begin();
}

template <typename T>
inline typename TypedEntityMap<T>::iterator TypedEntityMap<T>::end()
{
    return EntityMap::end();
}

template <typename T>
inline typename TypedEntityMap<T>::const_iterator TypedEntityMap<T>::begin() const
{
    return EntityMap::begin();
}

template <typename T>
inline typename TypedEntityMap<T>::const_iterator TypedEntityMap<T>::end() const
{
    return EntityMap::end();
}

}   // namespace renderer
