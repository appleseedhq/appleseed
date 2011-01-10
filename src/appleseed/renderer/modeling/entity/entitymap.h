
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_RENDERER_MODELING_ENTITY_ENTITYMAP_H
#define APPLESEED_RENDERER_MODELING_ENTITY_ENTITYMAP_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"

// Standard headers.
#include <map>

namespace renderer
{

//
// An entity container providing insertion of deletion of entities.
//

class RENDERERDLL EntityMap
  : public foundation::NonCopyable
{
  public:
    // Mutable iterator.
    class RENDERERDLL iterator
    {
      public:
        // Value type.
        typedef Entity value_type;

        // Constructors.
        iterator();
        iterator(const iterator& rhs);

        // Assignment operator.
        iterator& operator=(const iterator& rhs);

        // Equality and inequality tests.
        bool operator==(const iterator& rhs) const;
        bool operator!=(const iterator& rhs) const;

        // Preincrement and predecrement operators.
        iterator& operator++();
        iterator& operator--();

        // Dereference operators.
        value_type& operator*() const;
        value_type* operator->() const;

      private:
        friend class EntityMap;

        // Private implementation.
        struct Impl;
        Impl* impl;
    };

    // Constant iterator.
    class RENDERERDLL const_iterator
    {
      public:
        // Value type.
        typedef Entity value_type;

        // Constructors.
        const_iterator();
        const_iterator(const const_iterator& rhs);

        // Assignment operator.
        const_iterator& operator=(const const_iterator& rhs);

        // Equality and inequality tests.
        bool operator==(const const_iterator& rhs) const;
        bool operator!=(const const_iterator& rhs) const;

        // Preincrement and predecrement operators.
        const_iterator& operator++();
        const_iterator& operator--();

        // Dereference operators.
        const value_type& operator*() const;
        const value_type* operator->() const;

      private:
        friend class EntityMap;

        // Private implementation.
        struct Impl;
        Impl* impl;
    };

    // Constructor.
    EntityMap();

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
    void remove(const foundation::UniqueID id);

    // Return a given entity.
    // Return 0 if the requested entity does not exist.
    Entity* get(const foundation::UniqueID id) const;
    Entity* get(const char* name) const;

    // Return mutable begin and end entity iterators.
    iterator begin();
    iterator end();

    // Return constant begin and end entity iterators.
    const_iterator begin() const;
    const_iterator end() const;

  private:
    // Private implementation.
    struct Impl;
    Impl* impl;

    // No copy constructor or assignment operator.
    EntityMap(const EntityMap&);
    EntityMap& operator=(const EntityMap& rhs);
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
    // Mutable iterator.
    class iterator
      : public EntityMap::iterator
    {
      public:
        // Value type.
        typedef T value_type;

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
        // Value type.
        typedef T value_type;

        // Constructor.
        const_iterator(const EntityMap::const_iterator& rhs);

        // Dereference operators.
        const value_type& operator*() const;
        const value_type* operator->() const;
    };

    // Insert an entity into the container and return its index.
    void insert(foundation::auto_release_ptr<T> entity);

    // Return a given entity.
    // Return 0 if the requested entity does not exist.
    T* get(const foundation::UniqueID id) const;
    T* get(const char* name) const;

    // Return mutable begin and end entity iterators.
    iterator begin();
    iterator end();

    // Return constant begin and end entity iterators.
    const_iterator begin() const;
    const_iterator end() const;
};


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
inline void TypedEntityMap<T>::insert(foundation::auto_release_ptr<T> entity)
{
    EntityMap::insert(
        foundation::auto_release_ptr<Entity>(entity.release()));
}

template <typename T>
inline T* TypedEntityMap<T>::get(const foundation::UniqueID id) const
{
    return static_cast<T*>(EntityMap::get(id));
}

template <typename T>
inline T* TypedEntityMap<T>::get(const char* name) const
{
    return static_cast<T*>(EntityMap::get(name));
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

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_ENTITY_ENTITYMAP_H
