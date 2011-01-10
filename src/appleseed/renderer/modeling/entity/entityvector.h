
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

#ifndef APPLESEED_RENDERER_MODELING_ENTITY_ENTITYVECTOR_H
#define APPLESEED_RENDERER_MODELING_ENTITY_ENTITYVECTOR_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"

// Standard headers.
#include <map>
#include <vector>

namespace renderer
{

//
// An entity container providing random access to entities.
//

class RENDERERDLL EntityVector
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
        friend class EntityVector;

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
        friend class EntityVector;

        // Private implementation.
        struct Impl;
        Impl* impl;
    };

    // Constructor.
    EntityVector();

    // Destructor.
    ~EntityVector();

    // Swap the content of this container with another container.
    void swap(EntityVector& rhs);

    // Remove all entities from the container.
    void clear();

    // Return the number of entities in the container.
    size_t size() const;

    // Return true if the container is empty.
    bool empty() const;

    // Insert an entity into the container and return its index.
    size_t insert(foundation::auto_release_ptr<Entity> entity);

    // Remove an entity from the container. The entity is deleted.
    void remove(Entity* entity);

    // Return the index of a given entity in the container.
    // Return ~size_t(0) if the requested entity does not exist.
    size_t get_index(const char* name) const;

    // Return a given entity.
    Entity* get(const size_t index) const;

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
    EntityVector(const EntityVector&);
    EntityVector& operator=(const EntityVector& rhs);
};

inline void swap(EntityVector& lhs, EntityVector& rhs)
{
    lhs.swap(rhs);
}


//
// A typed facade to EntityVector.
//

template <typename T>
class TypedEntityVector
  : public EntityVector
{
  public:
    // Mutable iterator.
    class iterator
      : public EntityVector::iterator
    {
      public:
        // Value type.
        typedef T value_type;

        // Constructor.
        iterator(const EntityVector::iterator& rhs);

        // Dereference operators.
        value_type& operator*() const;
        value_type* operator->() const;
    };

    // Constant iterator.
    class const_iterator
      : public EntityVector::const_iterator
    {
      public:
        // Value type.
        typedef T value_type;

        // Constructor.
        const_iterator(const EntityVector::const_iterator& rhs);

        // Dereference operators.
        const value_type& operator*() const;
        const value_type* operator->() const;
    };

    // Insert an entity into the container and return its index.
    size_t insert(foundation::auto_release_ptr<T> entity);

    // Return a given entity.
    T* get(const size_t index) const;

    // Return mutable begin and end entity iterators.
    iterator begin();
    iterator end();

    // Return constant begin and end entity iterators.
    const_iterator begin() const;
    const_iterator end() const;
};


//
// TypedEntityVector::iterator class implementation.
//

template <typename T>
inline TypedEntityVector<T>::iterator::iterator(const EntityVector::iterator& rhs)
  : EntityVector::iterator(rhs)
{
}

template <typename T>
inline T& TypedEntityVector<T>::iterator::operator*() const
{
    return reinterpret_cast<T&>(EntityVector::iterator::operator*());
}

template <typename T>
inline T* TypedEntityVector<T>::iterator::operator->() const
{
    return reinterpret_cast<T*>(EntityVector::iterator::operator->());
}


//
// TypedEntityVector::const_iterator class implementation.
//

template <typename T>
inline TypedEntityVector<T>::const_iterator::const_iterator(const EntityVector::const_iterator& rhs)
  : EntityVector::const_iterator(rhs)
{
}

template <typename T>
inline const T& TypedEntityVector<T>::const_iterator::operator*() const
{
    return reinterpret_cast<const T&>(EntityVector::const_iterator::operator*());
}

template <typename T>
inline const T* TypedEntityVector<T>::const_iterator::operator->() const
{
    return reinterpret_cast<const T*>(EntityVector::const_iterator::operator->());
}


//
// TypedEntityVector class implementation.
//

template <typename T>
inline size_t TypedEntityVector<T>::insert(foundation::auto_release_ptr<T> entity)
{
    return EntityVector::insert(
        foundation::auto_release_ptr<Entity>(entity.release()));
}

template <typename T>
inline T* TypedEntityVector<T>::get(const size_t index) const
{
    return reinterpret_cast<T*>(EntityVector::get(index));
}

template <typename T>
inline typename TypedEntityVector<T>::iterator TypedEntityVector<T>::begin()
{
    return EntityVector::begin();
}

template <typename T>
inline typename TypedEntityVector<T>::iterator TypedEntityVector<T>::end()
{
    return EntityVector::end();
}

template <typename T>
inline typename TypedEntityVector<T>::const_iterator TypedEntityVector<T>::begin() const
{
    return EntityVector::begin();
}

template <typename T>
inline typename TypedEntityVector<T>::const_iterator TypedEntityVector<T>::end() const
{
    return EntityVector::end();
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_ENTITY_ENTITYVECTOR_H
