
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

// Interface header.
#include "entitymap.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// EntityMap class implementation.
//

struct EntityMap::Impl
{
    typedef map<UniqueID, Entity*> Storage;
    typedef map<string, Entity*> Index;

    Storage m_storage;
    Index   m_index;
};

struct EntityMap::iterator::Impl
{
    EntityMap::Impl::Storage::iterator m_it;
};

// Constructors.
EntityMap::iterator::iterator()
  : impl(new Impl())
{
}
EntityMap::iterator::iterator(const iterator& rhs)
  : impl(new Impl(*rhs.impl))
{
}

// Assignment operator.
EntityMap::iterator&
EntityMap::iterator::operator=(const iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

// Equality and inequality tests.
bool EntityMap::iterator::operator==(const iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}
bool EntityMap::iterator::operator!=(const iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

// Preincrement and predecrement operators.
EntityMap::iterator&
EntityMap::iterator::operator++()
{
    ++impl->m_it;
    return *this;
}
EntityMap::iterator&
EntityMap::iterator::operator--()
{
    --impl->m_it;
    return *this;
}

// Dereference operators.
EntityMap::iterator::value_type&
EntityMap::iterator::operator*() const
{
    return *impl->m_it->second;
}
EntityMap::iterator::value_type*
EntityMap::iterator::operator->() const
{
    return impl->m_it->second;
}

struct EntityMap::const_iterator::Impl
{
    EntityMap::Impl::Storage::const_iterator m_it;
};

// Constructors.
EntityMap::const_iterator::const_iterator()
  : impl(new Impl())
{
}
EntityMap::const_iterator::const_iterator(const const_iterator& rhs)
  : impl(new Impl(*rhs.impl))
{
}

// Assignment operator.
EntityMap::const_iterator&
EntityMap::const_iterator::operator=(const const_iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

// Equality and inequality tests.
bool EntityMap::const_iterator::operator==(const const_iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}
bool EntityMap::const_iterator::operator!=(const const_iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

// Preincrement and predecrement operators.
EntityMap::const_iterator&
EntityMap::const_iterator::operator++()
{
    ++impl->m_it;
    return *this;
}
EntityMap::const_iterator&
EntityMap::const_iterator::operator--()
{
    --impl->m_it;
    return *this;
}

// Dereference operators.
const EntityMap::const_iterator::value_type&
EntityMap::const_iterator::operator*() const
{
    return *impl->m_it->second;
}
const EntityMap::const_iterator::value_type*
EntityMap::const_iterator::operator->() const
{
    return impl->m_it->second;
}

// Constructor.
EntityMap::EntityMap()
  : impl(new Impl())
{
}

// Destructor.
EntityMap::~EntityMap()
{
    // Delete all entities.
    clear();

    // Delete private implementation.
    delete impl;
}

// Swap the content of this container with another container.
void EntityMap::swap(EntityMap& rhs)
{
    impl->m_storage.swap(rhs.impl->m_storage);
    impl->m_index.swap(rhs.impl->m_index);
}

// Remove all entities from the container.
void EntityMap::clear()
{
    // Delete all entities.
    for (const_each<EntityMap::Impl::Storage> i = impl->m_storage; i; ++i)
        i->second->release();
    impl->m_storage.clear();
    impl->m_index.clear();
}

// Return the number of entities in the container.
size_t EntityMap::size() const
{
    return impl->m_storage.size();
}

// Return true if the container is empty.
bool EntityMap::empty() const
{
    return impl->m_storage.empty();
}

// Insert an entity into the container.
void EntityMap::insert(auto_release_ptr<Entity> entity)
{
    // Retrieve the entity.
    Entity* entity_ptr = entity.release();
    assert(entity_ptr);

    // Insert the entity into the container.
    impl->m_storage[entity_ptr->get_uid()] = entity_ptr;
    impl->m_index[entity_ptr->get_name()] = entity_ptr;
}

// Remove an entity from the container.
void EntityMap::remove(const UniqueID id)
{
    // Locate the entity.
    const EntityMap::Impl::Storage::iterator it = impl->m_storage.find(id);
    if (it == impl->m_storage.end())
        return;

    // Remove the entity from the container.
    impl->m_index.erase(it->second->get_name());
    impl->m_storage.erase(it);

    // Delete the entity.
    delete it->second;
}

// Access an entity.
Entity* EntityMap::get(const UniqueID id) const
{
    const EntityMap::Impl::Storage::iterator it = impl->m_storage.find(id);
    return it == impl->m_storage.end() ? 0 : it->second;
}
Entity* EntityMap::get(const char* name) const
{
    assert(name);
    const EntityMap::Impl::Index::iterator it = impl->m_index.find(name);
    return it == impl->m_index.end() ? 0 : it->second;
}

// Return mutable begin and end entity iterators.
EntityMap::iterator EntityMap::begin()
{
    iterator it;
    it.impl->m_it = impl->m_storage.begin();
    return it;
}
EntityMap::iterator EntityMap::end()
{
    iterator it;
    it.impl->m_it = impl->m_storage.end();
    return it;
}

// Return constant begin and end entity iterators.
EntityMap::const_iterator EntityMap::begin() const
{
    const_iterator it;
    it.impl->m_it = impl->m_storage.begin();
    return it;
}
EntityMap::const_iterator EntityMap::end() const
{
    const_iterator it;
    it.impl->m_it = impl->m_storage.end();
    return it;
}

}   // namespace renderer
