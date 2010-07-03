
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
#include "entityvector.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// EntityVector class implementation.
//

struct EntityVector::Impl
{
    typedef vector<Entity*> Storage;
    typedef map<string, size_t> Index;

    Storage m_storage;
    Index   m_index;
};

struct EntityVector::iterator::Impl
{
    EntityVector::Impl::Storage::iterator m_it;
};

// Constructors.
EntityVector::iterator::iterator()
  : impl(new Impl())
{
}
EntityVector::iterator::iterator(const iterator& rhs)
  : impl(new Impl(*rhs.impl))
{
}

// Assignment operator.
EntityVector::iterator&
EntityVector::iterator::operator=(const iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

// Equality and inequality tests.
bool EntityVector::iterator::operator==(const iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}
bool EntityVector::iterator::operator!=(const iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

// Preincrement and predecrement operators.
EntityVector::iterator&
EntityVector::iterator::operator++()
{
    ++impl->m_it;
    return *this;
}
EntityVector::iterator&
EntityVector::iterator::operator--()
{
    --impl->m_it;
    return *this;
}

// Dereference operators.
EntityVector::iterator::value_type&
EntityVector::iterator::operator*() const
{
    return **impl->m_it;
}
EntityVector::iterator::value_type*
EntityVector::iterator::operator->() const
{
    return *impl->m_it;
}

struct EntityVector::const_iterator::Impl
{
    EntityVector::Impl::Storage::const_iterator m_it;
};

// Constructors.
EntityVector::const_iterator::const_iterator()
  : impl(new Impl())
{
}
EntityVector::const_iterator::const_iterator(const const_iterator& rhs)
  : impl(new Impl(*rhs.impl))
{
}

// Assignment operator.
EntityVector::const_iterator&
EntityVector::const_iterator::operator=(const const_iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

// Equality and inequality tests.
bool EntityVector::const_iterator::operator==(const const_iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}
bool EntityVector::const_iterator::operator!=(const const_iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

// Preincrement and predecrement operators.
EntityVector::const_iterator&
EntityVector::const_iterator::operator++()
{
    ++impl->m_it;
    return *this;
}
EntityVector::const_iterator&
EntityVector::const_iterator::operator--()
{
    --impl->m_it;
    return *this;
}

// Dereference operators.
const EntityVector::const_iterator::value_type&
EntityVector::const_iterator::operator*() const
{
    return **impl->m_it;
}
const EntityVector::const_iterator::value_type*
EntityVector::const_iterator::operator->() const
{
    return *impl->m_it;
}

// Constructor.
EntityVector::EntityVector()
  : impl(new Impl())
{
}

// Destructor.
EntityVector::~EntityVector()
{
    // Delete all entities.
    clear();

    // Delete private implementation.
    delete impl;
}

// Swap the content of this container with another container.
void EntityVector::swap(EntityVector& rhs)
{
    impl->m_storage.swap(rhs.impl->m_storage);
    impl->m_index.swap(rhs.impl->m_index);
}

// Remove all entities from the container.
void EntityVector::clear()
{
    // Delete all entities.
    for (const_each<EntityVector::Impl::Storage> i = impl->m_storage; i; ++i)
        (*i)->release();
    impl->m_storage.clear();
    impl->m_index.clear();
}

// Return the number of entities in the container.
size_t EntityVector::size() const
{
    return impl->m_storage.size();
}

// Return true if the container is empty.
bool EntityVector::empty() const
{
    return impl->m_storage.empty();
}

// Insert an entity into the container and return its index.
size_t EntityVector::insert(auto_release_ptr<Entity> entity)
{
    // Retrieve the entity.
    Entity* entity_ptr = entity.release();
    assert(entity_ptr);

    // Insert the entity into the container.
    const size_t entity_index = impl->m_storage.size();
    impl->m_storage.push_back(entity_ptr);
    impl->m_index[entity_ptr->get_name()] = entity_index;

    return entity_index;
}

// Return the index of a given entity in the container.
size_t EntityVector::get_index(const char* name) const
{
    assert(name);
    const EntityVector::Impl::Index::iterator it = impl->m_index.find(name);
    return it == impl->m_index.end() ? ~size_t(0) : it->second;
}

// Return a given entity.
Entity* EntityVector::get(const size_t index) const
{
    assert(index < impl->m_storage.size());
    return impl->m_storage[index];
}

// Return mutable begin and end entity iterators.
EntityVector::iterator EntityVector::begin()
{
    iterator it;
    it.impl->m_it = impl->m_storage.begin();
    return it;
}
EntityVector::iterator EntityVector::end()
{
    iterator it;
    it.impl->m_it = impl->m_storage.end();
    return it;
}

// Return constant begin and end entity iterators.
EntityVector::const_iterator EntityVector::begin() const
{
    const_iterator it;
    it.impl->m_it = impl->m_storage.begin();
    return it;
}
EntityVector::const_iterator EntityVector::end() const
{
    const_iterator it;
    it.impl->m_it = impl->m_storage.end();
    return it;
}

}   // namespace renderer
