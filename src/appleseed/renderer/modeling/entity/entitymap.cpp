
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

// Interface header.
#include "entitymap.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <map>

using namespace foundation;

namespace renderer
{

struct EntityMap::Impl
{
    typedef std::map<UniqueID, Entity*> Storage;
    typedef std::map<std::string, Entity*> Index;

    Storage m_storage;
    Index   m_index;
};


//
// EntityMap::iterator class implementation.
//

struct EntityMap::iterator::Impl
{
    EntityMap::Impl::Storage::iterator m_it;
};

EntityMap::iterator::iterator()
  : impl(new Impl())
{
}

EntityMap::iterator::iterator(const iterator& rhs)
  : impl(new Impl(*rhs.impl))
{
}

EntityMap::iterator::~iterator()
{
    delete impl;
}

EntityMap::iterator& EntityMap::iterator::operator=(const iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

bool EntityMap::iterator::operator==(const iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}

bool EntityMap::iterator::operator!=(const iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

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

EntityMap::iterator
EntityMap::iterator::operator++(int)
{
    const iterator current(*this);
    ++impl->m_it;
    return current;
}

EntityMap::iterator
EntityMap::iterator::operator--(int)
{
    const iterator current(*this);
    --impl->m_it;
    return current;
}

EntityMap::iterator::reference
EntityMap::iterator::operator*() const
{
    return *impl->m_it->second;
}

EntityMap::iterator::pointer
EntityMap::iterator::operator->() const
{
    return impl->m_it->second;
}


//
// EntityMap::const_iterator class implementation.
//

struct EntityMap::const_iterator::Impl
{
    EntityMap::Impl::Storage::const_iterator m_it;
};

EntityMap::const_iterator::const_iterator()
  : impl(new Impl())
{
}

EntityMap::const_iterator::const_iterator(const const_iterator& rhs)
  : impl(new Impl(*rhs.impl))
{
}

EntityMap::const_iterator::~const_iterator()
{
    delete impl;
}

EntityMap::const_iterator& EntityMap::const_iterator::operator=(const const_iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

bool EntityMap::const_iterator::operator==(const const_iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}

bool EntityMap::const_iterator::operator!=(const const_iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

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

EntityMap::const_iterator
EntityMap::const_iterator::operator++(int)
{
    const const_iterator current(*this);
    ++impl->m_it;
    return current;
}

EntityMap::const_iterator
EntityMap::const_iterator::operator--(int)
{
    const const_iterator current(*this);
    --impl->m_it;
    return current;
}

EntityMap::const_iterator::reference
EntityMap::const_iterator::operator*() const
{
    return *impl->m_it->second;
}

EntityMap::const_iterator::pointer
EntityMap::const_iterator::operator->() const
{
    return impl->m_it->second;
}


//
// EntityMap class implementation.
//

EntityMap::EntityMap(Entity* parent)
  : impl(new Impl())
  , m_parent(parent)
{
}

EntityMap::~EntityMap()
{
    clear();

    delete impl;
}

void EntityMap::swap(EntityMap& rhs)
{
    impl->m_storage.swap(rhs.impl->m_storage);
    impl->m_index.swap(rhs.impl->m_index);

    for (const_each<Impl::Storage> i = impl->m_storage; i; ++i)
        i->second->set_parent(m_parent);

    for (const_each<Impl::Storage> i = rhs.impl->m_storage; i; ++i)
        i->second->set_parent(rhs.m_parent);
}

void EntityMap::clear()
{
    for (const_each<Impl::Storage> i = impl->m_storage; i; ++i)
        i->second->release();

    impl->m_storage.clear();
    impl->m_index.clear();
}

size_t EntityMap::size() const
{
    return impl->m_storage.size();
}

bool EntityMap::empty() const
{
    return impl->m_storage.empty();
}

void EntityMap::insert(auto_release_ptr<Entity> entity)
{
    // Retrieve the entity.
    Entity* entity_ptr = entity.release();
    assert(entity_ptr);

    // The entity shouldn't already be in the container.
    assert(impl->m_storage.find(entity_ptr->get_uid()) == impl->m_storage.end());
    assert(impl->m_index.find(entity_ptr->get_name()) == impl->m_index.end());

    // Insert the entity into the container.
    impl->m_storage[entity_ptr->get_uid()] = entity_ptr;
    impl->m_index[entity_ptr->get_name()] = entity_ptr;

    // Link the entity to its parent.
    entity_ptr->set_parent(m_parent);
}

auto_release_ptr<Entity> EntityMap::remove(const UniqueID id)
{
    // Locate the entity.
    const EntityMap::Impl::Storage::iterator it = impl->m_storage.find(id);
    assert(it != impl->m_storage.end());

    // Retrieve the entity.
    Entity* entity_ptr = it->second;
    assert(entity_ptr);

    // Remove the entity from the container.
    impl->m_index.erase(entity_ptr->get_name());
    impl->m_storage.erase(it);

    return auto_release_ptr<Entity>(entity_ptr);
}

Entity* EntityMap::get_by_uid(const UniqueID id) const
{
    const Impl::Storage::iterator it = impl->m_storage.find(id);
    return it == impl->m_storage.end() ? nullptr : it->second;
}

Entity* EntityMap::get_by_name(const char* name) const
{
    assert(name);
    const Impl::Index::iterator it = impl->m_index.find(name);
    return it == impl->m_index.end() ? nullptr : it->second;
}

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
