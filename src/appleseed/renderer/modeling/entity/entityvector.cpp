
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
#include "entityvector.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <map>
#include <vector>

using namespace foundation;

namespace renderer
{

struct EntityVector::Impl
{
    typedef std::vector<Entity*> Storage;
    typedef std::map<UniqueID, size_t> IDIndex;
    typedef std::map<std::string, size_t> NameIndex;

    Storage     m_storage;
    IDIndex     m_id_index;
    NameIndex   m_name_index;
};


//
// EntityVector::iterator class implementation.
//

struct EntityVector::iterator::Impl
{
    EntityVector::Impl::Storage::iterator m_it;
};

EntityVector::iterator::iterator()
  : impl(new Impl())
{
}

EntityVector::iterator::iterator(const iterator& rhs)
  : impl(new Impl(*rhs.impl))
{
}

EntityVector::iterator::~iterator()
{
    delete impl;
}

EntityVector::iterator& EntityVector::iterator::operator=(const iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

bool EntityVector::iterator::operator==(const iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}

bool EntityVector::iterator::operator!=(const iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

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

EntityVector::iterator
EntityVector::iterator::operator++(int)
{
    const iterator current(*this);
    ++impl->m_it;
    return current;
}

EntityVector::iterator
EntityVector::iterator::operator--(int)
{
    const iterator current(*this);
    --impl->m_it;
    return current;
}

EntityVector::iterator::reference
EntityVector::iterator::operator*() const
{
    return **impl->m_it;
}

EntityVector::iterator::pointer
EntityVector::iterator::operator->() const
{
    return *impl->m_it;
}


//
// EntityVector::const_iterator class implementation.
//

struct EntityVector::const_iterator::Impl
{
    EntityVector::Impl::Storage::const_iterator m_it;
};

EntityVector::const_iterator::const_iterator()
  : impl(new Impl())
{
}

EntityVector::const_iterator::const_iterator(const const_iterator& rhs)
  : impl(new Impl(*rhs.impl))
{
}

EntityVector::const_iterator::~const_iterator()
{
    delete impl;
}

EntityVector::const_iterator& EntityVector::const_iterator::operator=(const const_iterator& rhs)
{
    *impl = *rhs.impl;
    return *this;
}

bool EntityVector::const_iterator::operator==(const const_iterator& rhs) const
{
    return impl->m_it == rhs.impl->m_it;
}

bool EntityVector::const_iterator::operator!=(const const_iterator& rhs) const
{
    return impl->m_it != rhs.impl->m_it;
}

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

EntityVector::const_iterator
EntityVector::const_iterator::operator++(int)
{
    const const_iterator current(*this);
    ++impl->m_it;
    return current;
}

EntityVector::const_iterator
EntityVector::const_iterator::operator--(int)
{
    const const_iterator current(*this);
    --impl->m_it;
    return current;
}

EntityVector::const_iterator::reference
EntityVector::const_iterator::operator*() const
{
    return **impl->m_it;
}

EntityVector::const_iterator::pointer
EntityVector::const_iterator::operator->() const
{
    return *impl->m_it;
}


//
// EntityVector class implementation.
//

EntityVector::EntityVector(Entity* parent)
  : impl(new Impl())
  , m_parent(parent)
{
}

EntityVector::~EntityVector()
{
    clear();

    delete impl;
}

void EntityVector::swap(EntityVector& rhs)
{
    impl->m_storage.swap(rhs.impl->m_storage);
    impl->m_id_index.swap(rhs.impl->m_id_index);
    impl->m_name_index.swap(rhs.impl->m_name_index);

    for (const_each<Impl::Storage> i = impl->m_storage; i; ++i)
        (*i)->set_parent(m_parent);

    for (const_each<Impl::Storage> i = rhs.impl->m_storage; i; ++i)
        (*i)->set_parent(rhs.m_parent);
}

void EntityVector::clear()
{
    for (const_each<Impl::Storage> i = impl->m_storage; i; ++i)
        (*i)->release();

    impl->m_storage.clear();
    impl->m_id_index.clear();
    impl->m_name_index.clear();
}

size_t EntityVector::size() const
{
    return impl->m_storage.size();
}

bool EntityVector::empty() const
{
    return impl->m_storage.empty();
}

size_t EntityVector::insert(auto_release_ptr<Entity> entity)
{
    // Retrieve the entity.
    Entity* entity_ptr = entity.release();
    assert(entity_ptr);

    // The entity shouldn't already be in the container.
    assert(impl->m_id_index.find(entity_ptr->get_uid()) == impl->m_id_index.end());
    assert(impl->m_name_index.find(entity_ptr->get_name()) == impl->m_name_index.end());

    // Insert the entity into the container.
    const size_t entity_index = impl->m_storage.size();
    impl->m_storage.push_back(entity_ptr);
    impl->m_id_index[entity_ptr->get_uid()] = entity_index;
    impl->m_name_index[entity_ptr->get_name()] = entity_index;

    // Link the entity to its parent.
    entity_ptr->set_parent(m_parent);

    return entity_index;
}

auto_release_ptr<Entity> EntityVector::remove(Entity* entity)
{
    assert(entity);

    // Find the entity to remove in the vector.
    const Impl::IDIndex::iterator id_it = impl->m_id_index.find(entity->get_uid());
    const Impl::NameIndex::iterator name_it = impl->m_name_index.find(entity->get_name());
    assert(id_it != impl->m_id_index.end());
    assert(name_it != impl->m_name_index.end());
    assert(id_it->second == name_it->second);

    // Get the index of the entity.
    const size_t index = id_it->second;
    assert(impl->m_storage[index] == entity);

    // Replace the entity to remove by the last entity of the vector.
    const size_t last_index = impl->m_storage.size() - 1;
    if (index < last_index)
    {
        Entity* last_entity = impl->m_storage[last_index];
        impl->m_storage[index] = last_entity;
        impl->m_id_index[last_entity->get_uid()] = index;
        impl->m_name_index[last_entity->get_name()] = index;
    }

    // Remove the entity from the vector.
    impl->m_storage.pop_back();
    impl->m_id_index.erase(id_it);
    impl->m_name_index.erase(name_it);

    return auto_release_ptr<Entity>(entity);
}

size_t EntityVector::get_index(const UniqueID id) const
{
    const Impl::IDIndex::iterator it = impl->m_id_index.find(id);
    return it == impl->m_id_index.end() ? ~size_t(0) : it->second;
}

size_t EntityVector::get_index(const char* name) const
{
    assert(name);
    const Impl::NameIndex::iterator it = impl->m_name_index.find(name);
    return it == impl->m_name_index.end() ? ~size_t(0) : it->second;
}

Entity* EntityVector::get_by_index(const size_t index) const
{
    assert(index < impl->m_storage.size());
    return impl->m_storage[index];
}

Entity* EntityVector::get_by_uid(const UniqueID id) const
{
    const size_t index = get_index(id);
    return index == ~size_t(0) ? nullptr : get_by_index(index);
}

Entity* EntityVector::get_by_name(const char* name) const
{
    const size_t index = get_index(name);
    return index == ~size_t(0) ? nullptr : get_by_index(index);
}

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
