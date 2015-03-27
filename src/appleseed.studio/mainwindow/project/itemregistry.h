
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ITEMREGISTRY_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ITEMREGISTRY_H

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QMutex>

// Standard headers.
#include <map>

// Forward declarations.
namespace appleseed { namespace studio { class ItemBase; } }
namespace renderer  { class Entity; }

namespace appleseed {
namespace studio {

//
// The item registry maintains a mapping between entities (identified by
// their unique ID) and their corresponding project explorer item.
// All methods of this class are thread-safe.
//

class ItemRegistry
{
  public:
    void insert(
        const foundation::UniqueID  uid,
        ItemBase*                   item);
    void insert(
        const renderer::Entity&     entity,
        ItemBase*                   item);

    void remove(const foundation::UniqueID uid);
    void remove(const renderer::Entity& entity);

    ItemBase* get_item(const foundation::UniqueID uid) const;
    ItemBase* get_item(const renderer::Entity& entity) const;

    template <typename ItemType>
    ItemType* get_item(const foundation::UniqueID uid) const;
    template <typename ItemType>
    ItemType* get_item(const renderer::Entity& entity) const;

  private:
    typedef std::map<foundation::UniqueID, ItemBase*> RegistryType;

    mutable QMutex  m_mutex;
    RegistryType    m_registry;
};


//
// ItemRegistry class implementation.
//

template <typename ItemType>
inline ItemType* ItemRegistry::get_item(const foundation::UniqueID uid) const
{
    return static_cast<ItemType*>(get_item(uid));
}

template <typename ItemType>
inline ItemType* ItemRegistry::get_item(const renderer::Entity& entity) const
{
    return static_cast<ItemType*>(get_item(entity));
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ITEMREGISTRY_H
