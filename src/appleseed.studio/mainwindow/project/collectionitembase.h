
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_COLLECTIONITEMBASE_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_COLLECTIONITEMBASE_H

// appleseed.studio headers.
#include "mainwindow/project/itembase.h"
#include "utility/treewidget.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QFont>
#include <QObject>

// Standard headers.
#include <cassert>
#include <map>

// Forward declarations.
class QString;

namespace appleseed {
namespace studio {

// Work around a limitation in Qt: a template class cannot have slots.
class CollectionItemBaseSlots
  : public ItemBase
{
    Q_OBJECT

  protected:
    explicit CollectionItemBaseSlots(const foundation::UniqueID class_uid)
      : ItemBase(class_uid) {}

    CollectionItemBaseSlots(const foundation::UniqueID class_uid, const QString& title)
      : ItemBase(class_uid, title) {}

  protected slots:
    virtual void slot_create() {}
    virtual void slot_create_accepted(foundation::Dictionary values) {}
};

template <typename Entity>
class CollectionItemBase
  : public CollectionItemBaseSlots
{
  public:
    explicit CollectionItemBase(const foundation::UniqueID class_uid);
    CollectionItemBase(const foundation::UniqueID class_uid, const QString& title);

    void add_item(Entity* entity);
    template <typename EntityContainer> void add_items(EntityContainer& items);
    void delete_item(const foundation::UniqueID entity_id);

    void insert_item(const foundation::UniqueID entity_id, ItemBase* item);
    void remove_item(const foundation::UniqueID entity_id);

    ItemBase* get_item(const foundation::UniqueID entity_id) const;

  protected:
    typedef std::map<foundation::UniqueID, ItemBase*> ItemMap;

    ItemMap m_items;

    void initialize();

    void add_item(const int index, Entity* entity);

    virtual ItemBase* create_item(Entity* entity);
};


//
// CollectionItemBase class implementation.
//

template <typename Entity>
CollectionItemBase<Entity>::CollectionItemBase(const foundation::UniqueID class_uid)
  : CollectionItemBaseSlots(class_uid)
{
    initialize();
}

template <typename Entity>
CollectionItemBase<Entity>::CollectionItemBase(const foundation::UniqueID class_uid, const QString& title)
  : CollectionItemBaseSlots(class_uid, title)
{
    initialize();
}

template <typename Entity>
void CollectionItemBase<Entity>::initialize()
{
    set_allow_edition(false);
    set_allow_deletion(false);

    QFont font;
    font.setBold(true);
    setFont(0, font);
}

template <typename Entity>
void CollectionItemBase<Entity>::add_item(Entity* entity)
{
    assert(entity);

    add_item(find_sorted_position(this, entity->get_name()), entity);
}

template <typename Entity>
template <typename EntityContainer>
void CollectionItemBase<Entity>::add_items(EntityContainer& entities)
{
    for (foundation::each<EntityContainer> i = entities; i; ++i)
        add_item(&*i);
}

template <typename Entity>
void CollectionItemBase<Entity>::add_item(const int index, Entity* entity)
{
    assert(entity);

    ItemBase* item = create_item(entity);

    insertChild(index, item);

    m_items[entity->get_uid()] = item;
}

template <typename Entity>
void CollectionItemBase<Entity>::delete_item(const foundation::UniqueID entity_id)
{
    const ItemMap::iterator it = m_items.find(entity_id);
    assert(it != m_items.end());

    delete it->second;

    m_items.erase(it);
}

template <typename Entity>
void CollectionItemBase<Entity>::insert_item(const foundation::UniqueID entity_id, ItemBase* item)
{
    m_items[entity_id] = item;
}

template <typename Entity>
void CollectionItemBase<Entity>::remove_item(const foundation::UniqueID entity_id)
{
    const ItemMap::iterator it = m_items.find(entity_id);
    assert(it != m_items.end());

    m_items.erase(it);
}

template <typename Entity>
ItemBase* CollectionItemBase<Entity>::get_item(const foundation::UniqueID entity_id) const
{
    const ItemMap::const_iterator it = m_items.find(entity_id);

    return it == m_items.end() ? 0 : it->second;
}

template <typename Entity>
ItemBase* CollectionItemBase<Entity>::create_item(Entity* entity)
{
    assert(entity);

    return new ItemBase(entity->get_class_uid(), entity->get_name());
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_COLLECTIONITEMBASE_H
