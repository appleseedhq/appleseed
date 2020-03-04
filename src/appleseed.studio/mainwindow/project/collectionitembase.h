
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

// appleseed.studio headers.
#include "mainwindow/project/itembase.h"

// appleseed.qtcommon headers.
#include "widgets/treewidget.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QFont>
#include <QObject>

// Standard headers.
#include <cassert>

// Forward declarations.
namespace appleseed { namespace studio { class EntityEditorContext; } }
class QString;

namespace appleseed {
namespace studio {

// Work around a limitation in Qt: a template class cannot have slots.
class CollectionItemBaseSlots
  : public ItemBase
{
    Q_OBJECT

  protected:
    CollectionItemBaseSlots(
        EntityEditorContext&        editor_context,
        const foundation::UniqueID  class_uid)
      : ItemBase(editor_context, class_uid)
    {
    }

    CollectionItemBaseSlots(
        EntityEditorContext&        editor_context,
        const foundation::UniqueID  class_uid,
        const QString&              title)
      : ItemBase(editor_context, class_uid, title)
    {
    }

  protected slots:
    virtual void slot_create() {}

    virtual void slot_create_applied(foundation::Dictionary values) {}
    virtual void slot_create_accepted(foundation::Dictionary values) {}
    virtual void slot_create_canceled(foundation::Dictionary values) {}
};

template <typename Entity>
class CollectionItemBase
  : public CollectionItemBaseSlots
{
  public:
    CollectionItemBase(
        EntityEditorContext&        editor_context,
        const foundation::UniqueID  class_uid);
    CollectionItemBase(
        EntityEditorContext&        editor_context,
        const foundation::UniqueID  class_uid,
        const QString&              title);

    ItemBase* add_item(Entity* entity);

    template <typename EntityContainer> void add_items(EntityContainer& items);

  protected:
    void initialize();

    ItemBase* add_item(const int index, Entity* entity);

    virtual ItemBase* create_item(Entity* entity);
};


//
// CollectionItemBase class implementation.
//

template <typename Entity>
CollectionItemBase<Entity>::CollectionItemBase(
    EntityEditorContext&        editor_context,
    const foundation::UniqueID  class_uid)
  : CollectionItemBaseSlots(editor_context, class_uid)
{
    initialize();
}

template <typename Entity>
CollectionItemBase<Entity>::CollectionItemBase(
    EntityEditorContext&        editor_context,
    const foundation::UniqueID  class_uid,
    const QString&              title)
  : CollectionItemBaseSlots(editor_context, class_uid, title)
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
ItemBase* CollectionItemBase<Entity>::add_item(Entity* entity)
{
    assert(entity);

    return add_item(qtcommon::find_sorted_position(this, entity->get_name()), entity);
}

template <typename Entity>
template <typename EntityContainer>
void CollectionItemBase<Entity>::add_items(EntityContainer& entities)
{
    for (auto& entity : entities)
        add_item(&entity);
}

template <typename Entity>
ItemBase* CollectionItemBase<Entity>::add_item(const int index, Entity* entity)
{
    assert(entity);

    ItemBase* item = create_item(entity);
    insertChild(index, item);

    return item;
}

template <typename Entity>
ItemBase* CollectionItemBase<Entity>::create_item(Entity* entity)
{
    assert(entity);

    return
        new ItemBase(
            m_editor_context,
            entity->get_class_uid(),
            entity->get_name());
}

}   // namespace studio
}   // namespace appleseed
