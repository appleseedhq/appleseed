
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
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/itembase.h"
#include "mainwindow/project/itemregistry.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QObject>
#include <QString>

// Standard headers.
#include <cassert>

// Forward declarations.
namespace appleseed { namespace studio { class EntityEditorContext; } }

namespace appleseed {
namespace studio {

// Work around a limitation in Qt: a template class cannot have slots.
class EntityItemBaseSlots
  : public ItemBase
{
    Q_OBJECT

  public:
    EntityItemBaseSlots(
        EntityEditorContext&        editor_context,
        const foundation::UniqueID  class_uid)
      : ItemBase(editor_context, class_uid)
    {
    }

    EntityItemBaseSlots(
        EntityEditorContext&        editor_context,
        const foundation::UniqueID  class_uid,
        const QString&              title)
      : ItemBase(editor_context, class_uid, title)
    {
    }

  protected slots:
    virtual void slot_edit_accepted(foundation::Dictionary values) {}
};

template <typename Entity>
class EntityItemBase
  : public EntityItemBaseSlots
{
  public:
    EntityItemBase(
        EntityEditorContext&        editor_context,
        Entity*                     entity);

    ~EntityItemBase() override;

    void update();

  protected:
    Entity*                 m_entity;
    foundation::UniqueID    m_entity_uid;
};


//
// EntityItemBase class implementation.
//

template <typename Entity>
EntityItemBase<Entity>::EntityItemBase(
    EntityEditorContext&    editor_context,
    Entity*                 entity)
  : EntityItemBaseSlots(editor_context, entity->get_class_uid())
  , m_entity(entity)
  , m_entity_uid(entity->get_uid())
{
    assert(m_entity != nullptr);

    update();

    m_editor_context.m_item_registry.insert(*m_entity, this);
}

template <typename Entity>
EntityItemBase<Entity>::~EntityItemBase()
{
    m_editor_context.m_item_registry.remove(m_entity_uid);
}

template <typename Entity>
void EntityItemBase<Entity>::update()
{
    set_title(m_entity->get_name());
}

}   // namespace studio
}   // namespace appleseed
