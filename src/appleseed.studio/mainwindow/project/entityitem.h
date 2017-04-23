
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entityactions.h"
#include "mainwindow/project/entitycreatorbase.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/entityitembase.h"
#include "mainwindow/project/ientityvalueprovider.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/rendering/renderingmanager.h"
#include "utility/miscellaneous.h"
#include "utility/treewidget.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QList>
#include <QObject>
#include <QWidget>

// Standard headers.
#include <memory>
#include <string>

// Forward declarations.
namespace appleseed     { namespace studio { class EntityEditorContext; } }
namespace appleseed     { namespace studio { class ItemBase; } }
namespace foundation    { class Dictionary; }

namespace appleseed {
namespace studio {

template <typename Entity, typename ParentEntity, typename CollectionItem>
class EntityItem
  : public EntityItemBase<Entity>
  , public IEntityValueProvider
  , private EntityCreatorBase
{
  public:
    EntityItem(
        EntityEditorContext&    editor_context,
        Entity*                 entity,
        ParentEntity&           parent,
        CollectionItem*         collection_item);

    void set_fixed_position(const bool fixed);
    bool is_fixed_position() const;

  protected:
    typedef EntityItemBase<Entity> Base;

    ParentEntity&               m_parent;
    CollectionItem*             m_collection_item;

    virtual void slot_edit_accepted(foundation::Dictionary values) APPLESEED_OVERRIDE;
    void edit(const foundation::Dictionary& values);

    virtual void delete_multiple(const QList<ItemBase*>& items) APPLESEED_OVERRIDE;
    void do_delete();

  private:
    friend class EntityCreatorBase;
    friend class EntityEditionAction<EntityItem>;
    friend class EntityInstantiationAction<EntityItem>;
    friend class EntityDeletionAction<EntityItem>;

    foundation::UniqueID        m_entity_uid;
    bool                        m_fixed_position;
};


//
// EntityItem class implementation.
//

template <typename Entity, typename ParentEntity, typename CollectionItem>
EntityItem<Entity, ParentEntity, CollectionItem>::EntityItem(
    EntityEditorContext&        editor_context,
    Entity*                     entity,
    ParentEntity&               parent,
    CollectionItem*             collection_item)
  : Base(editor_context, entity)
  , m_parent(parent)
  , m_collection_item(collection_item)
  , m_entity_uid(entity->get_uid())
  , m_fixed_position(false)
{
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void EntityItem<Entity, ParentEntity, CollectionItem>::set_fixed_position(const bool fixed)
{
    m_fixed_position = fixed;
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
bool EntityItem<Entity, ParentEntity, CollectionItem>::is_fixed_position() const
{
    return m_fixed_position;
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void EntityItem<Entity, ParentEntity, CollectionItem>::slot_edit_accepted(foundation::Dictionary values)
{
    if (Base::m_editor_context.m_rendering_manager.is_rendering())
    {
        Base::m_editor_context.m_rendering_manager.schedule(
            std::auto_ptr<RenderingManager::IScheduledAction>(
                new EntityEditionAction<EntityItem>(this, values)));

        Base::m_editor_context.m_rendering_manager.reinitialize_rendering();
    }
    else
    {
        catch_entity_creation_errors(
            &EntityItem::edit,
            values,
            renderer::EntityTraits<Entity>::get_human_readable_entity_type_name());
    }
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void EntityItem<Entity, ParentEntity, CollectionItem>::edit(const foundation::Dictionary& values)
{
    Base::m_editor_context.m_item_registry.remove(m_entity_uid);

    const std::string old_entity_name = Base::m_entity->get_name();

    Base::m_entity =
        Base::m_editor_context.m_project_builder.edit_entity(
            Base::m_entity,
            m_parent,
            values);

    const std::string new_entity_name = Base::m_entity->get_name();

    m_entity_uid = Base::m_entity->get_uid();
    Base::m_editor_context.m_item_registry.insert(m_entity_uid, this);

    Base::update();

    // Move the item to its sorted position.
    if (!m_fixed_position && old_entity_name != new_entity_name)
        move_to_sorted_position(this);
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void EntityItem<Entity, ParentEntity, CollectionItem>::delete_multiple(const QList<ItemBase*>& items)
{
    Base::m_editor_context.m_rendering_manager.schedule_or_execute(
        std::auto_ptr<RenderingManager::IScheduledAction>(
            new EntityDeletionAction<EntityItem>(
                qlist_static_cast<EntityItem*>(items))));
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void EntityItem<Entity, ParentEntity, CollectionItem>::do_delete()
{
    if (Base::allows_deletion())
    {
        Base::m_editor_context.m_project_builder.remove_entity(Base::m_entity, m_parent);

        delete this;
    }
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYITEM_H
