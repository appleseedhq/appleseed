
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYITEM_H

// appleseed.studio headers.
#include "mainwindow/project/entitycreatorbase.h"
#include "mainwindow/project/entitydelayedactions.h"
#include "mainwindow/project/entityitembase.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/rendering/renderingmanager.h"
#include "utility/treewidget.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QObject>
#include <QWidget>

// Standard headers.
#include <memory>
#include <string>

// Forward declarations.
namespace foundation { class Dictionary; }

namespace appleseed {
namespace studio {

template <typename Entity, typename ParentEntity, typename CollectionItem>
class EntityItem
  : public EntityItemBase<Entity>
  , private EntityCreatorBase
{
  public:
    EntityItem(
        Entity*             entity,
        ParentEntity&       parent,
        CollectionItem*     collection_item,
        ProjectBuilder&     project_builder);

    void set_fixed_position(const bool fixed);
    bool is_fixed_position() const;

  protected:
    typedef EntityItemBase<Entity> EntityItemBaseType;

    ParentEntity&           m_parent;
    CollectionItem*         m_collection_item;
    ProjectBuilder&         m_project_builder;

    virtual void slot_edit_accepted(foundation::Dictionary values) OVERRIDE;

    void schedule_edit(const foundation::Dictionary& values);
    void edit(const foundation::Dictionary& values);

    virtual void slot_delete() OVERRIDE;

    void schedule_delete();
    void do_delete();

  private:
    friend class EntityCreatorBase;
    friend class EntityEditionDelayedAction<EntityItem>;
    friend class EntityDeletionDelayedAction<EntityItem>;

    foundation::UniqueID    m_entity_uid;
    bool                    m_fixed_position;
};


//
// EntityItem class implementation.
//

template <typename Entity, typename ParentEntity, typename CollectionItem>
EntityItem<Entity, ParentEntity, CollectionItem>::EntityItem(
    Entity*                 entity,
    ParentEntity&           parent,
    CollectionItem*         collection_item,
    ProjectBuilder&         project_builder)
  : EntityItemBaseType(entity)
  , m_parent(parent)
  , m_collection_item(collection_item)
  , m_project_builder(project_builder)
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
    catch_entity_creation_errors(
        m_project_builder.get_rendering_manager().is_rendering()
            ? &EntityItem::schedule_edit
            : &EntityItem::edit,
        values,
        renderer::EntityTraits<Entity>::get_human_readable_entity_type_name());
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void EntityItem<Entity, ParentEntity, CollectionItem>::schedule_edit(const foundation::Dictionary& values)
{
    m_project_builder.get_rendering_manager().push_delayed_action(
        std::auto_ptr<RenderingManager::IDelayedAction>(
            new EntityEditionDelayedAction<EntityItem>(this, values)));

    m_project_builder.get_rendering_manager().reinitialize_rendering();
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void EntityItem<Entity, ParentEntity, CollectionItem>::edit(const foundation::Dictionary& values)
{
    m_project_builder.get_item_registry().remove(m_entity_uid);

    const std::string old_entity_name = EntityItemBaseType::m_entity->get_name();

    EntityItemBaseType::m_entity =
        m_project_builder.edit_entity(
            EntityItemBaseType::m_entity,
            m_parent,
            values);

    const std::string new_entity_name = EntityItemBaseType::m_entity->get_name();

    m_entity_uid = EntityItemBaseType::m_entity->get_uid();
    m_project_builder.get_item_registry().insert(m_entity_uid, this);

    EntityItemBaseType::update();

    // Move the item to its sorted position.
    if (!m_fixed_position && old_entity_name != new_entity_name)
        move_to_sorted_position(this);
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void EntityItem<Entity, ParentEntity, CollectionItem>::slot_delete()
{
    if (m_project_builder.get_rendering_manager().is_rendering())
        schedule_delete();
    else do_delete();
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void EntityItem<Entity, ParentEntity, CollectionItem>::schedule_delete()
{
    m_project_builder.get_rendering_manager().push_delayed_action(
        std::auto_ptr<RenderingManager::IDelayedAction>(
            new EntityDeletionDelayedAction<EntityItem>(this)));

    m_project_builder.get_rendering_manager().reinitialize_rendering();
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void EntityItem<Entity, ParentEntity, CollectionItem>::do_delete()
{
    if (EntityItemBaseType::allows_deletion())
    {
        m_project_builder.remove_entity(EntityItemBaseType::m_entity, m_parent);

        delete this;
    }
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYITEM_H
