
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_COLLECTIONITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_COLLECTIONITEM_H

// appleseed.studio headers.
#include "mainwindow/project/collectionitembase.h"
#include "mainwindow/project/entityactions.h"
#include "mainwindow/project/entitycreatorbase.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QMenu>
#include <QObject>
#include <QString>

// Standard headers.
#include <memory>

// Forward declarations.
namespace foundation    { class Dictionary; }

namespace appleseed {
namespace studio {

template <typename Entity, typename ParentEntity, typename ParentItem>
class CollectionItem
  : public CollectionItemBase<Entity>
  , private EntityCreatorBase
{
  public:
    CollectionItem(
        EntityEditorContext&        editor_context,
        const foundation::UniqueID  class_uid,
        const QString&              title,
        ParentEntity&               parent,
        ParentItem*                 parent_item);

    void set_allow_creation(const bool allow);
    bool allows_creation() const;

    virtual QMenu* get_single_item_context_menu() const APPLESEED_OVERRIDE;

  protected:
    ParentEntity&       m_parent;
    ParentItem*         m_parent_item;
    bool                m_allow_creation;

    virtual void slot_create_accepted(foundation::Dictionary values) APPLESEED_OVERRIDE;
    void create(const foundation::Dictionary& values);

  private:
    friend class EntityCreatorBase;
    friend class EntityCreationAction<CollectionItem>;
};


//
// CollectionItem class implementation.
//

template <typename Entity, typename ParentEntity, typename ParentItem>
CollectionItem<Entity, ParentEntity, ParentItem>::CollectionItem(
    EntityEditorContext&            editor_context,
    const foundation::UniqueID      class_uid,
    const QString&                  title,
    ParentEntity&                   parent,
    ParentItem*                     parent_item)
  : CollectionItemBase<Entity>(editor_context, class_uid, title)
  , m_parent(parent)
  , m_parent_item(parent_item)
  , m_allow_creation(true)
{
}

template <typename Entity, typename ParentEntity, typename ParentItem>
inline void CollectionItem<Entity, ParentEntity, ParentItem>::set_allow_creation(const bool allow)
{
    m_allow_creation = allow;
}

template <typename Entity, typename ParentEntity, typename ParentItem>
inline bool CollectionItem<Entity, ParentEntity, ParentItem>::allows_creation() const
{
    return m_allow_creation;
}

template <typename Entity, typename ParentEntity, typename ParentItem>
QMenu* CollectionItem<Entity, ParentEntity, ParentItem>::get_single_item_context_menu() const
{
    QMenu* menu = CollectionItemBase<Entity>::get_single_item_context_menu();

    if (m_allow_creation)
    {
        menu->addSeparator();
        menu->addAction(
            QString("Create %1...").arg(
                renderer::EntityTraits<Entity>::get_human_readable_entity_type_name()),
            this,
            SLOT(slot_create()));
    }

    return menu;
}

template <typename Entity, typename ParentEntity, typename ParentItem>
void CollectionItem<Entity, ParentEntity, ParentItem>::slot_create_accepted(foundation::Dictionary values)
{
    RenderingManager& rendering_manager =
        CollectionItemBase<Entity>::m_editor_context.m_rendering_manager;

    if (rendering_manager.is_rendering())
    {
        rendering_manager.schedule(
            std::auto_ptr<RenderingManager::IScheduledAction>(
                new EntityCreationAction<CollectionItem>(this, values)));

        rendering_manager.reinitialize_rendering();
    }
    else
    {
        catch_entity_creation_errors(
            &CollectionItem::create,
            values,
            renderer::EntityTraits<Entity>::get_human_readable_entity_type_name());
    }
}

template <typename Entity, typename ParentEntity, typename ParentItem>
void CollectionItem<Entity, ParentEntity, ParentItem>::create(const foundation::Dictionary& values)
{
    ProjectBuilder& project_builder = CollectionItemBase<Entity>::m_editor_context.m_project_builder;
    Entity* entity = project_builder.insert_entity<Entity>(m_parent, values);

    m_parent_item->add_item(entity);
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_COLLECTIONITEM_H
