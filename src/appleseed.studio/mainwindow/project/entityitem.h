
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "mainwindow/project/entityitembase.h"
#include "mainwindow/project/projectbuilder.h"
#include "utility/treewidget.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"

// Qt headers.
#include <QObject>
#include <QWidget>

namespace appleseed {
namespace studio {

template <typename Entity, typename ParentEntity>
class EntityItem
  : public EntityItemBase<Entity>
  , private EntityCreatorBase
{
  public:
    EntityItem(
        Entity*             entity,
        ParentEntity&       parent,
        ProjectBuilder&     project_builder);

    void set_fixed_position(const bool fixed);
    bool is_fixed_position() const;

  protected:
    typedef EntityItemBase<Entity> EntityItemBaseType;

    ParentEntity&           m_parent;
    ProjectBuilder&         m_project_builder;

    virtual void slot_edit_accepted(foundation::Dictionary values);
    virtual void slot_delete();

    void edit(const foundation::Dictionary& values);

  private:
    friend class EntityCreatorBase;

    bool                    m_fixed_position;
};


//
// EntityItem class implementation.
//

template <typename Entity, typename ParentEntity>
EntityItem<Entity, ParentEntity>::EntityItem(
    Entity*                 entity,
    ParentEntity&           parent,
    ProjectBuilder&         project_builder)
  : EntityItemBaseType(entity)
  , m_parent(parent)
  , m_project_builder(project_builder)
  , m_fixed_position(false)
{
}

template <typename Entity, typename ParentEntity>
void EntityItem<Entity, ParentEntity>::set_fixed_position(const bool fixed)
{
    m_fixed_position = fixed;
}

template <typename Entity, typename ParentEntity>
bool EntityItem<Entity, ParentEntity>::is_fixed_position() const
{
    return m_fixed_position;
}

template <typename Entity, typename ParentEntity>
void EntityItem<Entity, ParentEntity>::slot_edit_accepted(foundation::Dictionary values)
{
    catch_entity_creation_errors(
        &EntityItem::edit,
        values,
        renderer::EntityTraits<Entity>::get_human_readable_entity_type_name());
}

template <typename Entity, typename ParentEntity>
void EntityItem<Entity, ParentEntity>::slot_delete()
{
    if (EntityItemBaseType::allows_deletion())
    {
        m_project_builder.remove_entity(EntityItemBaseType::m_entity, m_parent);

        delete this;
    }
}

template <typename Entity, typename ParentEntity>
void EntityItem<Entity, ParentEntity>::edit(const foundation::Dictionary& values)
{
    EntityItemBaseType::m_entity =
        m_project_builder.edit_entity(
            EntityItemBaseType::m_entity,
            m_parent,
            values);

    EntityItemBaseType::update();

    if (!m_fixed_position)
        move_to_sorted_position(this);

    qobject_cast<QWidget*>(QObject::sender())->close();
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYITEM_H
