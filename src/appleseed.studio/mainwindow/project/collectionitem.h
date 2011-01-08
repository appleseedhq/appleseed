
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "mainwindow/project/entitycreatorbase.h"
#include "mainwindow/project/projectbuilder.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QMenu>
#include <QObject>
#include <QString>
#include <QWidget>

namespace appleseed {
namespace studio {

#pragma warning (push)
#pragma warning (disable: 4250)     // 'class1' : inherits 'class2::member' via dominance

// Work around a limitation in Qt: a template class cannot have slots.
class CollectionItemSlots
  : public virtual ItemBase
{
    Q_OBJECT

  protected slots:
    virtual void slot_create() {}
    virtual void slot_create_accepted(foundation::Dictionary values) {}
};

template <typename Entity, typename ParentEntity>
class CollectionItem
  : public CollectionItemBase<Entity>
  , public CollectionItemSlots
  , private EntityCreatorBase
{
  public:
    CollectionItem(
        ParentEntity&               parent,
        ProjectBuilder&             project_builder);

    virtual QMenu* get_single_item_context_menu() const;

  protected:
    ParentEntity&       m_parent;
    ProjectBuilder&     m_project_builder;

    virtual void slot_create_accepted(foundation::Dictionary values);

  private:
    void create(const foundation::Dictionary& values);
};


//
// CollectionItem class implementation.
//

template <typename Entity, typename ParentEntity>
CollectionItem<Entity, ParentEntity>::CollectionItem(
    ParentEntity&                   parent,
    ProjectBuilder&                 project_builder)
  : m_parent(parent)
  , m_project_builder(project_builder)
{
}

template <typename Entity, typename ParentEntity>
QMenu* CollectionItem<Entity, ParentEntity>::get_single_item_context_menu() const
{
    QMenu* menu = new QMenu(treeWidget());

    menu->addAction(
        QString("Create %1...").arg(
            renderer::EntityTraits<Entity>::get_human_readable_entity_type_name()),
        this,
        SLOT(slot_create()));

    return menu;
}

template <typename Entity, typename ParentEntity>
void CollectionItem<Entity, ParentEntity>::slot_create_accepted(foundation::Dictionary values)
{
    catch_entity_creation_errors(
        &CollectionItem::create,
        values,
        renderer::EntityTraits<Entity>::get_human_readable_entity_type_name());
}

template <typename Entity, typename ParentEntity>
void CollectionItem<Entity, ParentEntity>::create(const foundation::Dictionary& values)
{
    m_project_builder.insert_entity<Entity>(m_parent, values);

    qobject_cast<QWidget*>(sender())->close();
}

#pragma warning (pop)

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_COLLECTIONITEM_H
