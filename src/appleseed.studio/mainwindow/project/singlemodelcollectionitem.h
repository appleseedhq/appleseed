
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_SINGLEMODELCOLLECTIONITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_SINGLEMODELCOLLECTIONITEM_H

// appleseed.studio headers.
#include "mainwindow/project/collectionitem.h"
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entityeditorwindow.h"
#include "mainwindow/project/singlemodelentityeditorformfactory.h"
#include "mainwindow/project/singlemodelentityitem.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <memory>
#include <string>

// Forward declarations.
namespace appleseed { namespace studio { class ItemBase; } }
namespace appleseed { namespace studio { class ProjectBuilder; } }

namespace appleseed {
namespace studio {

#pragma warning (push)
#pragma warning (disable: 4250)     // 'class1' : inherits 'class2::member' via dominance

template <typename Entity, typename ParentEntity>
class SingleModelCollectionItem
  : public CollectionItem<Entity, ParentEntity>
{
  public:
    SingleModelCollectionItem(
        const foundation::UniqueID  class_uid,
        const QString&              title,
        ParentEntity&               parent,
        ProjectBuilder&             project_builder);

  private:
    typedef CollectionItem<Entity, ParentEntity> CollectionItemType;

    virtual ItemBase* create_item(Entity* entity) const override;

    virtual void slot_create() override;
};


//
// SingleModelCollectionItem class implementation.
//

template <typename Entity, typename ParentEntity>
SingleModelCollectionItem<Entity, ParentEntity>::SingleModelCollectionItem(
    const foundation::UniqueID      class_uid,
    const QString&                  title,
    ParentEntity&                   parent,
    ProjectBuilder&                 project_builder)
  : CollectionItemType(class_uid, title, parent, project_builder)
{
}

template <typename Entity, typename ParentEntity>
ItemBase* SingleModelCollectionItem<Entity, ParentEntity>::create_item(Entity* entity) const
{
    assert(entity);

    return
        new SingleModelEntityItem<Entity, ParentEntity>(
            entity,
            CollectionItemType::m_parent,
            CollectionItemType::m_project_builder);
}

template <typename Entity, typename ParentEntity>
void SingleModelCollectionItem<Entity, ParentEntity>::slot_create()
{
    typedef typename renderer::EntityTraits<Entity> EntityTraits;

    const std::string window_title =
        std::string("Create ") +
        EntityTraits::get_human_readable_entity_type_name();

    const std::string name_suggestion =
        get_name_suggestion(
            EntityTraits::get_entity_type_name(),
            EntityTraits::get_entity_container(CollectionItemType::m_parent));

    typedef typename EntityTraits::FactoryType FactoryType;

    std::auto_ptr<EntityEditorWindow::IFormFactory> form_factory(
        new SingleModelEntityEditorFormFactory(
            name_suggestion,
            FactoryType::get_widget_definitions()));

    std::auto_ptr<EntityEditorWindow::IEntityBrowser> entity_browser(
        new EntityBrowser<ParentEntity>(CollectionItemType::m_parent));

    open_entity_editor(
        QTreeWidgetItem::treeWidget(),
        window_title,
        CollectionItemType::m_project_builder.get_project(),
        form_factory,
        entity_browser,
        this,
        SLOT(slot_create_accepted(foundation::Dictionary)));
}

#pragma warning (pop)

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_SINGLEMODELCOLLECTIONITEM_H
