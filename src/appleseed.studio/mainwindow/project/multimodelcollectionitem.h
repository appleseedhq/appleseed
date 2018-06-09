
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_MULTIMODELCOLLECTIONITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_MULTIMODELCOLLECTIONITEM_H

// appleseed.studio headers.
#include "mainwindow/project/collectionitem.h"
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entityeditor.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/multimodelentityeditorformfactory.h"
#include "mainwindow/project/multimodelentityitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QTreeWidgetItem>

// Standard headers.
#include <cassert>
#include <memory>
#include <string>

// Forward declarations.
namespace appleseed     { namespace studio { class ItemBase; } }
namespace foundation    { class Dictionary; }
class QString;

namespace appleseed {
namespace studio {

template <typename Entity, typename ParentEntity, typename ParentItem>
class MultiModelCollectionItem
  : public CollectionItem<Entity, ParentEntity, ParentItem>
{
  public:
    MultiModelCollectionItem(
        EntityEditorContext&        editor_context,
        const foundation::UniqueID  class_uid,
        const QString&              title,
        ParentEntity&               parent,
        ParentItem*                 parent_item);

  private:
    typedef CollectionItem<Entity, ParentEntity, ParentItem> Base;
    typedef MultiModelCollectionItem<Entity, ParentEntity, ParentItem> This;

    ItemBase* create_item(Entity* entity) override;

    void slot_create() override;
};


//
// MultiModelCollectionItem class implementation.
//

template <typename Entity, typename ParentEntity, typename ParentItem>
MultiModelCollectionItem<Entity, ParentEntity, ParentItem>::MultiModelCollectionItem(
    EntityEditorContext&            editor_context,
    const foundation::UniqueID      class_uid,
    const QString&                  title,
    ParentEntity&                   parent,
    ParentItem*                     parent_item)
  : Base(editor_context, class_uid, title, parent, parent_item)
{
}

template <typename Entity, typename ParentEntity, typename ParentItem>
ItemBase* MultiModelCollectionItem<Entity, ParentEntity, ParentItem>::create_item(Entity* entity)
{
    assert(entity);

    return
        new MultiModelEntityItem<Entity, ParentEntity, This>(
            Base::m_editor_context,
            entity,
            Base::m_parent,
            this);
}

template <typename Entity, typename ParentEntity, typename ParentItem>
void MultiModelCollectionItem<Entity, ParentEntity, ParentItem>::slot_create()
{
    typedef typename renderer::EntityTraits<Entity> EntityTraits;

    const std::string window_title =
        std::string("Create ") +
        EntityTraits::get_human_readable_entity_type_name();

    const std::string name_suggestion =
        make_unique_name(
            EntityTraits::get_entity_type_name(),
            EntityTraits::get_entity_container(Base::m_parent));

    typedef typename EntityTraits::FactoryRegistrarType FactoryRegistrarType;

    std::unique_ptr<EntityEditor::IFormFactory> form_factory(
        new MultiModelEntityEditorFormFactory<FactoryRegistrarType>(
            Base::m_editor_context.m_project.template get_factory_registrar<Entity>(),
            name_suggestion));

    std::unique_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<ParentEntity>(Base::m_parent));

    open_entity_editor(
        QTreeWidgetItem::treeWidget(),
        window_title,
        Base::m_editor_context.m_project,
        Base::m_editor_context.m_settings,
        std::move(form_factory),
        std::move(entity_browser),
        this,
        SLOT(slot_create_applied(foundation::Dictionary)),
        SLOT(slot_create_accepted(foundation::Dictionary)),
        SLOT(slot_create_canceled(foundation::Dictionary)));
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_MULTIMODELCOLLECTIONITEM_H
