
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
#include "mainwindow/project/attributeeditor.h"
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entityeditor.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/entityitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/singlemodelentityeditorformfactory.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <memory>
#include <string>

namespace appleseed {
namespace studio {

//
// Entity item class for single-model entities such as object instances,
// texture instances or environments.
//

template <typename Entity, typename ParentEntity, typename CollectionItem>
class SingleModelEntityItem
  : public EntityItem<Entity, ParentEntity, CollectionItem>
{
  public:
    SingleModelEntityItem(
        EntityEditorContext&    editor_context,
        Entity*                 entity,
        ParentEntity&           parent,
        CollectionItem*         collection_item);

    foundation::Dictionary get_values() const override;

  private:
    typedef EntityItem<Entity, ParentEntity, CollectionItem> Base;
    typedef typename renderer::EntityTraits<Entity> EntityTraitsType;

    void slot_edit(AttributeEditor* attribute_editor) override;
};


//
// SingleModelEntityItem class implementation.
//

template <typename Entity, typename ParentEntity, typename CollectionItem>
SingleModelEntityItem<Entity, ParentEntity, CollectionItem>::SingleModelEntityItem(
    EntityEditorContext&        editor_context,
    Entity*                     entity,
    ParentEntity&               parent,
    CollectionItem*             collection_item)
  : Base(editor_context, entity, parent, collection_item)
{
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
foundation::Dictionary SingleModelEntityItem<Entity, ParentEntity, CollectionItem>::get_values() const
{
    return renderer::EntityTraits<Entity>::get_entity_values(Base::m_entity);
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void SingleModelEntityItem<Entity, ParentEntity, CollectionItem>::slot_edit(AttributeEditor* attribute_editor)
{
    if (!Base::allows_edition())
        return;

    typedef typename EntityTraitsType::FactoryType FactoryType;

    std::unique_ptr<EntityEditor::IFormFactory> form_factory(
        new SingleModelEntityEditorFormFactory(
            Base::m_entity->get_name(),
            FactoryType::get_input_metadata()));

    std::unique_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<ParentEntity>(Base::m_parent));

    if (attribute_editor)
    {
        attribute_editor->edit(
            std::move(form_factory),
            std::move(entity_browser),
            std::unique_ptr<CustomEntityUI>(),
            get_values(),
            this,
            SLOT(slot_edit_accepted(foundation::Dictionary)));
    }
    else
    {
        const std::string window_title =
            std::string("Edit ") +
            EntityTraitsType::get_human_readable_entity_type_name();

        open_entity_editor(
            QTreeWidgetItem::treeWidget(),
            window_title,
            Base::m_editor_context.m_project,
            Base::m_editor_context.m_settings,
            std::move(form_factory),
            std::move(entity_browser),
            get_values(),
            this,
            SLOT(slot_edit_accepted(foundation::Dictionary)),
            SLOT(slot_edit_accepted(foundation::Dictionary)),
            SLOT(slot_edit_accepted(foundation::Dictionary)));
    }
}

}   // namespace studio
}   // namespace appleseed
