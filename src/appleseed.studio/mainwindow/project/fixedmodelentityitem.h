
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_FIXEDMODELENTITYITEM_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_FIXEDMODELENTITYITEM_H

// appleseed.studio headers.
#include "mainwindow/project/attributeeditor.h"
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entityeditor.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/entityitem.h"
#include "mainwindow/project/fixedmodelentityeditorformfactory.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <memory>
#include <string>

// Forward declarations.
namespace appleseed { namespace studio { class EntityEditorContext; } }

namespace appleseed {
namespace studio {

template <typename Entity, typename ParentEntity, typename CollectionItem>
class FixedModelEntityItem
  : public EntityItem<Entity, ParentEntity, CollectionItem>
{
  public:
    FixedModelEntityItem(
        EntityEditorContext&    editor_context,
        Entity*                 entity,
        ParentEntity&           parent,
        CollectionItem*         collection_item);

  protected:
    typedef EntityItem<Entity, ParentEntity, CollectionItem> Base;
    typedef typename renderer::EntityTraits<Entity> EntityTraitsType;

    typedef FixedModelEntityEditorFormFactory<
        typename EntityTraitsType::FactoryRegistrarType
    > FixedModelEntityEditorFormFactoryType;

  private:
    virtual void slot_edit(AttributeEditor* attribute_editor) APPLESEED_OVERRIDE;
};


//
// FixedModelEntityItem class implementation.
//

template <typename Entity, typename ParentEntity, typename CollectionItem>
FixedModelEntityItem<Entity, ParentEntity, CollectionItem>::FixedModelEntityItem(
    EntityEditorContext&        editor_context,
    Entity*                     entity,
    ParentEntity&               parent,
    CollectionItem*             collection_item)
  : Base(editor_context, entity, parent, collection_item)
{
}

template <typename Entity, typename ParentEntity, typename CollectionItem>
void FixedModelEntityItem<Entity, ParentEntity, CollectionItem>::slot_edit(AttributeEditor* attribute_editor)
{
    if (!Base::allows_edition())
        return;

    std::auto_ptr<EntityEditor::IFormFactory> form_factory(
        new FixedModelEntityEditorFormFactoryType(
            Base::m_editor_context.m_project_builder.template get_factory_registrar<Entity>(),
            Base::m_entity->get_name(),
            Base::m_entity->get_model()));

    std::auto_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<ParentEntity>(Base::m_parent));

    foundation::Dictionary values =
        EntityTraitsType::get_entity_values(Base::m_entity);

    if (attribute_editor)
    {
        attribute_editor->edit(
            form_factory,
            entity_browser,
            std::auto_ptr<CustomEntityUI>(),
            values,
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
            form_factory,
            entity_browser,
            values,
            this,
            SLOT(slot_edit_accepted(foundation::Dictionary)),
            SLOT(slot_edit_accepted(foundation::Dictionary)),
            SLOT(slot_edit_accepted(foundation::Dictionary)));
    }
}

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_FIXEDMODELENTITYITEM_H
