
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
// Copyright (c) 2014 Marius Avram, The appleseedhq Organization
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

// Interface header.
#include "materialitem.h"

// appleseed.studio headers.
#include "mainwindow/project/attributeeditor.h"
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entityeditor.h"
#include "mainwindow/project/entityitem.h"
#include "mainwindow/project/entityeditorfactory.h"
#include "mainwindow/project/materialcollectionitem.h"
#include "mainwindow/project/multimodelentityeditorformfactory.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/material.h"

// Standard headers
#include <string.h>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

MaterialItem::MaterialItem(
    Material*                 material,
    Assembly&                 parent,
    MaterialCollectionItem*   collection_item,
    ProjectBuilder&           project_builder)
    : EntityItem<Material, Assembly, MaterialCollectionItem>(material, parent, collection_item, project_builder)
    , m_parent(parent)
    , m_collection_item(collection_item)
    , m_project_builder(project_builder)
{
}

template <typename Entity>
void MaterialItem::edit_helper(AttributeEditor* attribute_editor)
{
    typedef typename renderer::EntityTraits<Material> EntityTraitsType;
    typedef MultiModelEntityEditorFormFactory<
            typename EntityTraitsType::FactoryRegistrarType
                > MultiModelEntityEditorFormFactoryType;

    std::auto_ptr<EntityEditor::IFormFactory> form_factory(
        new MultiModelEntityEditorFormFactoryType(
            m_project_builder.template get_factory_registrar<Material>(),
            m_entity->get_name()));

    std::auto_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Assembly>(m_parent));

    std::auto_ptr<IEntityEditorFactory> entity_editor_factory(
        new EntityEditorFactory<Entity>());

    foundation::Dictionary values =
        EntityTraitsType::get_entity_values(m_entity);

    values.insert(
        MultiModelEntityEditorFormFactoryType::ModelParameter,
        m_entity->get_model());

    if (attribute_editor)
    {
        attribute_editor->edit(
            form_factory,
            entity_browser,
            entity_editor_factory,
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
            m_project_builder.get_project(),
            form_factory,
            entity_browser,
            entity_editor_factory,
            values,
            this,
            SLOT(slot_edit_accepted(foundation::Dictionary)),
            SLOT(slot_edit_accepted(foundation::Dictionary)),
            SLOT(slot_edit_accepted(foundation::Dictionary)));
    }
}

void MaterialItem::slot_edit(AttributeEditor* attribute_editor)
{
    const char* model = m_entity->get_model();

    if (strcmp(model, "generic_material") == 0)
        edit_helper<Material>(attribute_editor);
    else if (strcmp(model, "disney_material") == 0)
        edit_helper<DisneyMaterial>(attribute_editor);
}

}   // namespace studio
}   // namespace appleseed

