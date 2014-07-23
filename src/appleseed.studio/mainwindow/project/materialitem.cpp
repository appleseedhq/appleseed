
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
#ifdef WITH_DISNEY_MATERIAL
#include "mainwindow/project/disneymaterialcustomui.h"
#endif

// appleseed.renderer headers.
#ifdef WITH_DISNEY_MATERIAL
#include "renderer/modeling/material/disneymaterial.h"
#endif

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

MaterialItem::MaterialItem(
    Material*                   entity,
    Assembly&                   parent,
    MaterialCollectionItem*     collection_item,
    ProjectBuilder&             project_builder)
  : FixedModelEntityItem<Material, Assembly, MaterialCollectionItem>(entity, parent, collection_item, project_builder)
{
}

void MaterialItem::slot_edit(AttributeEditor* attribute_editor)
{
    auto_ptr<EntityEditor::IFormFactory> form_factory(
        new FixedModelEntityEditorFormFactoryType(
            m_project_builder.get_factory_registrar<Material>(),
            m_entity->get_name(),
            m_entity->get_model()));

    auto_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Assembly>(m_parent));

    auto_ptr<CustomEntityUI> custom_entity_ui;

#ifdef WITH_DISNEY_MATERIAL
    if (strcmp(m_entity->get_model(), "disney_material") == 0)
    {
        custom_entity_ui =
            auto_ptr<CustomEntityUI>(
                new DisneyMaterialCustomUI(
                    Base::m_project_builder.get_project(),
                    DisneyMaterialLayer::get_input_metadata()));
    }
#endif

    foundation::Dictionary values =
        EntityTraitsType::get_entity_values(m_entity);

    if (attribute_editor)
    {
        attribute_editor->edit(
            form_factory,
            entity_browser,
            custom_entity_ui,
            values,
            this,
            SLOT(slot_edit_accepted(foundation::Dictionary)));
    }
    else
    {
        const string window_title =
            string("Edit ") +
            EntityTraitsType::get_human_readable_entity_type_name();

        open_entity_editor(
            QTreeWidgetItem::treeWidget(),
            window_title,
            m_project_builder.get_project(),
            form_factory,
            entity_browser,
            custom_entity_ui,
            values,
            this,
            SLOT(slot_edit_accepted(foundation::Dictionary)),
            SLOT(slot_edit_accepted(foundation::Dictionary)),
            SLOT(slot_edit_accepted(foundation::Dictionary)));
    }
}

}       // namespace studio
}       // namespace appleseed
