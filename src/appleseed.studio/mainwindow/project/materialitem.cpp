
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#ifdef APPLESEED_WITH_DISNEY_MATERIAL
#include "mainwindow/project/disneymaterialcustomui.h"
#endif
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/tools.h"
#include "utility/miscellaneous.h"
#include "utility/settingskeys.h"

// appleseed.renderer headers.
#include "renderer/api/material.h"

// appleseed.foundation headers.
#include "foundation/utility/settings/settingsfilewriter.h"

// Qt headers.
#include <QFileInfo>
#include <QMenu>
#include <QString>

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

using namespace foundation;
using namespace renderer;
using namespace std;
namespace bf = boost::filesystem;

namespace appleseed {
namespace studio {

MaterialItem::MaterialItem(
    EntityEditorContext&        editor_context,
    Material*                   entity,
    Assembly&                   parent,
    MaterialCollectionItem*     collection_item)
  : FixedModelEntityItem<Material, Assembly, MaterialCollectionItem>(
        editor_context,
        entity,
        parent,
        collection_item)
{
}

QMenu* MaterialItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();

#ifdef APPLESEED_WITH_DISNEY_MATERIAL
    if (strcmp(m_entity->get_model(), "disney_material") == 0)
    {
        menu->addSeparator();
        menu->addAction("Export...", this, SLOT(slot_export()));
    }
#endif

    return menu;
}

void MaterialItem::slot_edit(AttributeEditor* attribute_editor)
{
    auto_ptr<EntityEditor::IFormFactory> form_factory(
        new FixedModelEntityEditorFormFactoryType(
            m_editor_context.m_project_builder.get_factory_registrar<Material>(),
            m_entity->get_name(),
            m_entity->get_model()));

    auto_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Assembly>(m_parent));

    auto_ptr<CustomEntityUI> custom_entity_ui;

#ifdef APPLESEED_WITH_DISNEY_MATERIAL
    if (strcmp(m_entity->get_model(), "disney_material") == 0)
    {
        custom_entity_ui =
            auto_ptr<CustomEntityUI>(
                new DisneyMaterialCustomUI(
                    m_editor_context.m_project,
                    m_editor_context.m_settings));
    }
#endif

    const Dictionary values = get_values();

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
            m_editor_context.m_project,
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

void MaterialItem::slot_export()
{
    const char* project_path = m_editor_context.m_project.get_path();
    const bf::path project_root_path = bf::path(project_path).parent_path();
    const bf::path file_path = absolute("material.dmt", project_root_path);
    const bf::path file_root_path = file_path.parent_path();

    QString filepath =
        get_save_filename(
            0,
            "Export...",
            "Disney Materials (*.dmt)",
            m_editor_context.m_settings,
            SETTINGS_FILE_DIALOG_PROJECTS);

    if (!filepath.isEmpty())
    {
        if (QFileInfo(filepath).suffix().isEmpty())
            filepath += ".dmt";

        filepath = QDir::toNativeSeparators(filepath);

        ParamArray parameters = m_entity->get_parameters();
        parameters.insert("__name", m_entity->get_name());
        parameters.insert("__model", m_entity->get_model());

        SettingsFileWriter writer;
        if (!writer.write(filepath.toStdString().c_str(), parameters))
        {
            show_error_message_box(
                "Exporting Error",
                "Failed to export the Disney Material file " + filepath.toStdString() + ".");
        }
    }
}

}   // namespace studio
}   // namespace appleseed
