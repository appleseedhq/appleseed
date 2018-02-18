
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
#include "materialcollectionitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblyitem.h"
#ifdef APPLESEED_WITH_DISNEY_MATERIAL
#include "mainwindow/project/disneymaterialcustomui.h"
#endif
#include "mainwindow/project/entityeditor.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/entityeditorwindow.h"
#include "mainwindow/project/fixedmodelentityitem.h"
#include "mainwindow/project/materialitem.h"
#include "mainwindow/project/projectexplorer.h"
#include "mainwindow/project/tools.h"
#include "utility/miscellaneous.h"
#include "utility/settingskeys.h"

// appleseed.shared headers.
#include "application/application.h"

// appleseed.renderer headers.
#include "renderer/api/material.h"
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/settings/settingsfilereader.h"

// Qt headers.
#include <QString>

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <cstring>
#include <memory>
#include <string>
#include <utility>

using namespace appleseed::shared;
using namespace foundation;
using namespace renderer;
using namespace std;
namespace bf = boost::filesystem;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_class_uid = new_guid();
}

MaterialCollectionItem::MaterialCollectionItem(
    EntityEditorContext&    editor_context,
    MaterialContainer&      materials,
    Assembly&               parent,
    AssemblyItem*           parent_item)
  : Base(editor_context, g_class_uid, "Materials", parent, parent_item)
  , m_parent(parent)
  , m_parent_item(parent_item)
{
    add_items(materials);
}

#ifdef APPLESEED_WITH_DISNEY_MATERIAL

const Material& MaterialCollectionItem::create_default_disney_material(const string& material_name)
{
    auto_release_ptr<Material> material =
        DisneyMaterialFactory().create(material_name.c_str(), ParamArray());

    static_cast<DisneyMaterial*>(material.get())->add_new_default_layer();

    Material* material_ptr = material.get();
    add_item(material_ptr);

    EntityTraits<Material>::insert_entity(material, m_parent);
    m_editor_context.m_project_builder.notify_project_modification();

    return *material_ptr;
}

#endif // APPLESEED_WITH_DISNEY_MATERIAL

QMenu* MaterialCollectionItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();
    menu->clear();

    menu->addSeparator();
    menu->addAction("Create Generic Material...", this, SLOT(slot_create_generic()));
    menu->addAction("Create OSL Material...", this, SLOT(slot_create_osl()));

#ifdef APPLESEED_WITH_DISNEY_MATERIAL
    menu->addSeparator();
    menu->addAction("Create Disney Material...", this, SLOT(slot_create_disney()));
    menu->addAction("Import Disney Material...", this, SLOT(slot_import_disney()));
#endif

    return menu;
}

ItemBase* MaterialCollectionItem::create_item(Material* material)
{
    assert(material);

    ItemBase* item =
        new MaterialItem(
            m_editor_context,
            material,
            m_parent,
            this);

    m_editor_context.m_item_registry.insert(*material, item);

    return item;
}

void MaterialCollectionItem::slot_create_generic()
{
    do_create_material("generic_material");
}

void MaterialCollectionItem::slot_create_disney()
{
#ifdef APPLESEED_WITH_DISNEY_MATERIAL
    do_create_material("disney_material");
#endif
}

void MaterialCollectionItem::slot_import_disney()
{
#ifdef APPLESEED_WITH_DISNEY_MATERIAL
    QString filepath =
        get_open_filename(
            nullptr,
            "Import...",
            "Disney Material (*.dmt);;All Files (*.*)",
            m_editor_context.m_settings,
            SETTINGS_FILE_DIALOG_PROJECTS);

    if (!filepath.isEmpty())
    {
        filepath = QDir::toNativeSeparators(filepath);

        const bf::path root_path(Application::get_root_path());
        const bf::path schema_file_path = root_path / "schemas" / "settings.xsd";

        SettingsFileReader reader(global_logger());
        ParamArray parameters;
        const bool success =
            reader.read(
                filepath.toStdString().c_str(),
                schema_file_path.string().c_str(),
                parameters);

        if (!success)
        {
            show_error_message_box(
                "Importing Error",
                "Failed to import the Disney Material file " + filepath.toStdString());
            return;
        }

        string name = parameters.get("__name");
        const string model = parameters.get("__model");
        parameters.strings().remove("__name");
        parameters.strings().remove("__model");

        if (model != "disney_material")
        {
            show_error_message_box(
                "Importing Error",
                "Material model " + model + " is not supported.");
            return;
        }

        // If there is already a material with the same name, rename the imported material.
        for (const_each<MaterialContainer> i = m_parent.materials(); i; ++i)
        {
            if (strcmp(i->get_name(), name.c_str()) == 0)
            {
                name = make_unique_name(name, m_parent.materials());
                break;
            }
        }

        auto_release_ptr<Material> material =
            DisneyMaterialFactory().create(name.c_str(), parameters);
        Material* material_ptr = material.get();

        add_item(material_ptr);

        EntityTraits<Material>::insert_entity(material, m_parent);
        m_editor_context.m_project_builder.notify_project_modification();

        m_editor_context.m_project_explorer.select_entity(material_ptr->get_uid());
    }
#endif
}

void MaterialCollectionItem::slot_create_osl()
{
    do_create_material("osl_material");
}

void MaterialCollectionItem::do_create_material(const char* model)
{
    typedef EntityTraits<Material> EntityTraits;

    const string window_title =
        string("Create ") +
        EntityTraits::get_human_readable_entity_type_name();

    const string name_suggestion =
        make_unique_name(
            EntityTraits::get_entity_type_name(),
            EntityTraits::get_entity_container(Base::m_parent));

    typedef EntityTraits::FactoryRegistrarType FactoryRegistrarType;

    unique_ptr<EntityEditor::IFormFactory> form_factory(
        new FixedModelEntityEditorFormFactory<FactoryRegistrarType>(
            m_editor_context.m_project.get_factory_registrar<Material>(),
            name_suggestion,
            model));

    unique_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Assembly>(Base::m_parent));

    unique_ptr<CustomEntityUI> custom_entity_ui;

#ifdef APPLESEED_WITH_DISNEY_MATERIAL
    if (strcmp(model, "disney_material") == 0)
    {
        custom_entity_ui.reset(
            new DisneyMaterialCustomUI(
                m_editor_context.m_project,
                m_editor_context.m_settings));
    }
#endif

    open_entity_editor(
        QTreeWidgetItem::treeWidget(),
        window_title,
        m_editor_context.m_project,
        m_editor_context.m_settings,
        move(form_factory),
        move(entity_browser),
        move(custom_entity_ui),
        Dictionary(),
        this,
        SLOT(slot_create_applied(foundation::Dictionary)),
        SLOT(slot_create_accepted(foundation::Dictionary)),
        SLOT(slot_create_canceled(foundation::Dictionary)));
}

}   // namespace studio
}   // namespace appleseed
