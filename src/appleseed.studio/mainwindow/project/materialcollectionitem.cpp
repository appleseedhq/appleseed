
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
#include "materialcollectionitem.h"

// appleseed.renderer headers.
#ifdef WITH_DISNEY_MATERIAL
#include "renderer/modeling/material/disneymaterial.h"
#endif

// appleseed.studio headers.
#include "mainwindow/project/assemblyitem.h"
#ifdef WITH_DISNEY_MATERIAL
#include "mainwindow/project/disneymaterialcustomui.h"
#endif
#include "mainwindow/project/entityeditor.h"
#include "mainwindow/project/entityeditorwindow.h"
#include "mainwindow/project/fixedmodelentityitem.h"
#include "mainwindow/project/materialitem.h"
#include "mainwindow/project/tools.h"

// appleseed.foundation headers.
#include "foundation/utility/settings/settingsfilereader.h"

// appleseed.shared headers.
#include "application/application.h"

// Qt headers.
#include <QFileDialog>
#include <QString>

// boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <cstring>
#include <memory>
#include <string>

using namespace appleseed::shared;
using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_class_uid = new_guid();
}

MaterialCollectionItem::MaterialCollectionItem(
    MaterialContainer&  materials,
    Assembly&           parent,
    AssemblyItem*       parent_item,
    ProjectBuilder&     project_builder,
    ParamArray&         settings)
  : Base(g_class_uid, "Materials", parent, parent_item, project_builder)
  , m_parent(parent)
  , m_parent_item(parent_item)
  , m_settings(settings)
{
    add_items(materials);
}

QMenu* MaterialCollectionItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();
    menu->clear();

    menu->addSeparator();
    menu->addAction("Create Generic Material...", this, SLOT(slot_create_generic()));
#ifdef WITH_OSL
    menu->addAction("Create OSL Material...", this, SLOT(slot_create_osl()));
#endif

#ifdef WITH_DISNEY_MATERIAL
    menu->addSeparator();
    menu->addAction("Create Disney Material...", this, SLOT(slot_create_disney()));
    menu->addAction("Import Disney Material...", this, SLOT(slot_import_disney()));
#endif
    return menu;
}

ItemBase* MaterialCollectionItem::create_item(Material* material)
{
    assert(material);

    ItemBase* item = new MaterialItem(material, m_parent, this, m_project_builder);
    m_project_builder.get_item_registry().insert(material->get_uid(), item);
    return item;
}

void MaterialCollectionItem::slot_create_generic()
{
    do_create_material("generic_material");
}

#ifdef WITH_DISNEY_MATERIAL
void MaterialCollectionItem::slot_create_disney()
{
    do_create_material("disney_material");
}

void MaterialCollectionItem::slot_import_disney()
{
    const filesystem::path root_path(Application::get_root_path());
    const string schema_file_path = (root_path / "schemas" / "settings.xsd").string();

    const char* project_path = m_project_builder.get_project().get_path();
    const filesystem::path project_root_path = filesystem::path(project_path).parent_path();
    const filesystem::path file_path = absolute("material.dmt", project_root_path);
    const filesystem::path file_root_path = file_path.parent_path();

    QFileDialog::Options options;
    QString selected_filter;

    QString filepath =
        QFileDialog::getOpenFileName(
            0,
            "Import...",
            QString::fromStdString(file_root_path.string()),
            "Disney Material (*.dmt);;All Files (*.*)",
            &selected_filter,
            options);

    if (!filepath.isEmpty())
    {
        filepath = QDir::toNativeSeparators(filepath);
        SettingsFileReader reader(global_logger());
        ParamArray parameters;
        bool result = reader.read(
            filepath.toStdString().c_str(),
            schema_file_path.c_str(),
            parameters);

        if (!result)
        {
            show_warning_message_box(
                "Importing error",
                "Failed to import the Disney Material file " + filepath.toStdString());
            return;
        }

        const string name = parameters.get("__name");
        const string model = parameters.get("__model");
        parameters.strings().remove("__name");
        parameters.strings().remove("__model");

        if (model != "disney_material")
        {
            show_warning_message_box(
                "Importing error",
                "Model " + model + " from material file not supported.");
            return;
        }

        for (each<MaterialContainer> i = m_parent.materials(); i; ++i)
        {
            if (strcmp(i->get_name(), name.c_str()) == 0)
            {
                show_warning_message_box(
                    "Importing error",
                    "Material named " + name + " already exists.");
                return;
            }
        }

        DisneyMaterialFactory factory;
        auto_release_ptr<Material> material = factory.create(name.c_str(), parameters);

        int index = find_sorted_position(this, name.c_str());
        ItemBase* item = create_item(material.get());
        insertChild(index, item);

        EntityTraits<Material>::insert_entity(material, m_parent);
        m_project_builder.notify_project_modification();
    }

}
#endif

#ifdef WITH_OSL
void MaterialCollectionItem::slot_create_osl()
{
    do_create_material("osl_material");
}
#endif

void MaterialCollectionItem::do_create_material(const char* model)
{
    typedef EntityTraits<Material> EntityTraits;

    const string window_title =
        string("Create ") +
        EntityTraits::get_human_readable_entity_type_name();

    const string name_suggestion =
        get_name_suggestion(
            EntityTraits::get_entity_type_name(),
            EntityTraits::get_entity_container(Base::m_parent));

    typedef EntityTraits::FactoryRegistrarType FactoryRegistrarType;

    auto_ptr<EntityEditor::IFormFactory> form_factory(
        new FixedModelEntityEditorFormFactory<FactoryRegistrarType>(
            Base::m_project_builder.get_factory_registrar<Material>(),
            name_suggestion,
            model));

    auto_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Assembly>(Base::m_parent));

    auto_ptr<CustomEntityUI> custom_entity_ui;
    Dictionary values;

#ifdef WITH_DISNEY_MATERIAL
    if (strcmp(model, "disney_material") == 0)
    {
        custom_entity_ui = auto_ptr<CustomEntityUI>(
            new DisneyMaterialCustomUI(Base::m_project_builder.get_project()));

        values = DisneyMaterialLayer::get_default_values();
    }
#endif

    open_entity_editor(
        QTreeWidgetItem::treeWidget(),
        window_title,
        Base::m_project_builder.get_project(),
        form_factory,
        entity_browser,
        custom_entity_ui,
        values,
        this,
        SLOT(slot_create_applied(foundation::Dictionary)),
        SLOT(slot_create_accepted(foundation::Dictionary)),
        SLOT(slot_create_canceled(foundation::Dictionary)));
}

}   // namespace studio
}   // namespace appleseed
