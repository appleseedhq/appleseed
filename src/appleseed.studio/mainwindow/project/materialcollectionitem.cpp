
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "mainwindow/project/entityeditor.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/entityeditorwindow.h"
#include "mainwindow/project/fixedmodelentityitem.h"
#include "mainwindow/project/materialitem.h"
#include "mainwindow/project/projectexplorer.h"
#include "mainwindow/project/tools.h"
#include "utility/settingskeys.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.common headers.
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

using namespace appleseed::common;
using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;
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

QMenu* MaterialCollectionItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();
    menu->clear();
    menu->addSeparator();
    menu->addAction("Create Generic Material...", this, SLOT(slot_create_generic()));
    menu->addAction("Create OSL Material...", this, SLOT(slot_create_osl()));
    return menu;
}

ItemBase* MaterialCollectionItem::create_item(Material* material)
{
    assert(material);

    return
        new MaterialItem(
            m_editor_context,
            material,
            m_parent,
            this);
}

void MaterialCollectionItem::slot_create_generic()
{
    do_create_material("generic_material");
}

void MaterialCollectionItem::slot_create_osl()
{
    do_create_material("osl_material");
}

void MaterialCollectionItem::do_create_material(const char* model)
{
    typedef EntityTraits<Material> EntityTraits;

    const std::string window_title =
        std::string("Create ") +
        EntityTraits::get_human_readable_entity_type_name();

    const std::string name_suggestion =
        make_unique_name(
            EntityTraits::get_entity_type_name(),
            EntityTraits::get_entity_container(Base::m_parent));

    typedef EntityTraits::FactoryRegistrarType FactoryRegistrarType;

    std::unique_ptr<EntityEditor::IFormFactory> form_factory(
        new FixedModelEntityEditorFormFactory<FactoryRegistrarType>(
            m_editor_context.m_project.get_factory_registrar<Material>(),
            name_suggestion,
            model));

    std::unique_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Assembly>(Base::m_parent));

    open_entity_editor(
        QTreeWidgetItem::treeWidget(),
        window_title,
        m_editor_context.m_project,
        m_editor_context.m_settings,
        std::move(form_factory),
        std::move(entity_browser),
        std::unique_ptr<CustomEntityUI>(),
        Dictionary(),
        this,
        SLOT(slot_create_applied(foundation::Dictionary)),
        SLOT(slot_create_accepted(foundation::Dictionary)),
        SLOT(slot_create_canceled(foundation::Dictionary)));
}

}   // namespace studio
}   // namespace appleseed
