
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

// appleseed.studio headers.
#include "mainwindow/project/assemblyitem.h"
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entityeditorfactory.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/multimodelentityitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/tools.h"
#include "mainwindow/rendering/renderingmanager.h"
#include "utility/interop.h"
#include "utility/settingskeys.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>

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
  : CollectionItem<Material, Assembly, AssemblyItem>(g_class_uid, "Materials", parent, parent_item, project_builder)
  , m_parent(parent)
  , m_parent_item(parent_item)
  , m_settings(settings)
{
    add_items(materials);
}

ItemBase* MaterialCollectionItem::create_item(Material* material)
{
    assert(material);

    typedef MultiModelEntityItem<Material, Assembly, MaterialCollectionItem> GenericMaterialItem;

    ItemBase* item = new GenericMaterialItem(
        material,
        m_parent,
        this,
        m_project_builder);

    m_project_builder.get_item_registry().insert(material->get_uid(), item);

    return item;
}

QMenu* MaterialCollectionItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();
    menu->clear();

    menu->addSeparator();
    menu->addAction("Create Disney Material...", this, SLOT(slot_create_disney()));
    menu->addAction("Create Generic Material...", this, SLOT(slot_create_generic()));
    return menu;
}

template <typename Entity>
void MaterialCollectionItem::create_editor()
{
    typedef typename renderer::EntityTraits<Entity> EntityTraits;

    const std::string window_title =
        std::string("Create ") +
        EntityTraits::get_human_readable_entity_type_name();

    const std::string name_suggestion =
        get_name_suggestion(
            EntityTraits::get_entity_type_name(),
            EntityTraits::get_entity_container(m_parent));

    typedef typename EntityTraits::FactoryRegistrarType FactoryRegistrarType;

    std::auto_ptr<EntityEditor::IFormFactory> form_factory(
        new MultiModelEntityEditorFormFactory<FactoryRegistrarType>(
            m_project_builder.template get_factory_registrar<Entity>(),
            name_suggestion));

    std::auto_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Assembly>(m_parent));

    std::auto_ptr<IEntityEditorFactory> entity_editor_factory(
        new EntityEditorFactory<Entity>());

    open_entity_editor(
        QTreeWidgetItem::treeWidget(),
        window_title,
        m_project_builder.get_project(),
        form_factory,
        entity_browser,
        entity_editor_factory,
        this,
        SLOT(slot_create_applied(foundation::Dictionary)),
        SLOT(slot_create_accepted(foundation::Dictionary)),
        SLOT(slot_create_canceled(foundation::Dictionary)));
}

void MaterialCollectionItem::slot_create_generic()
{
    create_editor<Material>();
}

void MaterialCollectionItem::slot_create_disney()
{
    create_editor<DisneyMaterial>();
}

}   // namespace studio
}   // namespace appleseed
