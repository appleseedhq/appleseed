
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
#include "mainwindow/project/entityeditor.h"
#include "mainwindow/project/fixedmodelentityitem.h"

// Standard headers.
#include <string.h>

using namespace foundation;
using namespace renderer;

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
    menu->addAction("Create Disney Material...", this, SLOT(slot_create_disney()));
#ifdef WITH_OSL
    menu->addAction("Create OSL Material...", this, SLOT(slot_create_osl()));
#endif
    return menu;
}

ItemBase* MaterialCollectionItem::create_item(Material* material)
{
    assert(material);

    typedef FixedModelEntityItem<renderer::Material, renderer::Assembly, MaterialCollectionItem> MaterialItem;
    
    ItemBase* item = new MaterialItem(material, m_parent, this, m_project_builder);
    m_project_builder.get_item_registry().insert(material->get_uid(), item);
    return item;
}

void MaterialCollectionItem::slot_create_generic()
{
    do_create_material("generic_material");  
}

void MaterialCollectionItem::slot_create_disney()
{
    //do_create_material("disney_material");    
}

#ifdef WITH_OSL
void MaterialCollectionItem::slot_create_osl()
{
    do_create_material("osl_material");
}
#endif

void MaterialCollectionItem::do_create_material(const char* model)
{
    typedef typename renderer::EntityTraits<Material> EntityTraits;

    const std::string window_title =
        std::string("Create ") +
        EntityTraits::get_human_readable_entity_type_name();

    const std::string name_suggestion =
        get_name_suggestion(
            EntityTraits::get_entity_type_name(),
            EntityTraits::get_entity_container(Base::m_parent));

    typedef typename EntityTraits::FactoryRegistrarType FactoryRegistrarType;

    std::auto_ptr<EntityEditor::IFormFactory> form_factory(
        new FixedModelEntityEditorFormFactory<FactoryRegistrarType>(
            Base::m_project_builder.get_factory_registrar<Material>(),
            name_suggestion,
            model));

    std::auto_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Assembly>(Base::m_parent));

    open_entity_editor(
        QTreeWidgetItem::treeWidget(),
        window_title,
        Base::m_project_builder.get_project(),
        form_factory,
        entity_browser,
        this,
        SLOT(slot_create_applied(foundation::Dictionary)),
        SLOT(slot_create_accepted(foundation::Dictionary)),
        SLOT(slot_create_canceled(foundation::Dictionary)));
}

}   // namespace studio
}   // namespace appleseed
