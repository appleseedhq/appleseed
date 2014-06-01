
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
#include "mainwindow/project/multimodelentityitem.h"
#include "mainwindow/project/singlemodelentityitem.h"

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
    typedef SingleModelEntityItem<DisneyMaterial, Assembly, MaterialCollectionItem> DisneyMaterialItem;
    const char* model = material->get_model();

    ItemBase* item;
    if (strcmp(model, "generic_material") == 0)
    {
        item = new GenericMaterialItem(
            material,
            m_parent,
            this,
            m_project_builder);
    }
    else if (strcmp(model, "disney_material") == 0)
    {
        item = new DisneyMaterialItem(
            static_cast<DisneyMaterial*>(material),
            m_parent,
            this,
            m_project_builder);
    }

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

void MaterialCollectionItem::slot_create_generic()
{
    typedef typename renderer::EntityTraits<Material> EntityTraits;

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
            m_project_builder.get_factory_registrar<Material>(),
            name_suggestion));

    std::auto_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Assembly>(m_parent));

    std::auto_ptr<IEntityEditorFactory> entity_editor_factory(
        new EntityEditorFactory<Material>());

    open_entity_editor(
        QTreeWidgetItem::treeWidget(),
        window_title,
        m_project_builder.get_project(),
        form_factory,
        entity_browser,
        entity_editor_factory,
        this,
        SLOT(slot_create_applied(foundation::Dictionary)),
        SLOT(slot_create_accepted_generic(foundation::Dictionary)),
        SLOT(slot_create_canceled(foundation::Dictionary)));
}

void MaterialCollectionItem::slot_create_disney()
{
    typedef typename renderer::EntityTraits<DisneyMaterial> EntityTraits;

    const std::string window_title =
        std::string("Create ") +
        EntityTraits::get_human_readable_entity_type_name();

    const std::string name_suggestion =
        get_name_suggestion(
            EntityTraits::get_entity_type_name(),
            EntityTraits::get_entity_container(m_parent));

    typedef typename EntityTraits::FactoryType FactoryType;

    std::auto_ptr<EntityEditor::IFormFactory> form_factory(
        new SingleModelEntityEditorFormFactory(
            name_suggestion,
            FactoryType::get_input_metadata()));

    std::auto_ptr<EntityEditor::IEntityBrowser> entity_browser(
        new EntityBrowser<Assembly>(m_parent));

    std::auto_ptr<IEntityEditorFactory> entity_editor_factory(
        new EntityEditorFactory<DisneyMaterial>());

    open_entity_editor(
        QTreeWidgetItem::treeWidget(),
        window_title,
        m_project_builder.get_project(),
        form_factory,
        entity_browser,
        entity_editor_factory,
        this,
        SLOT(slot_create_applied(foundation::Dictionary)),
        SLOT(slot_create_accepted_disney(foundation::Dictionary)),
        SLOT(slot_create_canceled(foundation::Dictionary)));
}

void MaterialCollectionItem::slot_create_accepted_generic(foundation::Dictionary values)
{
    create_accepted<Material>(values);
}

void MaterialCollectionItem::slot_create_accepted_disney(foundation::Dictionary values)
{
    create_accepted<DisneyMaterial>(values);
}

template <typename Material>
void MaterialCollectionItem::create_accepted(const foundation::Dictionary& values)
{
    catch_entity_creation_errors(
        m_project_builder.get_rendering_manager().is_rendering()
            ? &MaterialCollectionItem::schedule_create<Material>
            : &MaterialCollectionItem::create<Material>,
        values,
        renderer::EntityTraits<Material>::get_human_readable_entity_type_name());
}

template <typename Material>
void MaterialCollectionItem::schedule_create(const foundation::Dictionary& values)
{
    m_project_builder.get_rendering_manager().push_delayed_action(
        std::auto_ptr<RenderingManager::IDelayedAction>(
            new MaterialCreationDelayedAction<MaterialCollectionItem, Material>(this, values)));

    m_project_builder.get_rendering_manager().reinitialize_rendering();
}

template <typename Material>
void MaterialCollectionItem::create(const foundation::Dictionary& values)
{
    Material* material = m_project_builder.insert_entity<Material>(m_parent, values);
    m_parent_item->add_item(material);
}

}   // namespace studio
}   // namespace appleseed
