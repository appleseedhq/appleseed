
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "assemblyitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblycollectionitem.h"
#include "mainwindow/project/assemblyinstanceitem.h"
#include "mainwindow/project/collectionitem.h"
#include "mainwindow/project/instancecollectionitem.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/materialcollectionitem.h"
#include "mainwindow/project/multimodelcollectionitem.h"
#include "mainwindow/project/objectcollectionitem.h"
#include "mainwindow/project/objectinstanceitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/singlemodelcollectionitem.h"
#include "mainwindow/project/texturecollectionitem.h"
#include "mainwindow/project/tools.h"
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.renderer headers.
#include "renderer/api/bsdf.h"
#include "renderer/api/edf.h"
#include "renderer/api/entity.h"
#include "renderer/api/light.h"
#include "renderer/api/material.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QMenu>
#include <QMessageBox>
#include <QString>

// Standard headers.
#include <memory>
#include <string>
#include <vector>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

namespace
{
    const UniqueID g_class_uid = new_guid();
}

AssemblyItem::AssemblyItem(
    Assembly&       assembly,
    BaseGroup&      parent,
    BaseGroupItem*  parent_item,
    ProjectBuilder& project_builder,
    ParamArray&     settings)
  : BaseGroupItem(g_class_uid, assembly, project_builder, settings)
  , m_assembly(assembly)
  , m_parent(parent)
  , m_parent_item(parent_item)
  , m_project_builder(project_builder)
{
    set_title(QString::fromAscii(assembly.get_name()));

    set_allow_edition(false);

    insertChild(
        3,
        m_bsdf_collection_item = add_multi_model_collection_item<BSDF>(assembly.bsdfs()));

    insertChild(
        4,
        m_edf_collection_item = add_multi_model_collection_item<EDF>(assembly.edfs()));

    insertChild(
        5,
        m_surface_shader_collection_item = add_multi_model_collection_item<SurfaceShader>(assembly.surface_shaders()));

    insertChild(
        6,
        m_material_collection_item = 
            new MaterialCollectionItem(
                assembly.materials(),
                assembly,
                this,
                project_builder,
                settings));

    insertChild(
        7,
        m_light_collection_item = add_multi_model_collection_item<Light>(assembly.lights()));

    insertChild(
        8,
        m_object_collection_item =
            new ObjectCollectionItem(
                assembly.objects(),
                assembly,
                this,
                project_builder,
                settings));

    insertChild(
        9,
        m_object_instance_collection_item =
            new ObjectInstanceCollectionItem(
                new_guid(),
                EntityTraits<ObjectInstance>::get_human_readable_collection_type_name(),
                assembly,
                project_builder));
    m_object_instance_collection_item->add_items(assembly.object_instances());
}

QMenu* AssemblyItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();

    menu->addSeparator();
    menu->addAction("Instantiate...", this, SLOT(slot_instantiate()));

    menu->addSeparator();
    menu->addAction("Import Objects...", m_object_collection_item, SLOT(slot_import_objects()));
    menu->addAction("Import Textures...", &get_texture_collection_item(), SLOT(slot_import_textures()));

    menu->addSeparator();
    menu->addAction("Create Assembly...", &get_assembly_collection_item(), SLOT(slot_create()));
    menu->addAction("Create BSDF...", m_bsdf_collection_item, SLOT(slot_create()));
    menu->addAction("Create Color...", &get_color_collection_item(), SLOT(slot_create()));
    menu->addAction("Create EDF...", m_edf_collection_item, SLOT(slot_create()));
    menu->addAction("Create Light...", m_light_collection_item, SLOT(slot_create()));

    QMenu* submenu = menu->addMenu("Create Material...");
    submenu->addAction("Create Disney Material...", m_material_collection_item, SLOT(slot_create_disney()));
    submenu->addAction("Create Generic Material...", m_material_collection_item, SLOT(slot_create_generic()));

    menu->addAction("Create Surface Shader...", m_surface_shader_collection_item, SLOT(slot_create()));

    return menu;
}

void AssemblyItem::add_item(BSDF* bsdf)
{
    m_bsdf_collection_item->add_item(bsdf);
}

void AssemblyItem::add_item(EDF* edf)
{
    m_edf_collection_item->add_item(edf);
}

void AssemblyItem::add_item(SurfaceShader* surface_shader)
{
    m_surface_shader_collection_item->add_item(surface_shader);
}

void AssemblyItem::add_item(Material* material)
{
    m_material_collection_item->add_item(material);
}

void AssemblyItem::add_item(Light* light)
{
    m_light_collection_item->add_item(light);
}

void AssemblyItem::add_item(Object* object)
{
    m_object_collection_item->add_item(object);
}

void AssemblyItem::add_item(ObjectInstance* object_instance)
{
    m_object_instance_collection_item->add_item(object_instance);
}

ObjectCollectionItem& AssemblyItem::get_object_collection_item() const
{
    return *m_object_collection_item;
}

AssemblyItem::ObjectInstanceCollectionItem& AssemblyItem::get_object_instance_collection_item() const
{
    return *m_object_instance_collection_item;
}

void AssemblyItem::slot_instantiate()
{
    const string instance_name_suggestion =
        get_name_suggestion(
            string(m_assembly.get_name()) + "_inst",
            m_parent.assembly_instances());

    const string instance_name =
        get_entity_name_dialog(
            treeWidget(),
            "Instantiate Assembly",
            "Assembly Instance Name:",
            instance_name_suggestion);

    if (!instance_name.empty())
    {
        if (m_project_builder.get_rendering_manager().is_rendering())
            schedule_instantiate(instance_name);
        else
            do_instantiate(instance_name);
    }
}

void AssemblyItem::do_instantiate(const std::string& name)
{
    auto_release_ptr<AssemblyInstance> assembly_instance(
        AssemblyInstanceFactory::create(
            name.c_str(),
            ParamArray(),
            m_assembly.get_name()));

    m_parent_item->get_assembly_instance_collection_item().add_item(assembly_instance.get());
    m_parent.assembly_instances().insert(assembly_instance);

    m_project_builder.get_project().get_scene()->bump_version_id();
    m_project_builder.notify_project_modification();
}

void AssemblyItem::schedule_instantiate(const std::string& name)
{
    m_project_builder.get_rendering_manager().push_delayed_action(
            std::auto_ptr<RenderingManager::IDelayedAction>(
                new EntityInstantiationDelayedAction<AssemblyItem>(this, name)));

    m_project_builder.get_rendering_manager().reinitialize_rendering();
}

template <typename Entity, typename EntityContainer>
CollectionItem<Entity, Assembly, AssemblyItem>* AssemblyItem::add_single_model_collection_item(EntityContainer& entities)
{
    CollectionItem<Entity, Assembly, AssemblyItem>* item =
        new SingleModelCollectionItem<Entity, Assembly, AssemblyItem>(
            new_guid(),
            EntityTraits<Entity>::get_human_readable_collection_type_name(),
            m_assembly,
            this,
            m_project_builder);

    item->add_items(entities);

    return item;
}

template <typename Entity, typename EntityContainer>
CollectionItem<Entity, Assembly, AssemblyItem>* AssemblyItem::add_multi_model_collection_item(EntityContainer& entities)
{
    CollectionItem<Entity, Assembly, AssemblyItem>* item =
        new MultiModelCollectionItem<Entity, Assembly, AssemblyItem>(
            new_guid(),
            EntityTraits<Entity>::get_human_readable_collection_type_name(),
            m_assembly,
            this,
            m_project_builder);

    item->add_items(entities);

    return item;
}

namespace
{
    int ask_assembly_deletion_confirmation(const char* assembly_name)
    {
        QMessageBox msgbox;
        msgbox.setWindowTitle("Delete Assembly?");
        msgbox.setIcon(QMessageBox::Question);
        msgbox.setText(QString("You are about to delete the assembly \"%1\" and all its instances.").arg(assembly_name));
        msgbox.setInformativeText("Continue?");
        msgbox.setStandardButtons(QMessageBox::Yes | QMessageBox::No);
        msgbox.setDefaultButton(QMessageBox::No);
        return msgbox.exec();
    }
}

namespace
{
    vector<UniqueID> collect_assembly_instances(
        const AssemblyInstanceContainer&    assembly_instances,
        const UniqueID                      assembly_uid)
    {
        vector<UniqueID> collected;

        for (const_each<AssemblyInstanceContainer> i = assembly_instances; i; ++i)
        {
            const Assembly* assembly = i->find_assembly();

            if (assembly && assembly->get_uid() == assembly_uid)
                collected.push_back(i->get_uid());
        }

        return collected;
    }

    void remove_assembly_instances(
        ItemRegistry&                       item_registry,
        BaseGroup&                          base_group,
        const UniqueID                      assembly_uid)
    {
        AssemblyInstanceContainer& assembly_instances = base_group.assembly_instances();

        // Collect the assembly instances to remove.
        const vector<UniqueID> remove_list =
            collect_assembly_instances(assembly_instances, assembly_uid);

        // Remove assembly instances and their corresponding project items.
        for (const_each<vector<UniqueID> > i = remove_list; i; ++i)
        {
            assembly_instances.remove(*i);
            delete item_registry.get_item(*i);
            item_registry.remove(*i);
        }

        // Recurse into child assemblies.
        for (each<AssemblyContainer> i = base_group.assemblies(); i; ++i)
            remove_assembly_instances(item_registry, *i, assembly_uid);
    }
}

void AssemblyItem::slot_delete()
{
    if (m_project_builder.get_rendering_manager().is_rendering())
        schedule_delete();
    else do_delete();
}

void AssemblyItem::schedule_delete()
{
    m_project_builder.get_rendering_manager().push_delayed_action(
        auto_ptr<RenderingManager::IDelayedAction>(
            new EntityDeletionDelayedAction<AssemblyItem>(this)));

    m_project_builder.get_rendering_manager().reinitialize_rendering();
}

void AssemblyItem::do_delete()
{
    if (!allows_deletion())
        return;

    const char* assembly_name = m_assembly.get_name();

    if (ask_assembly_deletion_confirmation(assembly_name) != QMessageBox::Yes)
        return;

    const UniqueID assembly_uid = m_assembly.get_uid();

    // Remove all assembly instances and their corresponding project items.
    remove_assembly_instances(
        m_project_builder.get_item_registry(),
        m_parent,
        assembly_uid);

    // Remove and delete the assembly.
    m_parent.assemblies().remove(assembly_uid);

    // Mark the project as modified.
    m_project_builder.notify_project_modification();

    // Remove and delete the assembly item.
    ItemBase* assembly_item = m_project_builder.get_item_registry().get_item(assembly_uid);
    m_project_builder.get_item_registry().remove(assembly_uid);
    delete assembly_item;

    // At this point 'this' no longer exists.
}

}   // namespace studio
}   // namespace appleseed
