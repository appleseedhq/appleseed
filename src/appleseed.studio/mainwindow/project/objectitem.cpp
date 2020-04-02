
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "objectitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblycollectionitem.h"
#include "mainwindow/project/assemblyitem.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/instancecollectionitem.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/objectcollectionitem.h"
#include "mainwindow/project/objectinstanceitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/object.h"
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QMenu>

// Standard headers.
#include <memory>
#include <vector>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

ObjectItem::ObjectItem(
    EntityEditorContext&    editor_context,
    Object*                 object,
    Assembly&               parent,
    AssemblyItem*           parent_item)
  : Base(editor_context, object)
  , m_parent(parent)
  , m_parent_item(parent_item)
{
    set_allow_edition(false);
}

QMenu* ObjectItem::get_single_item_context_menu() const
{
    QMenu* menu = Base::get_single_item_context_menu();

    menu->addSeparator();
    menu->addAction("Instantiate...", this, SLOT(slot_instantiate()));

    return menu;
}

void ObjectItem::slot_instantiate()
{
    const std::string instance_name_suggestion =
        make_unique_name(
            std::string(m_entity->get_name()) + "_inst",
            m_parent.object_instances());

    const std::string instance_name =
        get_entity_name_dialog(
            treeWidget(),
            "Instantiate Object",
            "Object Instance Name:",
            instance_name_suggestion);

    if (!instance_name.empty())
    {
        m_editor_context.m_rendering_manager.schedule_or_execute(
            std::unique_ptr<RenderingManager::IScheduledAction>(
                new EntityInstantiationAction<ObjectItem>(this, instance_name)));
    }
}

void ObjectItem::do_instantiate(const std::string& name)
{
    auto_release_ptr<ObjectInstance> object_instance(
        ObjectInstanceFactory::create(
            name.c_str(),
            ParamArray(),
            m_entity->get_name(),
            Transformd::identity(),
            StringDictionary()));

    m_parent_item->add_item(object_instance.get());
    m_parent.object_instances().insert(object_instance);

    m_parent.bump_version_id();
    m_editor_context.m_project_builder.slot_notify_project_modification();
}

void ObjectItem::delete_multiple(const QList<ItemBase*>& items)
{
    m_editor_context.m_rendering_manager.schedule_or_execute(
        std::unique_ptr<RenderingManager::IScheduledAction>(
            new EntityDeletionAction<ObjectItem>(
                qlist_static_cast<ObjectItem*>(items))));
}

namespace
{
    std::vector<UniqueID> collect_object_instances(
        const ObjectInstanceContainer&      object_instances,
        const UniqueID                      object_uid)
    {
        std::vector<UniqueID> collected;

        for (const_each<ObjectInstanceContainer> i = object_instances; i; ++i)
        {
            const Object* object = i->find_object();

            if (object && object->get_uid() == object_uid)
                collected.push_back(i->get_uid());
        }

        return collected;
    }

    void remove_object_instances(
        ItemRegistry&                       item_registry,
        Assembly&                           assembly,
        const UniqueID                      object_uid)
    {
        ObjectInstanceContainer& object_instances = assembly.object_instances();

        // Collect the object instances to remove.
        const std::vector<UniqueID> remove_list =
            collect_object_instances(object_instances, object_uid);

        // Remove object instances and their corresponding project items.
        for (const_each<std::vector<UniqueID>> i = remove_list; i; ++i)
        {
            object_instances.remove(object_instances.get_by_uid(*i));
            delete item_registry.get_item(*i);
        }

        if (!remove_list.empty())
            assembly.bump_version_id();

        // Recurse into child assemblies.
        for (each<AssemblyContainer> i = assembly.assemblies(); i; ++i)
            remove_object_instances(item_registry, *i, object_uid);
    }
}

void ObjectItem::do_delete()
{
    if (!allows_deletion())
        return;

    // Remove all object instances and their corresponding project items.
    remove_object_instances(
        m_editor_context.m_item_registry,
        m_parent,
        m_entity_uid);

    // Remove and delete the object.
    m_parent.objects().remove(m_parent.objects().get_by_uid(m_entity_uid));

    // Mark the project as modified.
    m_editor_context.m_project_builder.slot_notify_project_modification();

    // Remove and delete the object item.
    delete this;
}

}   // namespace studio
}   // namespace appleseed
