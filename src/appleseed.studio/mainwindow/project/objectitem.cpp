
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
#include "objectitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblycollectionitem.h"
#include "mainwindow/project/assemblyitem.h"
#include "mainwindow/project/instancecollectionitem.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/objectcollectionitem.h"
#include "mainwindow/project/objectinstanceitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.renderer headers.
#include "renderer/api/object.h"
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QMenu>
#include <QString>

// Standard headers.
#include <memory>
#include <vector>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

ObjectItem::ObjectItem(
    Object*         object,
    Assembly&       parent,
    AssemblyItem*   parent_item,
    ProjectBuilder& project_builder)
  : EntityItemBase<Object>(object)
  , m_parent(parent)
  , m_parent_item(parent_item)
  , m_project_builder(project_builder)
{
    set_allow_edition(false);
}

namespace
{
    vector<UniqueID> collect_object_instances(
        const ObjectInstanceContainer&      object_instances,
        const UniqueID                      object_uid)
    {
        vector<UniqueID> collected;

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
        const vector<UniqueID> remove_list =
            collect_object_instances(object_instances, object_uid);

        // Remove object instances and their corresponding project items.
        for (const_each<vector<UniqueID> > i = remove_list; i; ++i)
        {
            object_instances.remove(object_instances.get_by_uid(*i));
            delete item_registry.get_item(*i);
            item_registry.remove(*i);
        }

        if (!remove_list.empty())
            assembly.bump_version_id();

        // Recurse into child assemblies.
        for (each<AssemblyContainer> i = assembly.assemblies(); i; ++i)
            remove_object_instances(item_registry, *i, object_uid);
    }
}

QMenu* ObjectItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();

    menu->addSeparator();
    menu->addAction("Instantiate...", this, SLOT(slot_instantiate()));

    return menu;
}

void ObjectItem::slot_instantiate()
{
    // Code goes here
	const string instance_name_suggestion =
		get_name_suggestion(
			string(m_entity->get_name()) + "_inst",
			m_parent.object_instances());

	const string instance_name =
		get_entity_name_dialog(
			treeWidget(),
			"Instantiate Object",
			"Object Instance Name:",
			instance_name_suggestion);

    if (!instance_name.empty())
    {
        auto_release_ptr<ObjectInstance> object_instance(
            ObjectInstanceFactory::create(
                instance_name.c_str(),
                ParamArray(),
                m_entity->get_name(),
                Transformd::identity(),
                StringDictionary()));

        m_parent_item->add_item(object_instance.get());
        m_parent.object_instances().insert(object_instance);

        m_parent.bump_version_id();
        m_project_builder.notify_project_modification();
    }
}

void ObjectItem::slot_delete()
{
    if (m_project_builder.get_rendering_manager().is_rendering())
        schedule_delete();
    else do_delete();
}

void ObjectItem::schedule_delete()
{
    m_project_builder.get_rendering_manager().push_delayed_action(
        auto_ptr<RenderingManager::IDelayedAction>(
            new EntityDeletionDelayedAction<ObjectItem>(this)));

    m_project_builder.get_rendering_manager().reinitialize_rendering();
}

void ObjectItem::do_delete()
{
    if (!allows_deletion())
        return;

    const UniqueID object_uid = m_entity->get_uid();

    // Remove all object instances and their corresponding project items.
    remove_object_instances(
        m_project_builder.get_item_registry(),
        m_parent,
        object_uid);

    // Remove and delete the object.
    m_parent.objects().remove(m_parent.objects().get_by_uid(object_uid));

    // Mark the project as modified.
    m_project_builder.notify_project_modification();

    // Remove and delete the object item.
    ItemBase* object_item = m_project_builder.get_item_registry().get_item(object_uid);
    m_project_builder.get_item_registry().remove(object_uid);
    delete object_item;

    // At this point 'this' no longer exists.
}

}   // namespace studio
}   // namespace appleseed
