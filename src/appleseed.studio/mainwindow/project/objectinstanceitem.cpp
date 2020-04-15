
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
#include "objectinstanceitem.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblyitem.h"
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entitybrowserwindow.h"
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/materialassignmenteditorwindow.h"
#include "mainwindow/project/materialcollectionitem.h"
#include "mainwindow/project/projectbuilder.h"
#include "mainwindow/project/projectexplorer.h"
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QAction>
#include <QColor>
#include <QMenu>
#include <QMetaType>
#include <QString>
#include <Qt>
#include <QVariant>
#include <QWidget>

// Standard headers.
#include <memory>
#include <string>
#include <utility>

using namespace appleseed::qtcommon;
using namespace appleseed::studio;
using namespace foundation;
using namespace renderer;

namespace
{
    struct MaterialAssignmentData
    {
        std::string                 m_slot;
        int                         m_sides;
        QList<ItemBase*>            m_items;

        MaterialAssignmentData()
        {
        }

        MaterialAssignmentData(
            const char*             slot,
            const int               sides,
            const QList<ItemBase*>& items)
          : m_slot(slot)
          , m_sides(sides)
          , m_items(items)
        {
        }
    };
}

Q_DECLARE_METATYPE(MaterialAssignmentData);

namespace appleseed {
namespace studio {

const char* ObjectInstanceItem::DefaultSlotName = "default";

ObjectInstanceItem::ObjectInstanceItem(
    EntityEditorContext&            editor_context,
    ObjectInstance*                 object_instance,
    Assembly&                       parent,
    ObjectInstanceCollectionItem*   collection_item)
  : Base(editor_context, object_instance, parent, collection_item)
{
    update_style();
}

const Assembly& ObjectInstanceItem::get_assembly() const
{
    return m_parent;
}

QMenu* ObjectInstanceItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();

    menu->addSeparator();

    menu->addAction("Assign Materials...", this, SLOT(slot_open_material_assignment_editor()));

    add_material_assignment_menu_actions(menu);

    return menu;
}

namespace
{
    bool are_in_same_assembly(
        const QList<ItemBase*>& items,
        const UniqueID          assembly_uid)
    {
        for (int i = 0; i < items.size(); ++i)
        {
            const ObjectInstanceItem* object_instance_item = static_cast<ObjectInstanceItem*>(items[i]);

            if (object_instance_item->get_assembly().get_uid() != assembly_uid)
                return false;
        }

        return true;
    }
}

QMenu* ObjectInstanceItem::get_multiple_items_context_menu(const QList<ItemBase*>& items) const
{
    if (!are_in_same_assembly(items, m_parent.get_uid()))
        return nullptr;

    QMenu* menu = ItemBase::get_multiple_items_context_menu(items);

    menu->addSeparator();

    add_material_assignment_menu_actions(menu, items);

    return menu;
}

void ObjectInstanceItem::slot_open_material_assignment_editor()
{
    MaterialAssignmentEditorWindow* editor_window =
        new MaterialAssignmentEditorWindow(
            *m_entity,
            *this,
            m_editor_context,
            QTreeWidgetItem::treeWidget());

    editor_window->showNormal();
    editor_window->activateWindow();
}

namespace
{
    class EnrichAndForwardAcceptedSignal
      : public QObject
    {
        Q_OBJECT

      public:
        EnrichAndForwardAcceptedSignal(QObject* parent, const QVariant& data)
          : QObject(parent)
          , m_data(data)
        {
        }

      public slots:
        void slot_accepted(QString page_name, QString item_value)
        {
            emit signal_accepted(page_name, item_value, m_data);
        }

      signals:
        void signal_accepted(QString page_name, QString item_value, QVariant data);

      private:
        const QVariant m_data;
    };
}

void ObjectInstanceItem::slot_assign_material()
{
    QAction* action = qobject_cast<QAction*>(sender());

    const MaterialAssignmentData data = action->data().value<MaterialAssignmentData>();

    const QString window_title =
        data.m_items.empty()
            ? QString("Assign Material to %1").arg(m_entity->get_name())
            : QString("Assign Material to Multiple Object Instances");

    EntityBrowserWindow* browser_window =
        new EntityBrowserWindow(
            treeWidget(),
            window_title.toStdString());

    EntityBrowser<Assembly> entity_browser(m_parent);

    browser_window->add_items_page(
        "material",
        "Materials",
        entity_browser.get_entities("material"));

    EnrichAndForwardAcceptedSignal* forwarder =
        new EnrichAndForwardAcceptedSignal(browser_window, action->data());

    connect(
        browser_window, SIGNAL(signal_accepted(QString, QString)),
        forwarder, SLOT(slot_accepted(QString, QString)));

    connect(
        forwarder, SIGNAL(signal_accepted(QString, QString, QVariant)),
        this, SLOT(slot_assign_material_accepted(QString, QString, QVariant)));

    browser_window->showNormal();
    browser_window->activateWindow();
}

namespace
{
    class AssignMaterialAction
      : public RenderingManager::IScheduledAction
    {
      public:
        AssignMaterialAction(
            ObjectInstanceItem*     parent,
            const QString&          page_name,
            const QString&          entity_name,
            const QVariant&         data)
          : m_parent(parent)
          , m_page_name(page_name)
          , m_entity_name(entity_name)
          , m_data(data)
        {
        }

        void operator()(
            Project&                project) override
        {
            m_parent->assign_material(m_page_name, m_entity_name, m_data);
        }

      private:
        ObjectInstanceItem*         m_parent;
        const QString               m_page_name;
        const QString               m_entity_name;
        const QVariant              m_data;
    };
}

void ObjectInstanceItem::slot_assign_material_accepted(QString page_name, QString entity_name, QVariant data)
{
    m_editor_context.m_rendering_manager.schedule_or_execute(
        std::unique_ptr<RenderingManager::IScheduledAction>(
            new AssignMaterialAction(this, page_name, entity_name, data)));

    qobject_cast<QWidget*>(sender()->parent())->close();
}

void ObjectInstanceItem::assign_material(
    const QString&                  page_name,
    const QString&                  entity_name,
    const QVariant&                 untyped_data)
{
    const std::string material_name = entity_name.toStdString();
    const MaterialAssignmentData data = untyped_data.value<MaterialAssignmentData>();

    if (data.m_items.empty())
        do_assign_material(data.m_slot.c_str(), data.m_sides, material_name.c_str());
    else
    {
        for (int i = 0; i < data.m_items.size(); ++i)
        {
            ObjectInstanceItem* item = static_cast<ObjectInstanceItem*>(data.m_items[i]);
            item->do_assign_material(data.m_slot.c_str(), data.m_sides, material_name.c_str());
        }
    }
}

namespace
{
    class ClearMaterialAction
      : public RenderingManager::IScheduledAction
    {
      public:
        ClearMaterialAction(
            ObjectInstanceItem*     parent,
            const QVariant&         data)
          : m_parent(parent)
          , m_data(data)
        {
        }

        void operator()(
            Project&                project) override
        {
            m_parent->clear_material(m_data);
        }

      private:
        ObjectInstanceItem*         m_parent;
        const QVariant              m_data;
    };
}

void ObjectInstanceItem::slot_clear_material()
{
    const QVariant data = qobject_cast<QAction*>(sender())->data();

    m_editor_context.m_rendering_manager.schedule_or_execute(
        std::unique_ptr<RenderingManager::IScheduledAction>(
            new ClearMaterialAction(this, data)));
}

void ObjectInstanceItem::clear_material(const QVariant& untyped_data)
{
    const MaterialAssignmentData data = untyped_data.value<MaterialAssignmentData>();

    if (data.m_items.empty())
        do_unassign_material(data.m_slot.c_str(), data.m_sides);
    else
    {
        for (int i = 0; i < data.m_items.size(); ++i)
        {
            ObjectInstanceItem* item = static_cast<ObjectInstanceItem*>(data.m_items[i]);
            item->do_unassign_material(data.m_slot.c_str(), data.m_sides);
        }
    }
}

void ObjectInstanceItem::delete_multiple(const QList<ItemBase*>& items)
{
    m_editor_context.m_rendering_manager.schedule_or_execute(
        std::unique_ptr<RenderingManager::IScheduledAction>(
            new EntityDeletionAction<ObjectInstanceItem>(
                qlist_static_cast<ObjectInstanceItem*>(items))));
}

void ObjectInstanceItem::do_delete()
{
    if (!allows_deletion())
        return;

    // Remove and delete the object instance.
    m_parent.object_instances().remove(
        m_parent.object_instances().get_by_uid(m_entity_uid));

    // Mark the assembly and the project as modified.
    m_parent.bump_version_id();
    m_editor_context.m_project_builder.slot_notify_project_modification();

    // Remove and delete the object instance item.
    delete this;
}

void ObjectInstanceItem::add_material_assignment_menu_actions(
    QMenu*                          menu,
    const QList<ItemBase*>&         items) const
{
    Object* object = m_entity->find_object();

    if (!object)
        return;

    QMenu* slots_menu = menu->addMenu("Material Slots");

    const size_t slot_count = object->get_material_slot_count();

    if (slot_count > 0)
    {
        for (size_t i = 0; i < slot_count; ++i)
        {
            const char* slot = object->get_material_slot(i);
            QMenu* slot_menu = slots_menu->addMenu(slot);
            add_material_assignment_menu_actions(slot_menu, slot, items);
        }
    }
    else
    {
        QMenu* slot_menu = slots_menu->addMenu(DefaultSlotName);
        add_material_assignment_menu_actions(slot_menu, DefaultSlotName, items);
    }
}

void ObjectInstanceItem::add_material_assignment_menu_actions(
    QMenu*                          menu,
    const char*                     slot,
    const QList<ItemBase*>&         items) const
{
    menu->addAction("Assign Material To Front Side...", this, SLOT(slot_assign_material()))
        ->setData(QVariant::fromValue(MaterialAssignmentData(slot, ObjectInstance::FrontSide, items)));

    menu->addAction("Assign Material To Back Side...", this, SLOT(slot_assign_material()))
        ->setData(QVariant::fromValue(MaterialAssignmentData(slot, ObjectInstance::BackSide, items)));

    menu->addAction("Assign Material To Both Sides...", this, SLOT(slot_assign_material()))
        ->setData(QVariant::fromValue(MaterialAssignmentData(slot, ObjectInstance::BothSides, items)));

    menu->addSeparator();

    menu->addAction("Clear Front Side Material", this, SLOT(slot_clear_material()))
        ->setData(QVariant::fromValue(MaterialAssignmentData(slot, ObjectInstance::FrontSide, items)));

    menu->addAction("Clear Back Side Material", this, SLOT(slot_clear_material()))
        ->setData(QVariant::fromValue(MaterialAssignmentData(slot, ObjectInstance::BackSide, items)));

    menu->addAction("Clear Both Sides Materials", this, SLOT(slot_clear_material()))
        ->setData(QVariant::fromValue(MaterialAssignmentData(slot, ObjectInstance::BothSides, items)));
}

void ObjectInstanceItem::do_assign_material(
    const char*                     slot_name,
    const int                       sides,
    const char*                     material_name)
{
    if (sides & ObjectInstance::FrontSide)
        m_entity->assign_material(slot_name, ObjectInstance::FrontSide, material_name);

    if (sides & ObjectInstance::BackSide)
        m_entity->assign_material(slot_name, ObjectInstance::BackSide, material_name);

    m_editor_context.m_project_builder.slot_notify_project_modification();

    update_style();
}

void ObjectInstanceItem::do_unassign_material(
    const char*                     slot_name,
    const int                       sides)
{
    if (sides & ObjectInstance::FrontSide)
        m_entity->unassign_material(slot_name, ObjectInstance::FrontSide);

    if (sides & ObjectInstance::BackSide)
        m_entity->unassign_material(slot_name, ObjectInstance::BackSide);

    m_editor_context.m_project_builder.slot_notify_project_modification();

    update_style();
}

void ObjectInstanceItem::update_style()
{
    if (m_entity->get_front_material_mappings().empty() &&
        m_entity->get_back_material_mappings().empty())
    {
        setForeground(0, QColor(255, 0, 255, 255));
    }
    else
    {
        // Remove the color overload. Not sure this is the easiest way to do it.
        setData(0, Qt::ForegroundRole, QVariant());
    }
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_objectinstanceitem.cxx"
