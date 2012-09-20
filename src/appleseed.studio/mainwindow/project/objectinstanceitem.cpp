
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "mainwindow/project/projectbuilder.h"

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
#include <string>

using namespace appleseed::studio;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace
{
    enum Side
    {
        FrontSide,
        BackSide,
        FrontAndBackSides
    };

    struct MaterialAssignmentData
    {
        Side                m_side;
        QList<ItemBase*>    m_items;

        MaterialAssignmentData()
        {
        }

        MaterialAssignmentData(
            const Side              side,
            const QList<ItemBase*>& items)
          : m_side(side)
          , m_items(items)
        {
        }
    };
}

Q_DECLARE_METATYPE(MaterialAssignmentData);

namespace appleseed {
namespace studio {

ObjectInstanceItem::ObjectInstanceItem(
    ObjectInstance*                 object_instance,
    Assembly&                       parent,
    ObjectInstanceCollectionItem*   collection_item,
    ProjectBuilder&                 project_builder)
  : Base(object_instance, parent, collection_item, project_builder)
{
    update_style();
}

const Assembly& ObjectInstanceItem::get_assembly() const
{
    return m_parent;
}

namespace
{
    void add_material_assignment_menu_actions(
        QMenu*                      menu,
        const ObjectInstanceItem*   item,
        const QList<ItemBase*>&     items = QList<ItemBase*>())
    {
        menu->addSeparator();

        menu->addAction("Assign Material To Front Side...", item, SLOT(slot_assign_material()))
            ->setData(QVariant::fromValue(MaterialAssignmentData(FrontSide, items)));

        menu->addAction("Assign Material To Back Side...", item, SLOT(slot_assign_material()))
            ->setData(QVariant::fromValue(MaterialAssignmentData(BackSide, items)));

        menu->addAction("Assign Material To Both Sides...", item, SLOT(slot_assign_material()))
            ->setData(QVariant::fromValue(MaterialAssignmentData(FrontAndBackSides, items)));

        menu->addSeparator();

        menu->addAction("Unassign Front Side Material", item, SLOT(slot_unassign_material()))
            ->setData(QVariant::fromValue(MaterialAssignmentData(FrontSide, items)));

        menu->addAction("Unassign Back Side Material", item, SLOT(slot_unassign_material()))
            ->setData(QVariant::fromValue(MaterialAssignmentData(BackSide, items)));

        menu->addAction("Unassign Both Sides Materials", item, SLOT(slot_unassign_material()))
            ->setData(QVariant::fromValue(MaterialAssignmentData(FrontAndBackSides, items)));
    }
}

QMenu* ObjectInstanceItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();

    add_material_assignment_menu_actions(menu, this);

    return menu;
}

namespace
{
    QList<ObjectInstanceItem*> items_to_object_instance_items(const QList<ItemBase*>& items)
    {
        QList<ObjectInstanceItem*> object_instance_items;

        for (int i = 0; i < items.size(); ++i)
            object_instance_items.append(static_cast<ObjectInstanceItem*>(items[i]));

        return object_instance_items;
    }

    bool are_in_assembly(
        const QList<ObjectInstanceItem*>&   object_instance_items,
        const UniqueID                      assembly_uid)
    {
        for (int i = 0; i < object_instance_items.size(); ++i)
        {
            if (object_instance_items[i]->get_assembly().get_uid() != assembly_uid)
                return false;
        }

        return true;
    }
}

QMenu* ObjectInstanceItem::get_multiple_items_context_menu(const QList<ItemBase*>& items) const
{
    if (!are_in_assembly(items_to_object_instance_items(items), m_parent.get_uid()))
        return 0;

    QMenu* menu = ItemBase::get_multiple_items_context_menu(items);

    add_material_assignment_menu_actions(menu, this, items);

    return menu;
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
    QAction* action = static_cast<QAction*>(sender());

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

    QObject::connect(
        browser_window, SIGNAL(signal_accepted(QString, QString)),
        forwarder, SLOT(slot_accepted(QString, QString)));

    QObject::connect(
        forwarder, SIGNAL(signal_accepted(QString, QString, QVariant)),
        this, SLOT(slot_assign_material_accepted(QString, QString, QVariant)));

    browser_window->showNormal();
    browser_window->activateWindow();
}

void ObjectInstanceItem::slot_assign_material_accepted(QString page_name, QString entity_name, QVariant untyped_data)
{
    const string material_name = entity_name.toStdString();

    const MaterialAssignmentData data = untyped_data.value<MaterialAssignmentData>();
    const bool front_side = data.m_side == FrontSide || data.m_side == FrontAndBackSides;
    const bool back_side = data.m_side == BackSide || data.m_side == FrontAndBackSides;

    if (data.m_items.empty())
    {
        assign_material(front_side, back_side, material_name.c_str());
    }
    else
    {
        for (int i = 0; i < data.m_items.size(); ++i)
        {
            ObjectInstanceItem* item = static_cast<ObjectInstanceItem*>(data.m_items[i]);
            item->assign_material(front_side, back_side, material_name.c_str());
        }
    }

    qobject_cast<QWidget*>(sender()->parent())->close();
}

void ObjectInstanceItem::slot_unassign_material()
{
    QAction* action = static_cast<QAction*>(sender());

    const MaterialAssignmentData data = action->data().value<MaterialAssignmentData>();
    const bool front_side = data.m_side == FrontSide || data.m_side == FrontAndBackSides;
    const bool back_side = data.m_side == BackSide || data.m_side == FrontAndBackSides;

    if (data.m_items.empty())
    {
        unassign_material(front_side, back_side);
    }
    else
    {
        for (int i = 0; i < data.m_items.size(); ++i)
        {
            ObjectInstanceItem* item = static_cast<ObjectInstanceItem*>(data.m_items[i]);
            item->unassign_material(front_side, back_side);
        }
    }
}

void ObjectInstanceItem::slot_delete()
{
    if (!allows_deletion())
        return;

    const UniqueID object_instance_uid = m_entity->get_uid();

    // Remove and delete the object instance.
    m_parent.object_instances().remove(
        m_parent.object_instances().get_by_uid(object_instance_uid));

    // Mark the assembly and the project as modified.
    m_parent.bump_version_id();
    m_project_builder.notify_project_modification();

    // Remove and delete the object instance item.
    m_collection_item->delete_item(object_instance_uid);

    // At this point 'this' no longer exists.
}

void ObjectInstanceItem::assign_material(const bool front_side, const bool back_side, const char* material_name)
{
    if (front_side)
        m_entity->assign_material("0", ObjectInstance::FrontSide, material_name);

    if (back_side)
        m_entity->assign_material("0", ObjectInstance::BackSide, material_name);

    m_project_builder.notify_project_modification();

    update_style();
}

void ObjectInstanceItem::unassign_material(const bool front_side, const bool back_side)
{
    if (front_side)
        m_entity->clear_front_materials();

    if (back_side)
        m_entity->clear_back_materials();

    m_project_builder.notify_project_modification();

    update_style();
}

void ObjectInstanceItem::update_style()
{
    if (m_entity->get_front_material_mappings().empty() &&
        m_entity->get_back_material_mappings().empty())
    {
        setTextColor(0, QColor(255, 0, 255, 255));
    }
    else
    {
        // Remove the color overload. Not sure this is the easiest way to do it.
        setData(0, Qt::TextColorRole, QVariant());
    }
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_objectinstanceitem.cxx"
