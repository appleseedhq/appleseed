
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "mainwindow/project/entitybrowser.h"
#include "mainwindow/project/entitybrowserwindow.h"
#include "mainwindow/project/projectbuilder.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QColor>
#include <QMenu>
#include <QMetaType>
#include <QString>
#include <Qt>
#include <QVariant>
#include <QWidget>

Q_DECLARE_METATYPE(QList<appleseed::studio::ItemBase*>);

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

ObjectInstanceItem::ObjectInstanceItem(
    Assembly&           assembly,
    ObjectInstance&     object_instance,
    ProjectBuilder&     project_builder)
  : ItemBase(object_instance.get_class_uid(), object_instance.get_name())
  , m_assembly(assembly)
  , m_object_instance(object_instance)
  , m_project_builder(project_builder)
{
    update_style();
}

QMenu* ObjectInstanceItem::get_single_item_context_menu() const
{
    QMenu* menu = ItemBase::get_single_item_context_menu();
    menu->addSeparator();

    menu->addAction("Assign Material...", this, SLOT(slot_assign_material()));
    menu->addAction("Unassign Material", this, SLOT(slot_unassign_material()));

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
    if (!are_in_assembly(items_to_object_instance_items(items), m_assembly.get_uid()))
        return 0;

    QMenu* menu = ItemBase::get_multiple_items_context_menu(items);
    menu->addSeparator();

    menu->addAction("Assign Material...", this, SLOT(slot_assign_material()))
        ->setData(QVariant::fromValue(items));
    menu->addAction("Unassign Material", this, SLOT(slot_unassign_material()))
        ->setData(QVariant::fromValue(items));

    return menu;
}

const Assembly& ObjectInstanceItem::get_assembly() const
{
    return m_assembly;
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
            emit accepted(page_name, item_value, m_data);
        }

      signals:
        void accepted(QString page_name, QString item_value, QVariant data);

      private:
        const QVariant m_data;
    };
}

void ObjectInstanceItem::slot_assign_material()
{
    QAction* action = static_cast<QAction*>(sender());

    const QString window_title =
        action->data().isNull()
            ? QString("Assign Material to %1").arg(m_object_instance.get_name())
            : QString("Assign Material to Multiple Object Instances");

    EntityBrowserWindow* browser_window =
        new EntityBrowserWindow(
            treeWidget(),
            window_title.toStdString());

    EntityBrowser<Assembly> entity_browser(m_assembly);

    browser_window->add_items_page(
        "material",
        "Materials",
        entity_browser.get_entities("material"));

    EnrichAndForwardAcceptedSignal* forwarder =
        new EnrichAndForwardAcceptedSignal(browser_window, action->data());

    QObject::connect(
        browser_window, SIGNAL(accepted(QString, QString)),
        forwarder, SLOT(slot_accepted(QString, QString)));

    QObject::connect(
        forwarder, SIGNAL(accepted(QString, QString, QVariant)),
        this, SLOT(slot_assign_material_accepted(QString, QString, QVariant)));

    browser_window->showNormal();
    browser_window->activateWindow();
}

void ObjectInstanceItem::slot_assign_material_accepted(QString page_name, QString entity_name, QVariant data)
{
    const size_t material_index = m_assembly.materials().get_index(entity_name.toAscii());
    assert(material_index != ~size_t(0));

    if (data.isNull())
        assign_material(material_index);
    else
    {
        const QList<ItemBase*> items = data.value<QList<ItemBase*> >();
        for (int i = 0; i < items.size(); ++i)
            static_cast<ObjectInstanceItem*>(items[i])->assign_material(material_index);
    }

    qobject_cast<QWidget*>(sender()->parent())->close();
}

void ObjectInstanceItem::slot_unassign_material()
{
    QAction* action = static_cast<QAction*>(sender());

    if (action->data().isNull())
        unassign_material();
    else
    {
        const QList<ItemBase*> items = action->data().value<QList<ItemBase*> >();
        for (int i = 0; i < items.size(); ++i)
            static_cast<ObjectInstanceItem*>(items[i])->unassign_material();
    }
}

void ObjectInstanceItem::update_style()
{
    if (m_object_instance.get_material_indices().empty())
        setTextColor(0, QColor(255, 0, 255, 255));
    else
    {
        // Remove the color overload. Not sure this is the easiest way to do it.
        setData(0, Qt::TextColorRole, QVariant());
    }
}

void ObjectInstanceItem::assign_material(const size_t material_index)
{
    m_object_instance.set_material_index(0, material_index);

    update_style();

    m_project_builder.notify_project_modification();
}

void ObjectInstanceItem::unassign_material()
{
    m_object_instance.set_material_indices(MaterialIndexArray());

    update_style();

    m_project_builder.notify_project_modification();
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_objectinstanceitem.cxx"
