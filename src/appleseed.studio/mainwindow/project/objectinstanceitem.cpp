
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
#include "mainwindow/project/assemblyentitybrowser.h"
#include "mainwindow/project/entitybrowserwindow.h"

// appleseed.renderer headers.
#include "renderer/api/scene.h"

// Qt headers.
#include <QColor>
#include <QMenu>
#include <QString>

using namespace renderer;

namespace appleseed {
namespace studio {

ObjectInstanceItem::ObjectInstanceItem(
    Assembly&       assembly,
    ObjectInstance& object_instance)
  : EntityItem(object_instance)
  , m_assembly(assembly)
  , m_object_instance(object_instance)
{
    if (object_instance.get_material_indices().empty())
        setTextColor(0, QColor(255, 0, 255, 255));
}

QMenu* ObjectInstanceItem::get_single_item_context_menu() const
{
    QMenu* menu = new QMenu(treeWidget());
    menu->addAction("Assign Material...", this, SLOT(slot_assign_material()));
    return menu;
}

void ObjectInstanceItem::slot_assign_material()
{
/*
    const QString window_title =
        items_data.size() == 1
            ? QString("Assign Material to %1").arg(first_object_instance.get_name())
            : QString("Assign Material to Multiple Object Instances");
*/

    const QString window_title =
        QString("Assign Material to %1").arg(m_object_instance.get_name());

    EntityBrowserWindow* browser_window =
        new EntityBrowserWindow(
            treeWidget(),
            window_title.toStdString());

    AssemblyEntityBrowser entity_browser(m_assembly);

    browser_window->add_items_page(
        "material",
        "Materials",
        entity_browser.get_entities("material"));

    QObject::connect(
        browser_window, SIGNAL(accepted(QString, QString)),
        this, SLOT(slot_assign_material_accepted(QString, QString)));

    browser_window->showNormal();
    browser_window->activateWindow();
}

void ObjectInstanceItem::slot_assign_material_accepted(QString page_name, QString entity_name)
{
    const size_t material_index = m_assembly.materials().get_index(entity_name.toAscii());
    assert(material_index != ~size_t(0));

    m_object_instance.set_material_index(0, material_index);

    qobject_cast<QWidget*>(sender())->close();
}

}   // namespace studio
}   // namespace appleseed
