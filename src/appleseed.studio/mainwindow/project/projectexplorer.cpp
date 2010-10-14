
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
#include "projectexplorer.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblycollectionitem.h"
#include "mainwindow/project/entitybrowserwindow.h"
#include "mainwindow/project/entityeditorwindow.h"
#include "mainwindow/project/itembase.h"
#include "mainwindow/project/tools.h"
#include "utility/tweaks.h"

// appleseed.renderer headers.
#include "renderer/api/color.h"
#include "renderer/api/edf.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/environmentshader.h"
#include "renderer/api/geometry.h"
#include "renderer/api/light.h"
#include "renderer/api/material.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/texture.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionaryarray.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/kvpair.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"

// Qt headers.
#include <QFileDialog>
#include <QInputDialog>
#include <QMenu>
#include <QMessageBox>
#include <QPoint>
#include <QStringList>
#include <Qt>
#include <QTreeWidget>

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <memory>
#include <string>

using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

ProjectExplorer::ProjectExplorer(
    Project&        project,
    QTreeWidget*    tree_widget)
  : m_project(project)
  , m_tree_widget(tree_widget)
  , m_project_tree(project, tree_widget)
{
    m_tree_widget->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(
        m_tree_widget, SIGNAL(customContextMenuRequested(const QPoint&)),
        this, SLOT(slot_context_menu(const QPoint&)));
}

namespace
{
    bool are_items_same_type(const QList<QTreeWidgetItem*>& items)
    {
        assert(!items.empty());

/*
        const ProjectItem::Type first_item_type = get_item_type(items[0]);

        for (int i = 1; i < items.size(); ++i)
        {
            if (get_item_type(items[i]) != first_item_type)
                return false;
        }
*/

        return true;
    }

/*
    bool are_items_from_same_assembly(const QList<QTreeWidgetItem*>& items)
    {
        assert(!items.empty());

        const Assembly* first_item_assembly = get_assembly_from_item(items[0]);

        for (int i = 1; i < items.size(); ++i)
        {
            if (get_assembly_from_item(items[i]) != first_item_assembly)
                return false;
        }

        return true;
    }
*/
}

QMenu* ProjectExplorer::build_context_menu(const QList<QTreeWidgetItem*>& items) const
{
    assert(!items.isEmpty());

    if (items.size() == 1)
    {
        const ItemBase* item = static_cast<ItemBase*>(items.first());
        return item->get_context_menu();
    }
    else if (are_items_same_type(items))
    {
/*
        switch (get_item_type(items.first()))
        {
          case ProjectItem::ItemObjectInstance:
            if (are_items_from_same_assembly(items))
                menu = build_object_instance_context_menu();
            break;
        }
*/
    }

    return 0;
}

QMenu* ProjectExplorer::build_generic_context_menu() const
{
    QMenu* menu = new QMenu(m_tree_widget);
    menu->addAction(
        "Create Assembly...",
        &m_project_tree.get_assembly_collection_item(),
        SLOT(slot_create_assembly()));
    return menu;
}

void ProjectExplorer::slot_context_menu(const QPoint& point)
{
    const QList<QTreeWidgetItem*> selected_items = m_tree_widget->selectedItems();

    QMenu* menu =
        selected_items.isEmpty()
            ? build_generic_context_menu()
            : build_context_menu(selected_items);

    if (menu)
        menu->exec(m_tree_widget->mapToGlobal(point));
}

/*
namespace
{
    const KeyValuePair<ProjectItem::Type, const char*> EntityNames[] =
    {
        { ProjectItem::ItemBSDF, "BSDF" },
        { ProjectItem::ItemMaterial, "material" }
    };

    void display_entity_creation_error(
        const ProjectItem::Type item_type,
        const QString&          message)
    {
        QMessageBox msgbox;
        msgbox.setWindowTitle(
            QString("Failed to create %1").arg(
                LOOKUP_KVPAIR_ARRAY(EntityNames, item_type)->m_value));
        msgbox.setIcon(QMessageBox::Warning);
        msgbox.setText(message);
        msgbox.setStandardButtons(QMessageBox::Ok);
        msgbox.setDefaultButton(QMessageBox::Ok);
        set_minimum_width(msgbox, 300);
        msgbox.exec();
    }
}

void ProjectExplorer::slot_create_entity(QVariant payload, Dictionary values)
{
    const ItemTypeQVariantPair item = payload.value<ItemTypeQVariantPair>();
    const ProjectItem::Type item_type = item.first;
    Assembly& assembly = qvariant_to_ref<Assembly>(item.second.toList().first());

    try
    {
        switch (item_type)
        {
          case ProjectItem::ItemBSDF:
            m_tree_widget_decorator.insert_assembly_item(
                assembly,
                m_project_builder.insert_bsdf(assembly, values));
            break;

          case ProjectItem::ItemMaterial:
            m_tree_widget_decorator.insert_assembly_item(
                assembly,
                m_project_builder.insert_material(assembly, values));
            break;

          case ProjectItem::ItemSurfaceShader:
            m_tree_widget_decorator.insert_assembly_item(
                assembly,
                m_project_builder.insert_surface_shader(assembly, values));
            break;

          assert_otherwise;
        }

        // Close the entity editor.
        qobject_cast<QWidget*>(sender()->parent())->close();

        emit project_modified();
    }
    catch (const ExceptionDictionaryItemNotFound& e)
    {
        display_entity_creation_error(
            item_type,
            QString("Required parameter \"%0\" missing.").arg(e.string()));
    }
    catch (const ExceptionUnknownEntity& e)
    {
        display_entity_creation_error(
            item_type,
            QString("Unknown entity \"%0\".").arg(e.string()));
    }
}
*/

}   // namespace studio
}   // namespace appleseed
