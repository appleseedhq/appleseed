
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
#include "mainwindow/project/itembase.h"

// Qt headers.
#include <QMenu>
#include <QPoint>
#include <QTreeWidget>

// Standard headers.
#include <cassert>

using namespace renderer;

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

}   // namespace studio
}   // namespace appleseed
