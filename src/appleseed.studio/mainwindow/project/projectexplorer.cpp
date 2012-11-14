
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
#include "projectexplorer.h"

// appleseed.studio headers.
#include "mainwindow/project/assemblycollectionitem.h"
#include "mainwindow/project/itembase.h"
#include "mainwindow/project/outputitem.h"
#include "mainwindow/project/sceneitem.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Qt headers.
#include <QKeySequence>
#include <QMenu>
#include <QPoint>
#include <Qt>
#include <QTreeWidget>

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

ProjectExplorer::ProjectExplorer(
    QTreeWidget*    tree_widget,
    Project&        project,
    ParamArray&     settings)
  : m_tree_widget(tree_widget)
  , m_project_builder(project)
{
    m_tree_widget->setContextMenuPolicy(Qt::CustomContextMenu);

    SceneItem* scene_item = new SceneItem(*project.get_scene(), m_project_builder, settings);
    m_tree_widget->addTopLevelItem(scene_item);
    scene_item->setExpanded(true);

    OutputItem* output_item = new OutputItem(project, m_project_builder);
    m_tree_widget->addTopLevelItem(output_item);
    output_item->setExpanded(true);

    connect(
        m_tree_widget, SIGNAL(customContextMenuRequested(const QPoint&)),
        this, SLOT(slot_context_menu(const QPoint&)));

    connect(
        m_tree_widget, SIGNAL(itemActivated(QTreeWidgetItem*, int)),
        this, SLOT(slot_edit_item(QTreeWidgetItem*, int)));

    m_delete_shortcut.reset(
        new QShortcut(QKeySequence(Qt::Key_Delete), m_tree_widget));

    connect(
        m_delete_shortcut.get(), SIGNAL(activated()),
        this, SLOT(slot_delete_item()));

    connect(
        &m_project_builder, SIGNAL(signal_project_modified()),
        this, SIGNAL(signal_project_modified()));

    connect(
        &m_project_builder, SIGNAL(signal_frame_modified()),
        this, SIGNAL(signal_frame_modified()));
}

QMenu* ProjectExplorer::build_single_item_context_menu(QTreeWidgetItem* item) const
{
    return static_cast<ItemBase*>(item)->get_single_item_context_menu();
}

namespace
{
    QList<ItemBase*> item_widgets_to_items(const QList<QTreeWidgetItem*>& item_widgets)
    {
        QList<ItemBase*> items;

        for (int i = 0; i < item_widgets.size(); ++i)
            items.append(static_cast<ItemBase*>(item_widgets[i]));

        return items;
    }

    bool are_same_class_uid(const QList<ItemBase*>& items)
    {
        assert(!items.empty());

        const UniqueID first_item_class_uid = items.first()->get_class_uid();

        for (int i = 1; i < items.size(); ++i)
        {
            if (items[i]->get_class_uid() != first_item_class_uid)
                return false;
        }

        return true;
    }
}

QMenu* ProjectExplorer::build_multiple_items_context_menu(const QList<QTreeWidgetItem*>& item_widgets) const
{
    assert(item_widgets.size() > 1);

    const QList<ItemBase*> items = item_widgets_to_items(item_widgets);

    return
        are_same_class_uid(items)
            ? items.first()->get_multiple_items_context_menu(items)
            : 0;
}

void ProjectExplorer::slot_context_menu(const QPoint& point)
{
    const QList<QTreeWidgetItem*> selected_items = m_tree_widget->selectedItems();

    if (selected_items.empty())
        return;

    QMenu* menu =
        selected_items.size() == 1
            ? build_single_item_context_menu(selected_items.first())
            : build_multiple_items_context_menu(selected_items);

    if (menu)
        menu->exec(m_tree_widget->mapToGlobal(point));
}

void ProjectExplorer::slot_edit_item(QTreeWidgetItem* item, int column)
{
    static_cast<ItemBase*>(item)->slot_edit();
}

void ProjectExplorer::slot_delete_item()
{
    if (!m_tree_widget->hasFocus())
        return;

    const QList<QTreeWidgetItem*> selected_items = m_tree_widget->selectedItems();

    if (selected_items.empty())
        return;

    const QList<ItemBase*> items = item_widgets_to_items(selected_items);

    if (!are_same_class_uid(items))
        return;

    items.first()->slot_delete_multiple(items);
}

}   // namespace studio
}   // namespace appleseed
