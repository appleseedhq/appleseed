
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
#include "projectexplorer.h"

// appleseed.studio headers.
#include "mainwindow/project/attributeeditor.h"
#include "mainwindow/project/itembase.h"
#include "mainwindow/project/projectitem.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// Qt headers.
#include <QDrag>
#include <QKeySequence>
#include <QMenu>
#include <QMimeData>
#include <QPoint>
#include <QRect>
#include <QRegExp>
#include <QScrollBar>
#include <QString>
#include <Qt>
#include <QTreeWidget>
#include <QTreeWidgetItem>

// Standard headers.
#include <cassert>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

ProjectExplorer::ProjectExplorer(
    QTreeWidget*        tree_widget,
    AttributeEditor*    attribute_editor,
    Project&            project,
    ProjectManager&     project_manager,
    RenderingManager&   rendering_manager,
    ParamArray&         settings)
  : m_tree_widget(tree_widget)
  , m_attribute_editor(attribute_editor)
  , m_project_builder(project)
  , m_editor_context(
        project,
        project_manager,
        *this,
        m_project_builder,
        m_item_registry,
        rendering_manager,
        settings)
{
    m_tree_widget->setContextMenuPolicy(Qt::CustomContextMenu);

    ProjectItem* project_item = new ProjectItem(m_editor_context);
    m_tree_widget->addTopLevelItem(project_item);

    project_item->expand();

    connect(
        m_tree_widget, SIGNAL(customContextMenuRequested(const QPoint&)),
        SLOT(slot_context_menu(const QPoint&)));

    connect(
        m_tree_widget, SIGNAL(itemSelectionChanged()),
        SLOT(slot_item_selection_changed()));

    connect(
        m_tree_widget, SIGNAL(itemActivated(QTreeWidgetItem*, int)),
        SLOT(slot_edit_item(QTreeWidgetItem*, int)));

    connect(
        m_tree_widget, SIGNAL(itemPressed(QTreeWidgetItem*, int)),
        SLOT(slot_drag_item(QTreeWidgetItem*, int)));

    m_delete_shortcut.reset(
        new QShortcut(QKeySequence(Qt::Key_Delete), m_tree_widget));

    connect(
        m_delete_shortcut.get(), SIGNAL(activated()),
        SLOT(slot_delete_items()));

    connect(
        &m_project_builder, SIGNAL(signal_project_modified()),
        SIGNAL(signal_project_modified()));

    connect(
        &m_project_builder, SIGNAL(signal_frame_modified()),
        SIGNAL(signal_frame_modified()));

    connect(
        &m_project_builder, SIGNAL(signal_post_processing_stage_modified(const std::uint64_t)),
        SIGNAL(signal_post_processing_stage_modified(const std::uint64_t)));
}

ProjectExplorer::~ProjectExplorer()
{
    m_tree_widget->clear();
}

namespace
{
    bool do_filter_items(QTreeWidgetItem* item, const QRegExp& regexp)
    {
        bool any_children_visible = false;

        for (int i = 0; i < item->childCount(); ++i)
        {
            if (do_filter_items(item->child(i), regexp))
                any_children_visible = true;
        }

        const bool visible = any_children_visible || regexp.indexIn(item->text(0)) >= 0;

        item->setHidden(!visible);

        return visible;
    }
}

void ProjectExplorer::filter_items(const QString& pattern) const
{
    const QRegExp regexp(pattern, Qt::CaseInsensitive);

    for (int i = 0; i < m_tree_widget->topLevelItemCount(); ++i)
        do_filter_items(m_tree_widget->topLevelItem(i), regexp);
}

void ProjectExplorer::clear_selection() const
{
    m_tree_widget->clearSelection();
}

namespace
{
    void expand_parents(QTreeWidgetItem* item)
    {
        while ((item = item->parent()))
            item->setExpanded(true);
    }
}

ItemBase* ProjectExplorer::select_entity(const UniqueID uid) const
{
    clear_selection();

    QTreeWidgetItem* item = m_item_registry.get_item(uid);

    if (item)
    {
        m_tree_widget->scrollToItem(item);

        const QRect r = m_tree_widget->visualItemRect(item);
        m_tree_widget->horizontalScrollBar()->setValue(r.x());

        item->setSelected(true);
        expand_parents(item);
    }

    return static_cast<ItemBase*>(item);
}

QMenu* ProjectExplorer::build_single_item_context_menu(QTreeWidgetItem* item) const
{
    return static_cast<ItemBase*>(item)->get_single_item_context_menu();
}

namespace
{
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

    const QList<ItemBase*> items = qlist_static_cast<ItemBase*>(item_widgets);

    return
        are_same_class_uid(items)
            ? items.first()->get_multiple_items_context_menu(items)
            : nullptr;
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

void ProjectExplorer::slot_item_selection_changed()
{
    m_attribute_editor->clear();

    const QList<QTreeWidgetItem*> selected_items = m_tree_widget->selectedItems();

    if (selected_items.size() == 1)
    {
        static_cast<ItemBase*>(selected_items.first())->slot_edit(m_attribute_editor);
        m_tree_widget->setFocus();
    }
}

void ProjectExplorer::slot_edit_item(QTreeWidgetItem* item, int column)
{
    static_cast<ItemBase*>(item)->slot_edit();
}

void ProjectExplorer::slot_drag_item(QTreeWidgetItem* item, int column)
{
    if (item && item->flags() & Qt::ItemIsDragEnabled)
    {
        QDrag* drag = new QDrag(m_tree_widget);
        QMimeData* mimeData = new QMimeData();

        mimeData->setText(item->text(column));
        drag->setMimeData(mimeData);

        drag->exec();
    }
}

void ProjectExplorer::slot_delete_items()
{
    if (!m_tree_widget->hasFocus())
        return;

    const QList<QTreeWidgetItem*> selected_items = m_tree_widget->selectedItems();

    if (selected_items.empty())
        return;

    const QList<ItemBase*> items = qlist_static_cast<ItemBase*>(selected_items);

    if (!are_same_class_uid(items))
        return;

    items.first()->delete_multiple(items);
}

}   // namespace studio
}   // namespace appleseed
