
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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
#include "searchpathswindow.h"

// UI definition headers.
#include "ui_searchpathswindow.h"

// appleseed.studio headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/utility/searchpaths.h"

// Qt headers.
#include <QKeySequence>
#include <QLineEdit>
#include <QShortcut>
#include <QString>
#include <Qt>

using namespace renderer;

namespace appleseed {
namespace studio {

//
// SearchPathsWindow class implementation.
//

namespace
{
    QListWidgetItem* make_item(const QString& text = QString())
    {
        auto item = new QListWidgetItem(text);
        item->setFlags(item->flags() | Qt::ItemIsEditable);
        return item;
    }
}

SearchPathsWindow::SearchPathsWindow(
    const Project&  project,
    QWidget*        parent)
  : QWidget(parent)
  , m_ui(new Ui::SearchPathsWindow())
  , m_project(project)
  , m_edit_committed(false)
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    // Buttons.
    connect(m_ui->pushbutton_add, SIGNAL(clicked()), SLOT(slot_add()));
    connect(m_ui->pushbutton_remove, SIGNAL(clicked()), SLOT(slot_remove()));
    connect(m_ui->pushbutton_moveup, SIGNAL(clicked()), SLOT(slot_move_up()));
    connect(m_ui->pushbutton_movedown, SIGNAL(clicked()), SLOT(slot_move_down()));

    // List widget.
    connect(
        m_ui->listwidget_paths, SIGNAL(itemSelectionChanged()),
        SLOT(slot_path_selection_changed()));
    connect(
        m_ui->listwidget_paths->itemDelegate(), SIGNAL(commitData(QWidget*)),
        SLOT(slot_path_editor_committed(QWidget*)));
    connect(
        m_ui->listwidget_paths->itemDelegate(), SIGNAL(closeEditor(QWidget*)),
        SLOT(slot_path_editor_closed(QWidget*)));

    // Keyboard shortcuts.
    connect(create_window_local_shortcut(this, Qt::Key_Escape), SIGNAL(activated()), SLOT(reject()));
    connect(new QShortcut(QKeySequence(Qt::Key_Delete), this), SIGNAL(activated()), SLOT(slot_remove()));
    connect(new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Up), this), SIGNAL(activated()), SLOT(slot_move_up()));
    connect(new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Down), this), SIGNAL(activated()), SLOT(slot_move_down()));

    // Project root path.
    if (m_project.search_paths().has_root_path())
    {
        const auto root_path = m_project.search_paths().get_root_path();
        m_ui->lineedit_root_path->setText(root_path.c_str());
        m_ui->lineedit_root_path->setProperty("isSet", true);
    }
    else
    {
        m_ui->lineedit_root_path->setText("Not Set");
        m_ui->lineedit_root_path->setProperty("isSet", false);
    }

    // Populate list widget with search paths from project.
    load_search_paths();

    slot_path_selection_changed();
}

SearchPathsWindow::~SearchPathsWindow()
{
    delete m_ui;
}

void SearchPathsWindow::load_search_paths()
{
    const auto& search_paths = m_project.search_paths();

    m_ui->listwidget_paths->clear();

    for (size_t i = 0, e = search_paths.get_path_count(); i < e; ++i)
    {
        auto item = make_item(search_paths.get_path(i));

        // Environment search paths are grayed out and cannot be removed, edited or moved.
        if (i < search_paths.get_environment_path_count())
            item->setFlags(item->flags() & ~Qt::ItemIsEnabled);

        m_ui->listwidget_paths->addItem(item);
    }
}

void SearchPathsWindow::save_search_paths()
{
    auto& search_paths = m_project.search_paths();

    search_paths.clear_explicit_paths();

    for (int i = 0, e = m_ui->listwidget_paths->count(); i < e; ++i)
        search_paths.push_back_explicit_path(m_ui->listwidget_paths->item(i)->text().toStdString());

    emit signal_paths_modified();
}

void SearchPathsWindow::accept()
{
    save_search_paths();
    close();
}

void SearchPathsWindow::reject()
{
    load_search_paths();
    close();
}

void SearchPathsWindow::slot_add()
{
    auto item = make_item();
    m_ui->listwidget_paths->addItem(item);
    m_ui->listwidget_paths->setCurrentItem(item);

    m_ui->pushbutton_add->setEnabled(false);
    m_ui->pushbutton_remove->setEnabled(false);
    m_ui->pushbutton_moveup->setEnabled(false);
    m_ui->pushbutton_movedown->setEnabled(false);

    m_edit_committed = false;
    m_ui->listwidget_paths->editItem(item);
}

void SearchPathsWindow::slot_remove()
{
    for (auto selected_item : m_ui->listwidget_paths->selectedItems())
        delete selected_item;

    slot_path_selection_changed();
}

void SearchPathsWindow::slot_move_up()
{
    const auto index = m_ui->listwidget_paths->currentRow();

    const auto first_editable_path_index =
        static_cast<int>(m_project.search_paths().get_environment_path_count());

    if (index > first_editable_path_index)
    {
        auto item = m_ui->listwidget_paths->takeItem(index);
        m_ui->listwidget_paths->insertItem(index - 1, item);
        m_ui->listwidget_paths->setCurrentItem(item);
    }
}

void SearchPathsWindow::slot_move_down()
{
    const auto index = m_ui->listwidget_paths->currentRow();

    if (index < m_ui->listwidget_paths->count() - 1)
    {
        auto item = m_ui->listwidget_paths->takeItem(index);
        m_ui->listwidget_paths->insertItem(index + 1, item);
        m_ui->listwidget_paths->setCurrentItem(item);
    }
}

void SearchPathsWindow::slot_path_selection_changed()
{
    const auto index = m_ui->listwidget_paths->currentRow();

    const auto first_editable_path_index =
        static_cast<int>(m_project.search_paths().get_environment_path_count());

    m_ui->pushbutton_add->setEnabled(true);
    m_ui->pushbutton_remove->setEnabled(index != -1);
    m_ui->pushbutton_moveup->setEnabled(index > first_editable_path_index);
    m_ui->pushbutton_movedown->setEnabled(index != -1 && index < m_ui->listwidget_paths->count() - 1);
}

void SearchPathsWindow::slot_path_editor_committed(QWidget* widget)
{
    m_edit_committed = true;
}

void SearchPathsWindow::slot_path_editor_closed(QWidget* widget)
{
    const QString path = reinterpret_cast<QLineEdit*>(widget)->text();

    if (!m_edit_committed || path.isEmpty())
        delete m_ui->listwidget_paths->currentItem();

    slot_path_selection_changed();
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/project/moc_cpp_searchpathswindow.cxx"
