
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

// appleseed.qtcommon headers.
#include "project/projectmanager.h"
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/searchpaths.h"

// Qt headers.
#include <QKeySequence>
#include <QLineEdit>
#include <QListWidget>
#include <QListWidgetItem>
#include <QMetaType>
#include <QPushButton>
#include <QShortcut>
#include <QString>
#include <Qt>

// Standard headers.
#include <cstddef>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace
{
    enum class SearchPathType { Environment, Explicit };
}

Q_DECLARE_METATYPE(SearchPathType);

namespace appleseed {
namespace studio {

//
// SearchPathsWindow class implementation.
//

namespace
{
    QListWidgetItem* make_item(const SearchPathType search_path_type, const QString& text = QString())
    {
        QListWidgetItem* item = new QListWidgetItem(text);

        item->setData(Qt::UserRole, QVariant::fromValue(search_path_type));
        item->setFlags(item->flags() | Qt::ItemIsEditable);

        if (search_path_type == SearchPathType::Environment)
            item->setFlags(item->flags() & ~Qt::ItemIsEnabled);

        return item;
    }
}

SearchPathsWindow::SearchPathsWindow(
    const Project&  project,
    ProjectManager& project_manager,
    QWidget*        parent)
  : WindowBase(parent, "search_paths_window")
  , m_ui(new Ui::SearchPathsWindow())
  , m_project(project)
  , m_edit_committed(false)
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    connect(&project_manager, SIGNAL(signal_project_path_changed(const QString&)), SLOT(slot_update_root_path()));

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
    connect(new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Return), this), SIGNAL(activated()), SLOT(accept()));
    connect(new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Enter), this), SIGNAL(activated()), SLOT(accept()));
    
    WindowBase::load_settings();

    slot_update_root_path();
    load_search_paths();

    slot_path_selection_changed();
}

SearchPathsWindow::~SearchPathsWindow()
{
    delete m_ui;
}

void SearchPathsWindow::slot_update_root_path()
{
    if (m_project.search_paths().has_root_path())
    {
        const APIString root_path = m_project.search_paths().get_root_path();
        m_ui->lineedit_root_path->setText(root_path.c_str());
        m_ui->lineedit_root_path->setProperty("isSet", true);
    }
    else
    {
        m_ui->lineedit_root_path->setText("Not Set");
        m_ui->lineedit_root_path->setProperty("isSet", false);
    }
}

void SearchPathsWindow::load_search_paths()
{
    m_ui->listwidget_paths->clear();

    const SearchPaths& search_paths = m_project.search_paths();

    for (size_t i = 0, e = search_paths.get_environment_path_count(); i < e; ++i)
    {
        m_ui->listwidget_paths->addItem(
            make_item(SearchPathType::Environment, search_paths.get_environment_path(i)));
    }

    for (size_t i = 0, e = search_paths.get_explicit_path_count(); i < e; ++i)
    {
        m_ui->listwidget_paths->addItem(
            make_item(SearchPathType::Explicit, search_paths.get_explicit_path(i)));
    }
}

void SearchPathsWindow::save_search_paths()
{
    SearchPaths& search_paths = m_project.search_paths();

    search_paths.clear_explicit_paths();

    for (int i = 0, e = m_ui->listwidget_paths->count(); i < e; ++i)
    {
        QListWidgetItem* item = m_ui->listwidget_paths->item(i);

        if (item->data(Qt::UserRole).value<SearchPathType>() == SearchPathType::Explicit)
            search_paths.push_back_explicit_path(item->text().toStdString());
    }

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
    QListWidgetItem* item = make_item(SearchPathType::Explicit);
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
    for (QListWidgetItem* selected_item : m_ui->listwidget_paths->selectedItems())
        delete selected_item;

    slot_path_selection_changed();
}

void SearchPathsWindow::slot_move_up()
{
    const int index = m_ui->listwidget_paths->currentRow();

    const int first_editable_path_index =
        static_cast<int>(m_project.search_paths().get_environment_path_count());

    if (index > first_editable_path_index)
    {
        QListWidgetItem* item = m_ui->listwidget_paths->takeItem(index);
        m_ui->listwidget_paths->insertItem(index - 1, item);
        m_ui->listwidget_paths->setCurrentItem(item);
    }
}

void SearchPathsWindow::slot_move_down()
{
    const int index = m_ui->listwidget_paths->currentRow();

    if (index < m_ui->listwidget_paths->count() - 1)
    {
        QListWidgetItem* item = m_ui->listwidget_paths->takeItem(index);
        m_ui->listwidget_paths->insertItem(index + 1, item);
        m_ui->listwidget_paths->setCurrentItem(item);
    }
}

void SearchPathsWindow::slot_path_selection_changed()
{
    const int index = m_ui->listwidget_paths->currentRow();

    const int first_editable_path_index =
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
