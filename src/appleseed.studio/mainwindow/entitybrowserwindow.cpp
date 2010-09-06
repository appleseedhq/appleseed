
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
#include "entitybrowserwindow.h"

// UI definition header.
#include "ui_entitybrowserwindow.h"

// appleseed.studio headers.
#include "utility/interop.h"
#include "utility/tweaks.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"

// Qt headers.
#include <QDialogButtonBox>
#include <QList>
#include <QListWidget>
#include <QListWidgetItem>
#include <QPushButton>
#include <QShortcut>
#include <Qt>

using namespace foundation;
using namespace std;

namespace appleseed {
namespace studio {

EntityBrowserWindow::EntityBrowserWindow(
    QWidget*        parent,
    const string&   window_title)
  : QWidget(parent)
  , m_ui(new Ui::EntityBrowserWindow())
{
    m_ui->setupUi(this);

    setAttribute(Qt::WA_DeleteOnClose);
    setWindowFlags(Qt::Tool);
    setWindowTitle(QString::fromStdString(window_title));

    resize(400, 300);

    m_ui->buttonbox->button(QDialogButtonBox::Ok)->setEnabled(false);

    connect(
        m_ui->listwidget, SIGNAL(itemSelectionChanged()),
        this, SLOT(slot_item_selection_changed()));

    connect(
        m_ui->listwidget, SIGNAL(itemActivated(QListWidgetItem*)),
        this, SLOT(slot_item_activated(QListWidgetItem*)));

    connect(
        m_ui->buttonbox->button(QDialogButtonBox::Ok), SIGNAL(clicked()),
        this, SLOT(slot_accept()));

    connect(
        m_ui->buttonbox->button(QDialogButtonBox::Cancel), SIGNAL(clicked()),
        this, SLOT(close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Return), SIGNAL(activated()),
        this, SLOT(slot_accept()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), SIGNAL(activated()),
        this, SLOT(close()));
}

EntityBrowserWindow::~EntityBrowserWindow()
{
    delete m_ui;
}

void EntityBrowserWindow::add_items(const StringDictionary& items)
{
    for (const_each<StringDictionary> i = items; i; ++i)
    {
        const QString item_label = i->name();
        const QString item_value = i->value<QString>();

        QListWidgetItem* item = new QListWidgetItem(item_label, m_ui->listwidget);
        item->setData(0, item_value);
    }
}

void EntityBrowserWindow::slot_item_selection_changed()
{
    const bool item_selected = m_ui->listwidget->selectedItems().size() == 1;

    m_ui->buttonbox->button(QDialogButtonBox::Ok)->setEnabled(item_selected);
}

void EntityBrowserWindow::slot_item_activated(QListWidgetItem* item)
{
    emit accepted(item->data(0).toString());
}

void EntityBrowserWindow::slot_accept()
{
    const QList<QListWidgetItem*> selected_items = m_ui->listwidget->selectedItems();

    if (selected_items.size() == 1)
        slot_item_activated(selected_items.first());
}

}   // namespace studio
}   // namespace appleseed
