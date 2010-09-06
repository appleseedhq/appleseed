
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_ENTITYBROWSERWINDOW_H
#define APPLESEED_STUDIO_MAINWINDOW_ENTITYBROWSERWINDOW_H

// Qt headers.
#include <QObject>
#include <QString>
#include <QWidget>

// Standard headers.
#include <string>

// Forward declarations.
namespace foundation    { class StringDictionary; }
namespace Ui            { class EntityBrowserWindow; }
class QListWidgetItem;

namespace appleseed {
namespace studio {

class EntityBrowserWindow
  : public QWidget
{
    Q_OBJECT

  public:
    EntityBrowserWindow(
        QWidget*            parent,
        const std::string&  window_title);

    ~EntityBrowserWindow();

    void add_items(const foundation::StringDictionary& items);

  signals:
    void accepted(QString item_value);

  private:
    // Not wrapped in std::auto_ptr<> to avoid pulling in the UI definition code.
    Ui::EntityBrowserWindow* m_ui;

  private slots:
    void slot_item_selection_changed();
    void slot_item_activated(QListWidgetItem* current);
    void slot_accept();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_ENTITYBROWSERWINDOW_H
