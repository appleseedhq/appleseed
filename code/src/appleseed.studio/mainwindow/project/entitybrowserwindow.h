
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYBROWSERWINDOW_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYBROWSERWINDOW_H

// Qt headers.
#include <QObject>
#include <QString>
#include <QWidget>

// Standard headers.
#include <map>
#include <string>

// Forward declarations.
namespace foundation    { class StringDictionary; }
namespace Ui            { class EntityBrowserWindow; }
class QListWidget;
class QListWidgetItem;

namespace appleseed {
namespace studio {

class EntityBrowserWindow
  : public QWidget
{
    Q_OBJECT

  public:
    EntityBrowserWindow(
        QWidget*                            parent,
        const std::string&                  window_title);

    ~EntityBrowserWindow() override;

    void add_items_page(
        const std::string&                  page_name,
        const std::string&                  page_label,
        const foundation::StringDictionary& items);

  signals:
    void signal_accepted(QString page_name, QString item_value);

  private:
    struct Page
    {
        std::string     m_page_name;
        QListWidget*    m_list_widget;
    };

    // Not wrapped in std::unique_ptr<> to avoid pulling in the UI definition code.
    Ui::EntityBrowserWindow*    m_ui;

    std::map<int, Page>         m_pages;

  private slots:
    void slot_current_tab_changed(int tab_index);
    void slot_item_selection_changed();
    void slot_item_activated(QListWidgetItem* current);
    void slot_accept();
    void slot_clear_filter();
    void slot_filter_text_changed(const QString& pattern);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYBROWSERWINDOW_H
