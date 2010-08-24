
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECTEXPLORER_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECTEXPLORER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Qt headers.
#include <QList>
#include <QObject>

// Standard headers.
#include <string>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class Project; }
class QMenu;
class QPoint;
class QTreeWidget;
class QTreeWidgetItem;

namespace appleseed {
namespace studio {

class ProjectExplorer
  : public QObject
  , foundation::NonCopyable
{
    Q_OBJECT

  public:
    ProjectExplorer(QTreeWidget* tree_widget, renderer::Project* project);

  private:
    QTreeWidget*        m_tree_widget;
    renderer::Project*  m_project;

    void update_tree_widget();

    QMenu* build_context_menu(const QList<QTreeWidgetItem*> selected_items) const;
    QMenu* build_generic_context_menu() const;
    QMenu* build_assembly_context_menu(const void* assembly) const;

    void add_objects(
        const renderer::Assembly&   assembly,
        const std::string&          path) const;

  private slots:
    void slot_context_menu(const QPoint&);

    void slot_add_assembly();
    void slot_instantiate_assembly();
    void slot_add_objects_to_assembly();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECTEXPLORER_H
