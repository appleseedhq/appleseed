
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

#pragma once

// appleseed.studio headers.
#include "mainwindow/project/entityeditorcontext.h"
#include "mainwindow/project/itemregistry.h"
#include "mainwindow/project/projectbuilder.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QList>
#include <QObject>
#include <QShortcut>

// Standard headers.
#include <memory>

// Forward declarations.
namespace appleseed { namespace qtcommon { class ProjectManager; } }
namespace appleseed { namespace studio { class AttributeEditor; } }
namespace appleseed { namespace studio { class ItemBase; } }
namespace appleseed { namespace studio { class RenderingManager; } }
namespace renderer  { class ParamArray; }
namespace renderer  { class Project; }
class QMenu;
class QPoint;
class QString;
class QTreeWidget;
class QTreeWidgetItem;

namespace appleseed {
namespace studio {

class ProjectExplorer
  : public QObject
  , public foundation::NonCopyable
{
    Q_OBJECT

  public:
    ProjectExplorer(
        QTreeWidget*                tree_widget,
        AttributeEditor*            attribute_editor,
        renderer::Project&          project,
        qtcommon::ProjectManager&   project_manager,
        RenderingManager&           rendering_manager,
        renderer::ParamArray&       settings);

    ~ProjectExplorer() override;

    void filter_items(const QString& pattern) const;

    void clear_selection() const;
    ItemBase* select_entity(const foundation::UniqueID uid) const;

  signals:
    void signal_project_modified() const;
    void signal_frame_modified() const;
    void signal_post_processing_stage_modified(const std::uint64_t stage_uid) const;

  private:
    QTreeWidget*                    m_tree_widget;
    AttributeEditor*                m_attribute_editor;
    ProjectBuilder                  m_project_builder;
    ItemRegistry                    m_item_registry;
    EntityEditorContext             m_editor_context;
    std::unique_ptr<QShortcut>      m_delete_shortcut;

    QMenu* build_single_item_context_menu(QTreeWidgetItem* item) const;
    QMenu* build_multiple_items_context_menu(const QList<QTreeWidgetItem*>& item_widgets) const;

  private slots:
    void slot_context_menu(const QPoint& point);
    void slot_item_selection_changed();
    void slot_edit_item(QTreeWidgetItem* item, int column);
    void slot_drag_item(QTreeWidgetItem* item, int column);
    void slot_delete_items();
};

}   // namespace studio
}   // namespace appleseed
