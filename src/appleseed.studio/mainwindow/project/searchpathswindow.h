
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

#pragma once

// appleseed.studio headers.
#include "utility/windowbase.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed { namespace qtcommon { class ProjectManager; } }
namespace renderer  { class Project; }
namespace Ui        { class SearchPathsWindow; }
class QWidget;

namespace appleseed {
namespace studio {

//
// Search paths window.
//

class SearchPathsWindow
  : public WindowBase
{
    Q_OBJECT

  public:
    // Constructor.
    SearchPathsWindow(
        const renderer::Project&    project,
        qtcommon::ProjectManager&   project_manager,
        QWidget*                    parent = nullptr);

    // Destructor.
    ~SearchPathsWindow() override;

  signals:
    void signal_paths_modified() const;

  private:
    // Not wrapped in std::unique_ptr<> to avoid pulling in the UI definition code.
    Ui::SearchPathsWindow*          m_ui;

    const renderer::Project&        m_project;
    bool                            m_edit_committed;

    void load_search_paths();
    void save_search_paths();

  private slots:
    void accept();
    void reject();
    void slot_update_root_path();
    void slot_add();
    void slot_remove();
    void slot_move_up();
    void slot_move_down();
    void slot_path_selection_changed();
    void slot_path_editor_committed(QWidget* widget);
    void slot_path_editor_closed(QWidget* widget);

};

}   // namespace studio
}   // namespace appleseed
