
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
#include "mainwindow/rendering/cameracontroller.h"
#include "mainwindow/rendering/lightpathsmanager.h"
#include "mainwindow/rendering/renderclipboardhandler.h"
#include "mainwindow/rendering/viewportcanvas.h"

// appleseed.qtcommon headers.
#include "widgets/scrollareapanhandler.h"
#include "widgets/widgetzoomhandler.h"

// appleseed.renderer headers.
#include "renderer/api/rendering.h"

// Qt headers.
#include <QObject>
#include <QWidget>

// Forward declarations.
namespace appleseed { namespace studio { class ViewportTab; } }
namespace renderer  { class PamArray; }
namespace renderer  { class Project; }
class QAction;
class QEvent;
class QLabel;
class QPoint;
class QRect;
class QScrollArea;
class QToolBar;
class QToolButton;

namespace appleseed {
namespace studio {

//
// Control pannel for light paths.
//

class LightPathsViewportToolbar
  : public QObject
{
    Q_OBJECT

  public:
    LightPathsViewportToolbar(
        ViewportTab*                                viewport_tab,
        renderer::Project*                          project,
        LightPathsManager&                          light_paths_manager);

    void reset(renderer::Project* project);

    QToolBar* toolbar() const;

    void set_enabled(const bool enabled);

  signals:
    void signal_display_next_light_path();
    void signal_display_previous_light_path();

  private slots:
    void slot_light_path_selection_changed(
        const bool                                  display_light_paths, 
        const int                                   selected_light_path_index,
        const int                                   total_light_paths);
    void slot_save_light_paths();

  private:
    bool                                        m_enabled;

    renderer::Project*                          m_project;
    LightPathsManager&                          m_light_paths_manager;
    ViewportCanvas*                             m_viewport_canvas;
    QToolBar*                                   m_toolbar;
    QToolButton*                                m_save_light_paths_button;
    QToolButton*                                m_prev_path_button;
    QToolButton*                                m_next_path_button;

    void create_toolbar();
    void refresh_toolbar() const;

    bool eventFilter(QObject* object, QEvent* event) override;
};

}   // namespace studio
}   // namespace appleseed
