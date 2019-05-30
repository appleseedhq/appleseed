
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Kevin Masson, The appleseedhq Organization
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
#include "mainwindow/rendering/lightpathsviewporttoolbar.h"
#include "mainwindow/rendering/materialdrophandler.h"
#include "mainwindow/rendering/pixelcolortracker.h"
#include "mainwindow/rendering/pixelinspectorhandler.h"
#include "mainwindow/rendering/renderclipboardhandler.h"
#include "mainwindow/rendering/scenepickinghandler.h"
#include "mainwindow/rendering/viewportcanvas.h"
#include "mainwindow/rendering/viewportregionselectionhandler.h"
#include "mainwindow/rendering/viewporttab.h"

// appleseed.qtcommon headers.
#include "widgets/mousecoordinatestracker.h"
#include "widgets/scrollareapanhandler.h"
#include "widgets/widgetzoomhandler.h"

// OpenColorIO headers.
#include <OpenColorIO/OpenColorIO.h>
namespace OCIO = OCIO_NAMESPACE;

// Qt headers.
#include <QObject>
#include <QWidget>

// Standard headers.
#include <memory>

// Forward declarations.
namespace appleseed { namespace studio { class ProjectExplorer; } }
namespace appleseed { namespace studio { class RenderingManager; } }
namespace renderer  { class Entity; }
namespace renderer  { class Project; }
class QComboBox;
class QLabel;
class QPoint;
class QRect;
class QScrollArea;
class QToolBar;
class QToolButton;

namespace appleseed {
namespace studio {

class OpenGLViewportTab
  : public ViewportTab
{
    Q_OBJECT

  public:
    OpenGLViewportTab(
        ProjectExplorer&                    project_explorer,
        renderer::Project&                  project,
        RenderingManager&                   rendering_manager,
        LightPathsManager&                  light_paths_manager,
        OCIO::ConstConfigRcPtr              ocio_config,
        renderer::ParamArray                application_settings);

    ViewportCanvas* get_viewport_canvas() const override;

    void render_began() override;
    void update_size() override;
    void on_tab_selected() override;

  signals:
    void signal_reset_zoom();
    void signal_viewport_canvas_context_menu(const QPoint& point);

  private slots:
    void slot_camera_changed();
    void slot_viewport_canvas_context_menu(const QPoint& point);
    void slot_toggle_light_paths(const bool checked);

  private:
    ViewportCanvas*                                     m_viewport_canvas;
    QScrollArea*                                        m_scroll_area;
    QToolBar*                                           m_toolbar;
    QToolButton*                                        m_light_paths_toggle_button;

    std::unique_ptr<CameraController>                   m_camera_controller;
    std::unique_ptr<RenderClipboardHandler>             m_clipboard_handler;

    LightPathsManager&                                  m_light_paths_manager;
    std::unique_ptr<LightPathsViewportToolbar>          m_light_paths_viewport_toolbar;

    void create_viewport_canvas();
    void create_toolbar();
    void create_scrollarea();
    void recreate_handlers();
};

}   // namespace studio
}   // namespace appleseed
