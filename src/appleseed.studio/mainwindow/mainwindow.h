
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_MAINWINDOW_H
#define APPLESEED_STUDIO_MAINWINDOW_MAINWINDOW_H

// appleseed.studio headers.
#include "debug/benchmarks/benchmarkwindow.h"
#include "debug/tests/testwindow.h"
#include "help/about/aboutwindow.h"
#include "mainwindow/project/projectexplorer.h"
#include "mainwindow/project/projectmanager.h"
#include "mainwindow/rendering/renderingmanager.h"
#include "mainwindow/rendering/renderwidget.h"
#include "mainwindow/qtlogtarget.h"
#include "mainwindow/rendersettingswindow.h"
#include "mainwindow/statusbar.h"
#include "utility/mousecoordinatestracker.h"
#include "utility/scrollareapanhandler.h"
#include "utility/widgetzoomhandler.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Qt headers.
#include <QCloseEvent>
#include <QMainWindow>
#include <QObject>
#include <QScrollArea>

// Standard headers.
#include <map>
#include <memory>
#include <string>
#include <vector>

// Forward declarations.
namespace appleseed { namespace studio { class LogWidget; } }
namespace Ui        { class MainWindow; }
class QAction;
class QLabel;
class QPoint;
class QString;
class QStringList;

namespace appleseed {
namespace studio {

//
// Application's main window.
//

class MainWindow
  : public QMainWindow
{
    Q_OBJECT

  public:
    // Constructor.
    explicit MainWindow(QWidget* parent = 0);

    // Destructor.
    ~MainWindow();

  public slots:
    void slot_recreate_render_widgets();

  private:
    // Not wrapped in std::auto_ptr<> to avoid pulling in the UI definition code.
    Ui::MainWindow*                     m_ui;

    QAction*                            m_action_new_project;
    QAction*                            m_action_open_project;
    QAction*                            m_clear_open_recent_menu;
    QAction*                            m_action_save_project;
    QAction*                            m_action_start_interactive_rendering;
    QAction*                            m_action_start_final_rendering;
    QAction*                            m_action_stop_rendering;

    std::vector<QAction*>               m_recently_opened;

    StatusBar                           m_status_bar;
    std::auto_ptr<QtLogTarget>          m_log_target;

    renderer::ParamArray                m_settings;

    std::auto_ptr<RenderSettingsWindow> m_render_settings_window;
    std::auto_ptr<TestWindow>           m_test_window;
    std::auto_ptr<BenchmarkWindow>      m_benchmark_window;

    ProjectManager                      m_project_manager;
    std::auto_ptr<ProjectExplorer>      m_project_explorer;
    RenderingManager                    m_rendering_manager;

    // A helper structure to associate event handlers to a render widget.
    struct RenderWidgetRecord
      : public foundation::NonCopyable
    {
        RenderWidget*                           m_render_widget;
        std::auto_ptr<WidgetZoomHandler>        m_zoom_handler;
        std::auto_ptr<ScrollAreaPanHandler>     m_pan_handler;
        std::auto_ptr<MouseCoordinatesTracker>  m_mouse_tracker;

        RenderWidgetRecord(
            QScrollArea*    scroll_area,
            RenderWidget*   render_widget,
            QLabel*         info_label,
            const int       content_width,
            const int       content_height)
          : m_render_widget(render_widget)
        {
            m_zoom_handler.reset(
                new WidgetZoomHandler(
                    scroll_area,
                    render_widget,
                    content_width,
                    content_height));

            m_pan_handler.reset(new ScrollAreaPanHandler(scroll_area));

            m_mouse_tracker.reset(
                new MouseCoordinatesTracker(
                    m_render_widget,
                    info_label,
                    content_width,
                    content_height));
        }
    };

    typedef std::map<std::string, RenderWidgetRecord*> RenderWidgetCollection;

    RenderWidgetCollection              m_render_widgets;

    void build_menus();
    void build_override_shading_menu_item();
    void update_override_shading_menu_item();

    void build_recent_files_menu();
    void update_recent_files_menu(const QString& filepath);
    void update_recent_files_menu(const QStringList& files);

    void build_toolbar();

    LogWidget* create_log_widget() const;
    void build_log();

    void build_project_explorer();

    void build_connections();

    void print_startup_information();

    renderer::ParamArray get_project_params(const char* configuration_name) const;

    void open_project(const QString& filepath);
    bool can_close_project();
    void on_project_change();

    void update_workspace();
    void update_project_explorer();
    void update_window_title();

    void set_project_widgets_enabled(const bool enabled);
    void set_rendering_widgets_enabled(const bool enabled, const bool is_rendering);

    void recreate_render_widgets();
    void remove_render_widgets();
    void add_render_widgets();
    void add_render_widget(
        const int       width,
        const int       height,
        const QString&  label);

    void start_rendering(const bool interactive);

    virtual void closeEvent(QCloseEvent* event);

  private slots:
    void slot_new_project();
    void slot_open_project();
    void slot_open_recent();
    void slot_clear_open_recent_files_menu();
    void slot_open_cornellbox_builtin_project();
    void slot_reload_project();
    void slot_open_project_complete(const QString& filepath, const bool successful);
    void slot_save_project();
    void slot_save_project_as();

    void slot_project_modified();

    void slot_start_interactive_rendering();
    void slot_start_final_rendering();
    void slot_stop_rendering();
    void slot_rendering_end();

    void slot_camera_changed();

    void slot_show_render_settings_window();
    void slot_show_test_window();
    void slot_show_benchmark_window();
    void slot_show_about_window();

    void slot_load_settings();
    void slot_save_settings();

    void slot_filter_text_changed(const QString& pattern);
    void slot_clear_filter();

    void slot_render_widget_context_menu(const QPoint& point);
    void slot_save_frame();
    void slot_save_all_aovs();
    void slot_clear_frame();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_MAINWINDOW_H
