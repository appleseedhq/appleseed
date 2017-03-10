
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_MAINWINDOW_H
#define APPLESEED_STUDIO_MAINWINDOW_MAINWINDOW_H

// appleseed.studio headers.
#include "debug/benchmarks/benchmarkwindow.h"
#include "debug/tests/testwindow.h"
#include "mainwindow/project/projectmanager.h"
#include "mainwindow/qtlogtarget.h"
#include "mainwindow/rendering/renderingmanager.h"
#include "mainwindow/rendering/rendertab.h"
#include "mainwindow/renderingsettingswindow.h"
#include "mainwindow/statusbar.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// Qt headers.
#include <QMainWindow>
#include <QObject>

// Standard headers.
#include <map>
#include <memory>
#include <string>
#include <vector>

// Forward declarations.
namespace appleseed { namespace studio { class AttributeEditor; } }
namespace appleseed { namespace studio { class ProjectExplorer; } }
namespace appleseed { namespace studio { class MinimizeButton; } }
namespace Ui        { class MainWindow; }
class QAction;
class QCloseEvent;
class QDragEnterEvent;
class QDropEvent;
class QFileSystemWatcher;
class QPoint;
class QRect;
class QString;
class QStringList;
class QWidget;

namespace appleseed {
namespace studio {

//
// appleseed.studio's main window.
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

    void open_project(const QString& filepath);
    void open_and_render_project(const QString& filepath, const QString& configuration);

  signals:
    void signal_refresh_attribute_editor(const foundation::Dictionary& values) const;

  private:
    // Not wrapped in std::auto_ptr<> to avoid pulling in the UI definition code.
    Ui::MainWindow*                         m_ui;

    QAction*                                m_action_new_project;
    QAction*                                m_action_open_project;
    QAction*                                m_action_save_project;
    QAction*                                m_action_reload_project;
    QAction*                                m_action_monitor_project_file;

    QAction*                                m_action_start_interactive_rendering;
    QAction*                                m_action_start_final_rendering;
    QAction*                                m_action_stop_rendering;
    QAction*                                m_action_rendering_settings;

    std::vector<QAction*>                   m_recently_opened;
    std::vector<MinimizeButton*>            m_minimize_buttons;

    StatusBar                               m_status_bar;
    std::auto_ptr<QtLogTarget>              m_log_target;

    renderer::ParamArray                    m_settings;

    std::auto_ptr<RenderingSettingsWindow>  m_rendering_settings_window;
    std::auto_ptr<TestWindow>               m_test_window;
    std::auto_ptr<BenchmarkWindow>          m_benchmark_window;

    ProjectManager                          m_project_manager;
    ProjectExplorer*                        m_project_explorer;
    QFileSystemWatcher*                     m_project_file_watcher;
    AttributeEditor*                        m_attribute_editor;
    RenderingManager                        m_rendering_manager;

    typedef std::map<std::string, RenderTab*> RenderTabCollection;
    typedef std::map<std::string, RenderTab::State> RenderTabStateCollection;

    RenderTabCollection                     m_render_tabs;
    std::map<int, RenderTab*>               m_tab_index_to_render_tab;

    struct StateBeforeProjectOpen
    {
        bool                                m_is_rendering;
        RenderTabStateCollection            m_render_tab_states;
    };

    std::auto_ptr<StateBeforeProjectOpen>   m_state_before_project_open;

    bool                                    m_fullscreen;

    // Menus.
    void build_menus();
    void build_override_shading_menu_item();
    void update_override_shading_menu_item();
    void build_recent_files_menu();
    void update_recent_files_menu(const QString& filepath);
    void update_recent_files_menu(const QStringList& files);

    // Other UI elements.
    void build_toolbar();
    void build_log_panel();
    void build_project_explorer();
    void build_minimize_buttons();
    void build_connections();

    enum RenderingMode
    {
        NotRendering,
        InteractiveRendering,
        FinalRendering
    };

    // UI state management.
    void update_workspace();
    void update_project_explorer();
    void update_window_title();
    void set_file_widgets_enabled(const bool is_enabled, const RenderingMode rendering_mode);
    void set_project_explorer_enabled(const bool is_enabled);
    void set_rendering_widgets_enabled(const bool is_enabled, const RenderingMode rendering_mode);
    void save_state_before_project_open();
    void restore_state_after_project_open();

    // Render widgets.
    void recreate_render_widgets();
    void remove_render_widgets();
    void add_render_widget(const QString& label);

    // Project file handling.
    renderer::ParamArray get_project_params(const char* configuration_name) const;
    bool can_close_project();
    void on_project_change();

    // Project file monitoring.
    void enable_project_file_monitoring();
    void disable_project_file_monitoring();
    void start_monitoring_project_file();
    void stop_monitoring_project_file();

    // Drag-and-drop.
    void dragEnterEvent(QDragEnterEvent* event);
    void dropEvent(QDropEvent* event);

    // Rendering.
    void start_rendering(const RenderingMode rendering_mode);

    // Miscellaneous.
    void print_startup_information();
    virtual void closeEvent(QCloseEvent* event);

  private slots:
    // Project I/O.
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

    // Project file monitoring.
    void slot_toggle_project_file_monitoring(const bool checked);
    void slot_project_file_changed(const QString& filepath);

    // Settings I/O.
    void slot_load_settings();
    void slot_save_settings();

    // Rendering.
    void slot_start_interactive_rendering();
    void slot_start_final_rendering();
    void slot_start_rendering_once(
        const QString&  filepath,
        const QString&  configuration,
        const bool      successful);
    void slot_rendering_end();
    void slot_camera_changed();

    // Shading overrides.
    void slot_clear_shading_override();
    void slot_set_shading_override();

    // Render region.
    void slot_clear_render_region();
    void slot_set_render_region(const QRect& rect);

    // Render widget actions.
    void slot_render_widget_context_menu(const QPoint& point);
    void slot_save_frame();
    void slot_save_all_aovs();
    void slot_quicksave_all_aovs();
    void slot_clear_frame();
    void slot_reset_zoom();

    // Project explorer.
    void slot_filter_text_changed(const QString& pattern);
    void slot_clear_filter();
    void slot_frame_modified();

    // General UI actions.
    void slot_fullscreen();

    // Child windows.
    void slot_show_rendering_settings_window();
    void slot_show_test_window();
    void slot_show_benchmark_window();
    void slot_show_about_window();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_MAINWINDOW_H
