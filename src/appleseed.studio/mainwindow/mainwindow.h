
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
#include "debug/benchmarks/benchmarkwindow.h"
#include "debug/tests/testwindow.h"
#include "mainwindow/applicationsettingswindow.h"
#include "mainwindow/falsecolorswindow.h"
#include "mainwindow/rendering/renderingmanager.h"
#include "mainwindow/rendering/rendertab.h"
#include "mainwindow/renderingsettingswindow.h"
#include "mainwindow/statusbar.h"

// appleseed.qtcommon headers.
#include "project/projectmanager.h"
#include "widgets/qtlogtarget.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// Qt headers.
#include <QMainWindow>
#include <QObject>

// OpenColorIO headers.
#include <OpenColorIO/OpenColorIO.h>
namespace OCIO = OCIO_NAMESPACE;

// Standard headers.
#include <map>
#include <memory>
#include <string>
#include <vector>

// Forward declarations.
namespace appleseed { namespace studio { class AttributeEditor; } }
namespace appleseed { namespace studio { class LightPathsTab; } }
namespace appleseed { namespace studio { class MinimizeButton; } }
namespace appleseed { namespace studio { class ProjectExplorer; } }
namespace renderer  { class Project; }
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
    explicit MainWindow(QWidget* parent = nullptr);

    // Destructor.
    ~MainWindow() override;

    // Project file handling.
    void new_project();
    bool open_project(const QString& filepath);
    void open_project_async(const QString& filepath);
    void open_and_render_project(const QString& filepath, const QString& configuration);
    bool save_project(QString filepath);
    bool pack_project(QString filepath);
    void close_project();

    void on_project_change();

    qtcommon::ProjectManager* get_project_manager();
    renderer::ParamArray& get_application_settings();

    QDockWidget* create_dock_widget(const char* dock_name);

  signals:
    void signal_refresh_attribute_editor(const foundation::Dictionary& values) const;
    void signal_application_settings_modified() const;

  private:
    enum class RenderingMode
    {
        NotRendering,
        InteractiveRendering,
        FinalRendering
    };

    // Not wrapped in std::unique_ptr<> to avoid pulling in the UI definition code.
    Ui::MainWindow*                             m_ui;

    QAction*                                    m_action_new_project;
    QAction*                                    m_action_open_project;
    QAction*                                    m_action_save_project;
    QAction*                                    m_action_reload_project;
    QAction*                                    m_action_monitor_project_file;

    QAction*                                    m_action_start_interactive_rendering;
    QAction*                                    m_action_start_final_rendering;
    QAction*                                    m_action_pause_resume_rendering;
    QAction*                                    m_action_stop_rendering;
    QAction*                                    m_action_rendering_settings;
    QAction*                                    m_action_fullscreen;

    std::vector<QAction*>                       m_recently_opened;
    std::vector<MinimizeButton*>                m_minimize_buttons;

    StatusBar                                   m_status_bar;
    std::unique_ptr<qtcommon::QtLogTarget>      m_log_target;

    renderer::ParamArray                        m_application_settings;

    std::unique_ptr<ApplicationSettingsWindow>  m_application_settings_window;
    std::unique_ptr<RenderingSettingsWindow>    m_rendering_settings_window;
    std::unique_ptr<TestWindow>                 m_test_window;
    std::unique_ptr<BenchmarkWindow>            m_benchmark_window;
    std::unique_ptr<FalseColorsWindow>          m_false_colors_window;

    qtcommon::ProjectManager                    m_project_manager;
    ProjectExplorer*                            m_project_explorer;
    QFileSystemWatcher*                         m_project_file_watcher;
    AttributeEditor*                            m_attribute_editor;
    RenderingManager                            m_rendering_manager;

    typedef std::map<std::string, RenderTab*> RenderTabCollection;
    typedef std::map<std::string, RenderTab::State> RenderTabStateCollection;

    RenderTabCollection                         m_render_tabs;
    std::map<int, RenderTab*>                   m_tab_index_to_render_tab;
    LightPathsTab*                              m_light_paths_tab;

    struct StateBeforeProjectOpen
    {
        bool                                    m_is_rendering;
        RenderTabStateCollection                m_render_tab_states;
    };

    std::unique_ptr<StateBeforeProjectOpen>     m_state_before_project_open;

    bool                                        m_fullscreen;
    OCIO::ConstConfigRcPtr                      m_ocio_config;

    // Menus.
    void build_menus();
    void build_override_shading_menu_item();
    void update_override_shading_menu_item();
    void build_recent_files_menu();
    void update_recent_files_menu(const QString& filepath);
    void update_recent_files_menu(const QStringList& files);
    void update_pause_resume_checkbox(const bool checked);

    // Other UI elements.
    void build_status_bar();
    void build_toolbar();
    void build_log_panel();
    void build_python_console_panel();
    void build_project_explorer();
    void build_connections();

    // UI state management.
    void update_workspace();
    void update_project_explorer();
    void update_window_title();
    void set_file_widgets_enabled(const bool is_enabled, const RenderingMode rendering_mode);
    void set_project_explorer_enabled(const bool is_enabled);
    void set_rendering_widgets_enabled(const bool is_enabled, const RenderingMode rendering_mode);
    void set_diagnostics_widgets_enabled(const bool is_enabled, const RenderingMode rendering_mode);
    void save_state_before_project_open();
    void restore_state_after_project_open();

    // Render tabs.
    void recreate_render_tabs();
    void remove_render_tabs();
    void add_render_tab(const QString& label);
    void add_light_paths_tab();
    void remove_light_paths_tab();

    // Project file handling.
    renderer::ParamArray get_project_params(const char* configuration_name) const;
    bool can_close_project();

    // Project file monitoring.
    void enable_project_file_monitoring();
    void disable_project_file_monitoring();
    void start_monitoring_project_file();
    void stop_monitoring_project_file();

    // Drag-and-drop.
    void dragEnterEvent(QDragEnterEvent* event) override;
    void dropEvent(QDropEvent* event) override;

    // Rendering.
    void start_rendering(const RenderingMode rendering_mode);

    // Diagnostics.
    void apply_false_colors_settings();
    void apply_post_processing_stage(
        renderer::PostProcessingStage&  stage,
        renderer::Frame&                working_frame);

    // Miscellaneous.
    void initialize_ocio();
    void closeEvent(QCloseEvent* event) override;

  private slots:
    // Project I/O.
    void slot_new_project();
    void slot_open_project();
    void slot_open_recent();
    void slot_clear_all_recent_project_files();
    void slot_clear_recent_missing_project_files();
    void slot_open_cornellbox_builtin_project();
    void slot_reload_project();
    void slot_open_project_complete(const QString& filepath, const bool successful);
    void slot_save_project();
    void slot_save_project_as();
    void slot_pack_project_as();
    void slot_close_project();
    void slot_project_modified();
    void slot_post_processing_stage_modified(const std::uint64_t stage_uid);

    // Project file monitoring.
    void slot_toggle_project_file_monitoring(const bool checked);
    void slot_project_file_changed(const QString& filepath);

    // Application settings I/O.
    void slot_load_application_settings();
    void slot_save_application_settings();
    void slot_apply_application_settings();

    // Rendering.
    void slot_start_interactive_rendering();
    void slot_start_final_rendering();
    void slot_start_rendering_once(
        const QString&  filepath,
        const QString&  configuration,
        const bool      successful);
    void slot_pause_or_resume_rendering(
        const bool      checked);
    void slot_rendering_end();
    void slot_camera_changed();

    // Diagnostics.
    void slot_clear_shading_override();
    void slot_set_shading_override();
    void slot_show_false_colors_window();
    void slot_apply_false_colors_settings_changes(foundation::Dictionary values);

    // Render region.
    void slot_clear_render_region();
    void slot_set_render_region(const QRect& rect);

    // Render widget actions.
    void slot_render_widget_context_menu(const QPoint& point);
    void slot_save_frame();
    void slot_save_frame_and_aovs();
    void slot_quicksave_frame_and_aovs();
    void slot_save_render_widget_content();
    void slot_clear_frame();
    void slot_reset_zoom();

    // Project explorer.
    void slot_filter_text_changed(const QString& pattern);
    void slot_clear_filter();
    void slot_frame_modified();

    // General UI actions.
    void slot_fullscreen();
    void slot_check_fullscreen();

    // Child windows.
    void slot_show_application_settings_window();
    void slot_show_rendering_settings_window();
    void slot_show_test_window();
    void slot_show_benchmark_window();
    void slot_show_about_window();
};

}   // namespace studio
}   // namespace appleseed
