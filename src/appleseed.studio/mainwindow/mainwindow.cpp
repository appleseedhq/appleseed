
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

// Interface header.
#include "mainwindow.h"

// UI definition header.
#include "ui_mainwindow.h"

// appleseed.studio headers.
#include "help/about/aboutwindow.h"
#include "mainwindow/minimizebutton.h"
#include "mainwindow/project/attributeeditor.h"
#include "mainwindow/project/projectexplorer.h"
#include "mainwindow/pythonconsole/pythonconsolewidget.h"
#include "mainwindow/rendering/lightpathstab.h"
#include "utility/settingskeys.h"

// appleseed.qtcommon headers.
#include "utility/interop.h"
#include "utility/miscellaneous.h"
#include "widgets/logwidget.h"
#include "widgets/renderwidget.h"

// appleseed.common headers.
#include "application/application.h"

// appleseed.renderer headers.
#include "renderer/api/aov.h"
#include "renderer/api/frame.h"
#include "renderer/api/lighting.h"
#include "renderer/api/log.h"
#include "renderer/api/postprocessing.h"
#include "renderer/api/project.h"
#include "renderer/api/rendering.h"
#include "renderer/api/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/core/appleseed.h"
#include "foundation/log/logmessage.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/path.h"
#include "foundation/platform/python.h"
#include "foundation/platform/system.h"
#include "foundation/utility/foreach.h"

// Qt headers.
#include <QAction>
#include <QActionGroup>
#include <QApplication>
#include <QCloseEvent>
#include <QDir>
#include <QDragEnterEvent>
#include <QFileInfo>
#include <QFileSystemWatcher>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QMenu>
#include <QMessageBox>
#include <QMimeData>
#include <QRect>
#include <QSettings>
#include <QStatusBar>
#include <QString>
#include <QStringList>
#include <Qt>
#include <QUrl>
#include <QVariant>

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstdlib>

using namespace appleseed::common;
using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;
namespace bf = boost::filesystem;

namespace appleseed {
namespace studio {

//
// MainWindow class implementation.
//

namespace
{
    const int MaxRecentlyOpenedFiles = 15;
}

MainWindow::MainWindow(QWidget* parent)
  : QMainWindow(parent)
  , m_ui(new Ui::MainWindow())
  , m_rendering_manager(m_status_bar)
  , m_project_explorer(nullptr)
  , m_attribute_editor(nullptr)
  , m_project_file_watcher(nullptr)
  , m_light_paths_tab(nullptr)
{
    initialize_ocio();

    m_ui->setupUi(this);

    build_menus();
    build_status_bar();
    build_toolbar();
    build_log_panel();
    build_python_console_panel();
    build_project_explorer();
    build_connections();

    slot_load_application_settings();
    slot_check_fullscreen();

    update_project_explorer();
    update_workspace();

    setAcceptDrops(true);
}

MainWindow::~MainWindow()
{
    delete m_project_explorer;
    delete m_ui;
}

namespace
{
    class CustomSignalMapper
      : public QObject
    {
        Q_OBJECT

      public:
        CustomSignalMapper(QObject* parent, const QString& configuration)
          : QObject(parent)
          , m_configuration(configuration)
        {
        }

      signals:
        void mapped(const QString& filepath, const QString& config, const bool success);

      public slots:
        void map(const QString& filepath, const bool success)
        {
            emit mapped(filepath, m_configuration, success);
        }

      private:
        const QString m_configuration;
    };
}

void MainWindow::new_project()
{
    m_project_manager.create_project();
    on_project_change();
}

bool MainWindow::open_project(const QString& filepath)
{
    save_state_before_project_open();

    if (m_rendering_manager.is_rendering())
    {
        m_rendering_manager.abort_rendering();
        m_rendering_manager.wait_until_rendering_end();
    }

    remove_render_tabs();

    set_file_widgets_enabled(false, RenderingMode::NotRendering);
    set_project_explorer_enabled(false);
    set_rendering_widgets_enabled(false, RenderingMode::NotRendering);
    set_diagnostics_widgets_enabled(false, RenderingMode::NotRendering);

    const bool successful = m_project_manager.load_project(filepath.toUtf8().constData());

    if (successful)
    {
        on_project_change();
    }
    else
    {
        recreate_render_tabs();
        update_workspace();
    }

    return successful;
}

void MainWindow::open_project_async(const QString& filepath)
{
    save_state_before_project_open();

    if (m_rendering_manager.is_rendering())
    {
        m_rendering_manager.abort_rendering();
        m_rendering_manager.wait_until_rendering_end();
    }

    remove_render_tabs();

    set_file_widgets_enabled(false, RenderingMode::NotRendering);
    set_project_explorer_enabled(false);
    set_rendering_widgets_enabled(false, RenderingMode::NotRendering);
    set_diagnostics_widgets_enabled(false, RenderingMode::NotRendering);

    m_project_manager.load_project_async(filepath.toUtf8().constData());
}

void MainWindow::open_and_render_project(const QString& filepath, const QString& configuration)
{
    CustomSignalMapper* mapper = new CustomSignalMapper(this, configuration);

    connect(
        &m_project_manager, SIGNAL(signal_load_project_async_complete(const QString&, const bool)),
        mapper, SLOT(map(const QString&, const bool)));

    connect(
        mapper, SIGNAL(mapped(const QString&, const QString&, const bool)),
        SLOT(slot_start_rendering_once(const QString&, const QString&, const bool)));

    open_project_async(filepath);
}

bool MainWindow::save_project(QString filepath)
{
    if (!m_project_manager.is_project_open())
        return false;

    const QString Extension = "appleseed";

    if (QFileInfo(filepath).suffix() != Extension)
        filepath += "." + Extension;

    if (m_project_file_watcher)
        stop_monitoring_project_file();

    const bool successful = m_project_manager.save_project_as(filepath.toUtf8().constData());

    if (m_project_file_watcher)
        start_monitoring_project_file();

    if (successful)
        update_recent_files_menu(filepath);
    update_workspace();

    return successful;
}

bool MainWindow::pack_project(QString filepath)
{
    if (!m_project_manager.is_project_open())
        return false;

    const QString Extension = "appleseedz";

    if (QFileInfo(filepath).suffix() != Extension)
        filepath += "." + Extension;

    return m_project_manager.pack_project_as(filepath.toUtf8().constData());
}

void MainWindow::close_project()
{
    m_project_manager.close_project();
    on_project_change();
}

ProjectManager* MainWindow::get_project_manager()
{
    return &m_project_manager;
}

ParamArray& MainWindow::get_application_settings()
{
    return m_application_settings;
}

QDockWidget* MainWindow::create_dock_widget(const char* dock_name)
{
    QDockWidget* dock_widget = new QDockWidget(this);

    const QString object_name = QString(dock_name).toLower().split(' ').join("_");
    dock_widget->setObjectName(object_name);
    dock_widget->setWindowTitle(dock_name);

    const QList<QAction*> actions = m_ui->menu_view->actions();
    QAction* menu_separator = actions.last();
    for (int i = actions.size() - 2; i != 0; --i)
    {
        if (actions[i]->isSeparator())
        {
            menu_separator = actions[i];
            break;
        }
    }

    m_ui->menu_view->insertAction(
        menu_separator,
        dock_widget->toggleViewAction());

    m_minimize_buttons.push_back(new MinimizeButton(dock_widget));

    statusBar()->insertPermanentWidget(
        static_cast<int>(m_minimize_buttons.size()),
        m_minimize_buttons.back());

    return dock_widget;
}

void MainWindow::build_menus()
{
    //
    // File menu.
    //

    m_ui->action_file_new_project->setShortcut(QKeySequence::New);
    connect(m_ui->action_file_new_project, SIGNAL(triggered()), SLOT(slot_new_project()));

    m_ui->action_file_open_project->setShortcut(QKeySequence::Open);
    connect(m_ui->action_file_open_project, SIGNAL(triggered()), SLOT(slot_open_project()));

    build_recent_files_menu();

    connect(m_ui->action_file_open_builtin_project_cornellbox, SIGNAL(triggered()), SLOT(slot_open_cornellbox_builtin_project()));
    connect(m_ui->action_file_reload_project, SIGNAL(triggered()), SLOT(slot_reload_project()));

    connect(m_ui->action_file_monitor_project, SIGNAL(toggled(bool)), SLOT(slot_toggle_project_file_monitoring(const bool)));

    m_ui->action_file_save_project->setShortcut(QKeySequence::Save);
    connect(m_ui->action_file_save_project, SIGNAL(triggered()), SLOT(slot_save_project()));

    m_ui->action_file_save_project_as->setShortcut(QKeySequence::SaveAs);
    connect(m_ui->action_file_save_project_as, SIGNAL(triggered()), SLOT(slot_save_project_as()));

    connect(m_ui->action_file_pack_project_as, SIGNAL(triggered()), SLOT(slot_pack_project_as()));

    m_ui->action_file_close_project->setShortcut(QKeySequence::Close);
    connect(m_ui->action_file_close_project, SIGNAL(triggered()), SLOT(slot_close_project()));

    m_ui->action_file_exit->setShortcut(QKeySequence::Quit);
    connect(m_ui->action_file_exit, SIGNAL(triggered()), SLOT(close()));

    //
    // View menu.
    //

    m_ui->menu_view->addAction(m_ui->project_explorer->toggleViewAction());
    m_ui->menu_view->addAction(m_ui->attribute_editor->toggleViewAction());
    m_ui->menu_view->addAction(m_ui->log->toggleViewAction());
    m_ui->menu_view->addAction(m_ui->python_console->toggleViewAction());

    m_ui->menu_view->addSeparator();

    m_action_fullscreen = m_ui->menu_view->addAction("&Full Screen");
    m_action_fullscreen->setCheckable(true);
    m_action_fullscreen->setShortcut(Qt::Key_F11);

    for (const auto dock_widget : findChildren<QDockWidget*>())
        connect(dock_widget->toggleViewAction(), SIGNAL(triggered()), SLOT(slot_check_fullscreen()));

    connect(m_action_fullscreen, SIGNAL(triggered()), SLOT(slot_fullscreen()));

    //
    // Rendering menu.
    //

    connect(m_ui->action_rendering_start_interactive_rendering, SIGNAL(triggered()), SLOT(slot_start_interactive_rendering()));
    connect(m_ui->action_rendering_start_final_rendering, SIGNAL(triggered()), SLOT(slot_start_final_rendering()));
    connect(m_ui->action_rendering_pause_resume_rendering, SIGNAL(toggled(bool)), SLOT(slot_pause_or_resume_rendering(const bool)));
    connect(m_ui->action_rendering_stop_rendering, SIGNAL(triggered()), &m_rendering_manager, SLOT(slot_abort_rendering()));
    connect(m_ui->action_rendering_rendering_settings, SIGNAL(triggered()), SLOT(slot_show_rendering_settings_window()));

    //
    // Diagnostics menu.
    //

    build_override_shading_menu_item();

    connect(m_ui->action_diagnostics_false_colors, SIGNAL(triggered()), SLOT(slot_show_false_colors_window()));

    //
    // Debug menu.
    //

    connect(m_ui->action_debug_tests, SIGNAL(triggered()), SLOT(slot_show_test_window()));
    connect(m_ui->action_debug_benchmarks, SIGNAL(triggered()), SLOT(slot_show_benchmark_window()));

    //
    // Tools menu.
    //

    connect(m_ui->action_tools_settings, SIGNAL(triggered()), SLOT(slot_show_application_settings_window()));
    connect(m_ui->action_tools_save_settings, SIGNAL(triggered()), SLOT(slot_save_application_settings()));
    connect(m_ui->action_tools_reload_settings, SIGNAL(triggered()), SLOT(slot_load_application_settings()));

    //
    // Help menu.
    //

    connect(m_ui->action_help_about, SIGNAL(triggered()), SLOT(slot_show_about_window()));
}

void MainWindow::build_override_shading_menu_item()
{
    QActionGroup* action_group = new QActionGroup(this);

    // No Override.
    connect(
        m_ui->action_diagnostics_override_shading_no_override, SIGNAL(triggered()),
        SLOT(slot_clear_shading_override()));
    action_group->addAction(m_ui->action_diagnostics_override_shading_no_override);

    for (int i = 0; i < DiagnosticSurfaceShader::ShadingModeCount; ++i)
    {
        const char* shading_mode_value = DiagnosticSurfaceShader::ShadingModeNames[i].m_key;
        const char* shading_mode_name = DiagnosticSurfaceShader::ShadingModeNames[i].m_value;

        QAction* action = new QAction(this);
        action->setObjectName(
            QString("action_diagnostics_override_shading_") + shading_mode_value);
        action->setCheckable(true);
        action->setText(shading_mode_name);

        const int shortcut_number = i + 1;
        if (shortcut_number <= 9)
            action->setShortcut(QKeySequence(QString("Ctrl+Shift+%1").arg(shortcut_number)));

        action->setData(shading_mode_value);

        connect(
            action, SIGNAL(triggered()),
            SLOT(slot_set_shading_override()));

        m_ui->menu_diagnostics_override_shading->addAction(action);
        action_group->addAction(action);
    }
}

void MainWindow::update_override_shading_menu_item()
{
    const ParamArray project_params = get_project_params("interactive");
    const ParamArray shading_engine_params = project_params.child("shading_engine");

    if (shading_engine_params.dictionaries().exist("override_shading"))
    {
        const std::string shading_mode =
            shading_engine_params.child("override_shading").get_optional<std::string>("mode", "coverage");

        for (const_each<QList<QAction*>> i = m_ui->menu_diagnostics_override_shading->actions(); i; ++i)
        {
            QAction* action = *i;

            if (action->data().toString().toStdString() == shading_mode)
            {
                action->activate(QAction::Trigger);
                break;
            }
        }
    }
    else
    {
        m_ui->action_diagnostics_override_shading_no_override->activate(QAction::Trigger);
    }
}

void MainWindow::build_recent_files_menu()
{
    assert(m_recently_opened.empty());
    m_recently_opened.reserve(MaxRecentlyOpenedFiles);

    for (int i = 0; i < MaxRecentlyOpenedFiles; ++i)
    {
        QAction* action = new QAction(this);
        action->setVisible(false);

        connect(action, SIGNAL(triggered()), SLOT(slot_open_recent()));

        m_ui->menu_open_recent->addAction(action);
        m_recently_opened.push_back(action);
    }

    QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    QStringList files = settings.value("recent_file_list").toStringList();

    update_recent_files_menu(files);

    m_ui->menu_open_recent->addSeparator();

    QAction* clear_missing_files = new QAction(this);
    clear_missing_files->setText("Clear &Missing Files");
    connect(clear_missing_files, SIGNAL(triggered()), SLOT(slot_clear_recent_missing_project_files()));
    m_ui->menu_open_recent->addAction(clear_missing_files);

    QAction* clear_all_files = new QAction(this);
    clear_all_files->setText("Clear &All Files");
    connect(clear_all_files, SIGNAL(triggered()), SLOT(slot_clear_all_recent_project_files()));
    m_ui->menu_open_recent->addAction(clear_all_files);
}

void MainWindow::update_recent_files_menu(const QString& filepath)
{
    QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    QStringList files = settings.value("recent_file_list").toStringList();

    files.removeAll(filepath);
    files.prepend(filepath);

    while (files.size() > MaxRecentlyOpenedFiles)
        files.removeLast();

    update_recent_files_menu(files);

    settings.setValue("recent_file_list", files);
}

void MainWindow::update_recent_files_menu(const QStringList& files)
{
    const int recent_file_count = std::min(files.size(), MaxRecentlyOpenedFiles);

    for (int i = 0; i < recent_file_count; ++i)
    {
        const int number = i + 1;
        const QString filepath = files[i];
        const QString format = number <= 9 ? "&%1 %2" : "%1 %2";
        const QString text = format.arg(number).arg(filepath);

        m_recently_opened[i]->setText(text);
        m_recently_opened[i]->setData(filepath);
        m_recently_opened[i]->setVisible(true);
    }

    for (int i = recent_file_count; i < MaxRecentlyOpenedFiles; ++i)
        m_recently_opened[i]->setVisible(false);
}

void MainWindow::update_pause_resume_checkbox(const bool checked)
{
    bool old_state = m_action_pause_resume_rendering->blockSignals(true);
    m_action_pause_resume_rendering->setChecked(checked);
    m_action_pause_resume_rendering->blockSignals(old_state);

    old_state = m_ui->action_rendering_pause_resume_rendering->blockSignals(true);
    m_ui->action_rendering_pause_resume_rendering->setChecked(checked);
    m_ui->action_rendering_pause_resume_rendering->blockSignals(old_state);
}

void MainWindow::build_status_bar()
{
    statusBar()->addWidget(&m_status_bar);

    m_minimize_buttons.push_back(new MinimizeButton(m_ui->project_explorer));
    m_minimize_buttons.push_back(new MinimizeButton(m_ui->attribute_editor));
    m_minimize_buttons.push_back(new MinimizeButton(m_ui->log));
    m_minimize_buttons.push_back(new MinimizeButton(m_ui->python_console));

    for (size_t i = 0; i < m_minimize_buttons.size(); ++i)
    {
        statusBar()->insertPermanentWidget(
            static_cast<int>(i + 1),
            m_minimize_buttons[i]);
    }
}

void MainWindow::build_toolbar()
{
    //
    // File actions.
    //

    m_action_new_project = new QAction(load_icons("project_new"), combine_name_and_shortcut("New Project", m_ui->action_file_new_project->shortcut()), this);
    connect(m_action_new_project, SIGNAL(triggered()), SLOT(slot_new_project()));
    m_ui->main_toolbar->addAction(m_action_new_project);

    m_action_open_project = new QAction(load_icons("project_open"), combine_name_and_shortcut("Open Project...", m_ui->action_file_open_project->shortcut()), this);
    connect(m_action_open_project, SIGNAL(triggered()), SLOT(slot_open_project()));
    m_ui->main_toolbar->addAction(m_action_open_project);

    m_action_save_project = new QAction(load_icons("project_save") , combine_name_and_shortcut("Save Project", m_ui->action_file_save_project->shortcut()), this);
    connect(m_action_save_project, SIGNAL(triggered()), SLOT(slot_save_project()));
    m_ui->main_toolbar->addAction(m_action_save_project);

    m_action_reload_project = new QAction(load_icons("project_reload"), combine_name_and_shortcut("Reload Project", m_ui->action_file_reload_project->shortcut()), this);
    connect(m_action_reload_project, SIGNAL(triggered()), SLOT(slot_reload_project()));
    m_ui->main_toolbar->addAction(m_action_reload_project);

    m_action_monitor_project_file = new QAction(load_icons("project_monitor"), "Toggle Project File Monitoring", this);
    m_action_monitor_project_file->setCheckable(true);
    connect(m_action_monitor_project_file, SIGNAL(toggled(bool)), SLOT(slot_toggle_project_file_monitoring(const bool)));
    m_ui->main_toolbar->addAction(m_action_monitor_project_file);

    m_ui->main_toolbar->addSeparator();

    //
    // Rendering actions.
    //

    m_action_start_interactive_rendering = new QAction(load_icons("rendering_start_interactive"), combine_name_and_shortcut("Start Interactive Rendering", m_ui->action_rendering_start_interactive_rendering->shortcut()), this);
    connect(m_action_start_interactive_rendering, SIGNAL(triggered()), SLOT(slot_start_interactive_rendering()));
    m_ui->main_toolbar->addAction(m_action_start_interactive_rendering);

    m_action_start_final_rendering = new QAction(load_icons("rendering_start_final"), combine_name_and_shortcut("Start Final Rendering", m_ui->action_rendering_start_final_rendering->shortcut()), this);
    connect(m_action_start_final_rendering, SIGNAL(triggered()), SLOT(slot_start_final_rendering()));
    m_ui->main_toolbar->addAction(m_action_start_final_rendering);

    m_action_pause_resume_rendering = new QAction(load_icons("rendering_pause_resume"), combine_name_and_shortcut("Pause Rendering", m_ui->action_rendering_pause_resume_rendering->shortcut()), this);
    m_action_pause_resume_rendering->setCheckable(true);
    connect(m_action_pause_resume_rendering, SIGNAL(toggled(bool)), SLOT(slot_pause_or_resume_rendering(const bool)));
    m_ui->main_toolbar->addAction(m_action_pause_resume_rendering);

    m_action_stop_rendering = new QAction(load_icons("rendering_stop"), combine_name_and_shortcut("Stop Rendering", m_ui->action_rendering_stop_rendering->shortcut()), this);
    connect(m_action_stop_rendering, SIGNAL(triggered()), &m_rendering_manager, SLOT(slot_abort_rendering()));
    m_ui->main_toolbar->addAction(m_action_stop_rendering);

    m_action_rendering_settings = new QAction(load_icons("rendering_settings"), combine_name_and_shortcut("Rendering Settings...", m_ui->action_rendering_rendering_settings->shortcut()), this);
    connect(m_action_rendering_settings, SIGNAL(triggered()), SLOT(slot_show_rendering_settings_window()));
    m_ui->main_toolbar->addAction(m_action_rendering_settings);
}

void MainWindow::build_log_panel()
{
    LogWidget* log_widget = new LogWidget(m_ui->log_contents);
    log_widget->setObjectName("textedit_log");
    log_widget->setUndoRedoEnabled(false);
    log_widget->setLineWrapMode(QTextEdit::NoWrap);
    log_widget->setReadOnly(true);
    log_widget->setTextInteractionFlags(Qt::TextSelectableByMouse);
    m_ui->log_contents->layout()->addWidget(log_widget);

    m_log_target.reset(new QtLogTarget(log_widget));
    global_logger().add_target(m_log_target.get());

    RENDERER_LOG_INFO(
        "%s, %s configuration\n"
        "compiled on %s at %s using %s version %s",
        Appleseed::get_synthetic_version_string(),
        Appleseed::get_lib_configuration(),
        Appleseed::get_lib_compilation_date(),
        Appleseed::get_lib_compilation_time(),
        Compiler::get_compiler_name(),
        Compiler::get_compiler_version());

    System::print_information(global_logger());
}

void MainWindow::build_python_console_panel()
{
    char* python_home = Py_GetPythonHome();
    if (python_home == nullptr)
        RENDERER_LOG_INFO("Python home not set.");
    else RENDERER_LOG_INFO("Python home set to %s.", python_home);

    PythonConsoleWidget* python_console_widget = new PythonConsoleWidget(m_ui->python_console_contents);
    python_console_widget->setObjectName("textedit_python_console");
    m_ui->python_console_contents->layout()->addWidget(python_console_widget);
}

void MainWindow::build_project_explorer()
{
    m_ui->treewidget_project_explorer_scene->setColumnWidth(0, 295);    // name

    disable_osx_focus_rect(m_ui->treewidget_project_explorer_scene);

    connect(
        m_ui->lineedit_filter, SIGNAL(textChanged(const QString&)),
        SLOT(slot_filter_text_changed(const QString&)));

    connect(
        m_ui->pushbutton_clear_filter, SIGNAL(clicked()),
        SLOT(slot_clear_filter()));

    m_ui->pushbutton_clear_filter->setEnabled(false);
}

void MainWindow::build_connections()
{
    connect(
        m_action_monitor_project_file, SIGNAL(toggled(bool)),
        m_ui->action_file_monitor_project, SLOT(setChecked(bool)));

    connect(
        m_ui->action_file_monitor_project, SIGNAL(toggled(bool)),
        m_action_monitor_project_file, SLOT(setChecked(bool)));

    connect(
        &m_project_manager, SIGNAL(signal_load_project_async_complete(const QString&, const bool)),
        SLOT(slot_open_project_complete(const QString&, const bool)));

    connect(
        &m_rendering_manager, SIGNAL(signal_rendering_end()),
        SLOT(slot_rendering_end()));
}

void MainWindow::update_workspace()
{
    update_window_title();

    // Enable/disable menus and widgets appropriately.
    set_file_widgets_enabled(true, RenderingMode::NotRendering);
    set_project_explorer_enabled(true);
    set_rendering_widgets_enabled(true, RenderingMode::NotRendering);
    set_diagnostics_widgets_enabled(true, RenderingMode::NotRendering);
    update_pause_resume_checkbox(false);
    m_ui->attribute_editor_scrollarea_contents->setEnabled(true);

    // Add/remove light paths tab.
    if (m_project_manager.is_project_open() &&
        m_project_manager.get_project()->get_light_path_recorder().get_light_path_count() > 0)
        add_light_paths_tab();
    else remove_light_paths_tab();
}

void MainWindow::update_project_explorer()
{
    delete m_project_explorer;
    m_project_explorer = nullptr;

    delete m_attribute_editor;
    m_attribute_editor = nullptr;

    if (m_project_manager.is_project_open())
    {
        m_attribute_editor =
            new AttributeEditor(
                m_ui->attribute_editor_scrollarea_contents,
                *m_project_manager.get_project(),
                m_application_settings);

        m_project_explorer =
            new ProjectExplorer(
                m_ui->treewidget_project_explorer_scene,
                m_attribute_editor,
                *m_project_manager.get_project(),
                m_project_manager,
                m_rendering_manager,
                m_application_settings);

        connect(
            m_project_explorer, SIGNAL(signal_project_modified()),
            SLOT(slot_project_modified()));

        connect(
            m_project_explorer, SIGNAL(signal_frame_modified()),
            SLOT(slot_frame_modified()));
    }

    m_ui->lineedit_filter->clear();
}

void MainWindow::update_window_title()
{
    QString title;

    if (m_project_manager.is_project_open())
    {
        if (m_project_manager.is_project_dirty())
            title.append("* ");

        title.append(QString::fromStdString(m_project_manager.get_project_display_name()));
        title.append(" - ");
    }

    title.append("appleseed.studio");

    setWindowTitle(title);
}

void MainWindow::set_file_widgets_enabled(const bool is_enabled, const RenderingMode rendering_mode)
{
    const bool is_project_open = m_project_manager.is_project_open();
    const bool project_has_path = is_project_open && m_project_manager.get_project()->has_path();

    // File -> New Project.
    m_ui->action_file_new_project->setEnabled(is_enabled);
    m_action_new_project->setEnabled(is_enabled);

    // File -> Open Project.
    m_ui->action_file_open_project->setEnabled(is_enabled);
    m_action_open_project->setEnabled(is_enabled);

    // File -> Open Recent.
    m_ui->menu_open_recent->setEnabled(is_enabled);

    // File -> Open Built-in Project.
    m_ui->menu_file_open_builtin_project->setEnabled(is_enabled);

    // File -> Reload Project.
    const bool allow_reload = (is_enabled || rendering_mode == RenderingMode::InteractiveRendering) && project_has_path;
    m_ui->action_file_reload_project->setEnabled(allow_reload);
    m_action_reload_project->setEnabled(allow_reload);

    // File -> Monitor Project.
    const bool allow_monitor = (is_enabled || rendering_mode == RenderingMode::InteractiveRendering) && project_has_path;
    m_ui->action_file_monitor_project->setEnabled(allow_monitor);
    m_action_monitor_project_file->setEnabled(allow_monitor);

    // File -> Save Project, Save Project As and Pack Project As.
    const bool allow_save = is_enabled && is_project_open;
    m_ui->action_file_save_project->setEnabled(allow_save);
    m_action_save_project->setEnabled(allow_save);
    m_ui->action_file_save_project_as->setEnabled(allow_save);
    m_ui->action_file_pack_project_as->setEnabled(allow_save);

    // File -> Close Project.
    const bool allow_close = is_enabled && is_project_open;
    m_ui->action_file_close_project->setEnabled(allow_close);

    // File -> Exit.
    m_ui->action_file_exit->setEnabled(is_enabled);
}

void MainWindow::set_project_explorer_enabled(const bool is_enabled)
{
    const bool is_project_open = m_project_manager.is_project_open();

    m_ui->label_filter->setEnabled(is_enabled && is_project_open);
    m_ui->lineedit_filter->setEnabled(is_enabled && is_project_open);
    m_ui->pushbutton_clear_filter->setEnabled(is_enabled && is_project_open && !m_ui->lineedit_filter->text().isEmpty());
    m_ui->treewidget_project_explorer_scene->setEnabled(is_enabled && is_project_open);
}

void MainWindow::set_rendering_widgets_enabled(const bool is_enabled, const RenderingMode rendering_mode)
{
    const bool is_project_open = m_project_manager.is_project_open();
    const bool allow_start = is_enabled && is_project_open && rendering_mode == RenderingMode::NotRendering;
    const bool allow_stop = is_enabled && is_project_open && rendering_mode != RenderingMode::NotRendering;

    // Rendering -> Start Interactive Rendering.
    m_ui->action_rendering_start_interactive_rendering->setEnabled(allow_start);
    m_action_start_interactive_rendering->setEnabled(allow_start);

    // Rendering -> Start Final Rendering.
    m_ui->action_rendering_start_final_rendering->setEnabled(allow_start);
    m_action_start_final_rendering->setEnabled(allow_start);

    // Rendering -> Pause/Resume Rendering.
    m_ui->action_rendering_pause_resume_rendering->setEnabled(allow_stop);
    m_action_pause_resume_rendering->setEnabled(allow_stop);

    // Rendering -> Stop Rendering.
    m_ui->action_rendering_stop_rendering->setEnabled(allow_stop);
    m_action_stop_rendering->setEnabled(allow_stop);

    // Rendering -> Rendering Settings.
    m_ui->action_rendering_rendering_settings->setEnabled(allow_start);
    m_action_rendering_settings->setEnabled(allow_start);

    // Render tab buttons.
    const int current_tab_index = m_ui->tab_render_channels->currentIndex();
    if (current_tab_index != -1)
    {
        const auto render_tab_it = m_tab_index_to_render_tab.find(current_tab_index);
        if (render_tab_it != m_tab_index_to_render_tab.end())
        {
            RenderTab* render_tab = render_tab_it->second;

            // Clear frame.
            render_tab->set_clear_frame_button_enabled(
                is_enabled && is_project_open && rendering_mode == RenderingMode::NotRendering);

            // Set/clear rendering region.
            render_tab->set_render_region_buttons_enabled(
                is_enabled && is_project_open && rendering_mode != RenderingMode::FinalRendering);

            // Scene picker.
            render_tab->get_scene_picking_handler()->set_enabled(
                is_enabled && is_project_open && rendering_mode != RenderingMode::FinalRendering);
        }
    }
}

void MainWindow::set_diagnostics_widgets_enabled(const bool is_enabled, const RenderingMode rendering_mode)
{
    const bool is_project_open = m_project_manager.is_project_open();

    m_ui->menu_diagnostics_override_shading->setEnabled(is_enabled && is_project_open);
    m_ui->action_diagnostics_false_colors->setEnabled(is_enabled && is_project_open && rendering_mode == RenderingMode::NotRendering);
}

void MainWindow::save_state_before_project_open()
{
    m_state_before_project_open.reset(new StateBeforeProjectOpen());

    m_state_before_project_open->m_is_rendering = m_rendering_manager.is_rendering();

    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        m_state_before_project_open->m_render_tab_states[i->first] = i->second->save_state();
}

void MainWindow::restore_state_after_project_open()
{
    if (m_state_before_project_open.get())
    {
        for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        {
            const RenderTabStateCollection& tab_states = m_state_before_project_open->m_render_tab_states;
            const RenderTabStateCollection::const_iterator tab_state_it = tab_states.find(i->first);

            if (tab_state_it != tab_states.end())
                i->second->load_state(tab_state_it->second);
        }

        if (m_state_before_project_open->m_is_rendering)
            start_rendering(RenderingMode::InteractiveRendering);
    }
}

void MainWindow::recreate_render_tabs()
{
    remove_render_tabs();

    if (m_project_manager.is_project_open())
        add_render_tab("RGB");
}

void MainWindow::remove_render_tabs()
{
    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        delete i->second;

    m_render_tabs.clear();
    m_tab_index_to_render_tab.clear();

    while (m_ui->tab_render_channels->count() > 0)
        m_ui->tab_render_channels->removeTab(0);
}

void MainWindow::add_render_tab(const QString& label)
{
    // Create render tab.
    RenderTab* render_tab =
        new RenderTab(
            *m_project_explorer,
            *m_project_manager.get_project(),
            m_rendering_manager,
            m_ocio_config);

    // Connect the render tab to the main window and the rendering manager.
    connect(
        render_tab, SIGNAL(signal_render_widget_context_menu(const QPoint&)),
        SLOT(slot_render_widget_context_menu(const QPoint&)));
    connect(
        render_tab, SIGNAL(signal_set_render_region(const QRect&)),
        SLOT(slot_set_render_region(const QRect&)));
    connect(
        render_tab, SIGNAL(signal_clear_render_region()),
        SLOT(slot_clear_render_region()));
    connect(
        render_tab, SIGNAL(signal_save_frame_and_aovs()),
        SLOT(slot_save_frame_and_aovs()));
    connect(
        render_tab, SIGNAL(signal_quicksave_frame_and_aovs()),
        SLOT(slot_quicksave_frame_and_aovs()));
    connect(
        render_tab, SIGNAL(signal_reset_zoom()),
        SLOT(slot_reset_zoom()));
    connect(
        render_tab, SIGNAL(signal_clear_frame()),
        SLOT(slot_clear_frame()));
    connect(
        render_tab, SIGNAL(signal_entity_picked(renderer::ScenePicker::PickingResult)),
        SLOT(slot_clear_filter()));
    connect(
        render_tab, SIGNAL(signal_camera_change_begin()),
        &m_rendering_manager, SLOT(slot_camera_change_begin()));
    connect(
        render_tab, SIGNAL(signal_camera_change_end()),
        &m_rendering_manager, SLOT(slot_camera_change_end()));
    connect(
        render_tab, SIGNAL(signal_camera_changed()),
        &m_rendering_manager, SLOT(slot_camera_changed()));

    // Add the render tab to the tab bar.
    const int tab_index = m_ui->tab_render_channels->addTab(render_tab, label);

    // Update mappings.
    m_render_tabs[label.toStdString()] = render_tab;
    m_tab_index_to_render_tab[tab_index] = render_tab;
}

void MainWindow::add_light_paths_tab()
{
    if (m_light_paths_tab == nullptr)
    {
        // Create light paths tab.
        m_light_paths_tab =
            new LightPathsTab(
                *m_project_manager.get_project(),
                m_application_settings);

        // Connect render tabs to the light paths tab.
        for (const auto& kv : m_render_tabs)
        {
            connect(
                kv.second, SIGNAL(signal_entity_picked(renderer::ScenePicker::PickingResult)),
                m_light_paths_tab, SLOT(slot_entity_picked(renderer::ScenePicker::PickingResult)));
            connect(
                kv.second, SIGNAL(signal_rectangle_selection(const QRect&)),
                m_light_paths_tab, SLOT(slot_rectangle_selection(const QRect&)));
        }

        // Add the light paths tab to the tab bar.
        m_ui->tab_render_channels->addTab(m_light_paths_tab, "Light Paths");
    }
}

void MainWindow::remove_light_paths_tab()
{
    if (m_light_paths_tab != nullptr)
    {
        delete m_light_paths_tab;
        m_light_paths_tab = nullptr;
    }
}

ParamArray MainWindow::get_project_params(const char* configuration_name) const
{
    ParamArray params;

    // Retrieve the configuration.
    Configuration* configuration =
        m_project_manager.is_project_open()
            ? m_project_manager.get_project()->configurations().get_by_name(configuration_name)
            : nullptr;

    // Start with the parameters from the base configuration.
    if (configuration && configuration->get_base())
        params = configuration->get_base()->get_parameters();

    // Override with application settings.
    params.merge(m_application_settings);

    // Override with parameters from the configuration.
    if (configuration)
        params.merge(configuration->get_parameters());

    return params;
}

namespace
{
    int show_modified_project_message_box(QWidget* parent)
    {
        QMessageBox msgbox(parent);
        msgbox.setWindowTitle("Save Changes?");
        msgbox.setIcon(QMessageBox::Question);
        msgbox.setText("The project has been modified.\n\nDo you want to save your changes?");
        msgbox.setStandardButtons(QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
        msgbox.setDefaultButton(QMessageBox::Save);
        return msgbox.exec();
    }
}

bool MainWindow::can_close_project()
{
    // Project being loaded: can't close.
    if (m_project_manager.is_project_loading())
        return false;

    // No project open: no problem.
    if (!m_project_manager.is_project_open())
        return true;

    // Unmodified project: no problem.
    if (!m_project_manager.is_project_dirty())
        return true;

    // The current project has been modified, ask the user what to do.
    switch (show_modified_project_message_box(this))
    {
      case QMessageBox::Save:
        slot_save_project();
        return true;

      case QMessageBox::Discard:
        return true;

      case QMessageBox::Cancel:
        return false;
    }

    assert(!"Should never be reached.");
    return false;
}

void MainWindow::on_project_change()
{
    update_project_explorer();
    recreate_render_tabs();

    update_override_shading_menu_item();
    m_false_colors_window.reset();

    if (m_rendering_settings_window.get() != nullptr &&
        m_project_manager.get_project() != nullptr)
        m_rendering_settings_window->reload();

    m_status_bar.clear();

    update_workspace();

    restore_state_after_project_open();

    if (m_project_file_watcher)
        start_monitoring_project_file();
}

void MainWindow::enable_project_file_monitoring()
{
    if (m_project_file_watcher == nullptr)
    {
        m_project_file_watcher = new QFileSystemWatcher(this);

        connect(
            m_project_file_watcher,
            SIGNAL(fileChanged(const QString&)),
            SLOT(slot_project_file_changed(const QString&)));

        RENDERER_LOG_INFO("project file monitoring is now enabled.");

        start_monitoring_project_file();
    }
}

void MainWindow::disable_project_file_monitoring()
{
    if (m_project_file_watcher)
    {
        delete m_project_file_watcher;
        m_project_file_watcher = nullptr;

        RENDERER_LOG_INFO("project file monitoring is now disabled.");
    }
}

void MainWindow::start_monitoring_project_file()
{
    assert(m_project_file_watcher);

    if (m_project_manager.is_project_open() &&
        m_project_manager.get_project()->has_path())
    {
        m_project_file_watcher->addPath(m_project_manager.get_project()->get_path());
    }
}

void MainWindow::stop_monitoring_project_file()
{
    assert(m_project_file_watcher);

    if (m_project_manager.is_project_open() &&
        m_project_manager.get_project()->has_path())
    {
        m_project_file_watcher->removePath(m_project_manager.get_project()->get_path());
    }
}

void MainWindow::dragEnterEvent(QDragEnterEvent* event)
{
    if (event->mimeData()->hasFormat("text/uri-list"))
        event->acceptProposedAction();
}

void MainWindow::dropEvent(QDropEvent* event)
{
    if (event->mimeData()->hasFormat("text/uri-list"))
    {
        const QList<QUrl> urls = event->mimeData()->urls();
        QApplication::sendEvent(this, new QCloseEvent());
        open_project_async(urls[0].toLocalFile());
        event->acceptProposedAction();
    }
}

void MainWindow::start_rendering(const RenderingMode rendering_mode)
{
    assert(m_project_manager.is_project_open());

    // Don't start a new render until the previous has completely ended.
    if (m_rendering_manager.is_rendering())
        return;

    m_false_colors_window.reset();

    // Enable/disable menus and widgets appropriately.
    set_file_widgets_enabled(false, rendering_mode);
    set_project_explorer_enabled(rendering_mode == RenderingMode::InteractiveRendering);
    set_rendering_widgets_enabled(true, rendering_mode);
    set_diagnostics_widgets_enabled(rendering_mode == RenderingMode::InteractiveRendering, rendering_mode);
    m_ui->attribute_editor_scrollarea_contents->setEnabled(rendering_mode == RenderingMode::InteractiveRendering);

    // Remove light paths tab.
    remove_light_paths_tab();

    // Stop monitoring the project file in Final rendering mode.
    if (rendering_mode == RenderingMode::FinalRendering)
    {
        if (m_project_file_watcher)
            stop_monitoring_project_file();
    }

    Project* project = m_project_manager.get_project();
    Frame* frame = project->get_frame();

    frame->clear_main_and_aov_images();

    // Darken render widgets.
    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
    {
        i->second->darken();
        i->second->update();
    }

    // Retrieve the appropriate rendering configuration.
    const char* configuration_name =
        rendering_mode == RenderingMode::InteractiveRendering ? "interactive" : "final";
    const ParamArray params = get_project_params(configuration_name);

    if (!project->has_texture_store())
        project->initialize_texture_store(params.child("texture_store"));

    // Effectively start rendering.
    m_rendering_manager.start_rendering(
        project,
        params,
        rendering_mode == RenderingMode::InteractiveRendering
            ? RenderingManager::RenderingMode::InteractiveRendering
            : RenderingManager::RenderingMode::FinalRendering,
        m_render_tabs["RGB"]);
}

void MainWindow::apply_false_colors_settings()
{
    Project* project = m_project_manager.get_project();
    assert(project != nullptr);

    Frame* frame = project->get_frame();
    assert(frame != nullptr);

    const ParamArray& false_colors_params = m_application_settings.child("false_colors");
    const bool false_colors_enabled = false_colors_params.get_optional<bool>("enabled", false);

    if (false_colors_enabled)
    {
        // Make a temporary copy of the frame.
        // Render info, AOVs and other data are not copied.
        // todo: creating a frame with denoising enabled is very expensive, see benchmark_frame.cpp.
        auto_release_ptr<Frame> working_frame =
            FrameFactory::create(
                (std::string(frame->get_name()) + "_copy").c_str(),
                frame->get_parameters()
                    .remove_path("denoiser"));
        working_frame->image().copy_from(frame->image());

        // Create post-processing stage.
        auto_release_ptr<PostProcessingStage> stage(
            ColorMapPostProcessingStageFactory().create(
                "__false_colors_post_processing_stage",
                false_colors_params));

        // Apply post-processing stage.
        apply_post_processing_stage(stage.ref(), working_frame.ref());
    }
    else
    {
        // Blit the regular frame into the render widget.
        for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        {
            i->second->get_render_widget()->blit_frame(*frame);
            i->second->get_render_widget()->update();
        }
    }
}

void MainWindow::apply_post_processing_stage(
    PostProcessingStage&        stage,
    Frame&                      working_frame)
{
    Project* project = m_project_manager.get_project();
    assert(project != nullptr);

    // Prepare the post-processing stage.
    OnFrameBeginRecorder recorder;
    if (stage.on_frame_begin(*project, nullptr, recorder, nullptr))
    {
        // Execute the post-processing stage.
        stage.execute(working_frame);

        // Blit the frame copy into the render widget.
        for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        {
            i->second->get_render_widget()->blit_frame(working_frame);
            i->second->get_render_widget()->update();
        }
    }
}

namespace
{
    int ask_abort_rendering_confirmation(QWidget* parent)
    {
        QMessageBox msgbox(parent);
        msgbox.setWindowTitle("Abort Rendering?");
        msgbox.setIcon(QMessageBox::Question);
        msgbox.setText("Rendering is in progress.\n\nDo you want to abort rendering?");
        msgbox.setStandardButtons(QMessageBox::Yes | QMessageBox::No);
        msgbox.setDefaultButton(QMessageBox::No);
        return msgbox.exec();
    }
}

void MainWindow::closeEvent(QCloseEvent* event)
{
    if (m_rendering_manager.is_rendering())
    {
        if (ask_abort_rendering_confirmation(this) != QMessageBox::Yes)
        {
            event->ignore();
            return;
        }

        m_rendering_manager.abort_rendering();
        m_rendering_manager.wait_until_rendering_end();
    }

    if (!can_close_project())
    {
        event->ignore();
        return;
    }

    slot_save_application_settings();

    if (m_test_window.get())
        m_test_window->close();

    if (m_benchmark_window.get())
        m_benchmark_window->close();

    remove_render_tabs();

    m_project_manager.close_project();

    event->accept();
}

void MainWindow::slot_new_project()
{
    if (!can_close_project())
        return;

    new_project();
}

void MainWindow::slot_open_project()
{
    if (!can_close_project())
        return;

    QString filepath =
        get_open_filename(
            this,
            "Open...",
            get_project_files_filter(),
            m_application_settings,
            SETTINGS_FILE_DIALOG_PROJECTS);

    if (!filepath.isEmpty())
    {
        open_project_async(filepath);
        update_recent_files_menu(filepath);
    }
}

void MainWindow::slot_open_recent()
{
    if (!can_close_project())
        return;

    QAction* action = qobject_cast<QAction*>(sender());

    if (action)
    {
        const QString filepath = action->data().toString();
        open_project_async(filepath);
    }
}

void MainWindow::slot_clear_all_recent_project_files()
{
    QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    settings.setValue("recent_file_list", QStringList());

    update_recent_files_menu(QStringList());
}

void MainWindow::slot_clear_recent_missing_project_files()
{
    QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    QStringList files = settings.value("recent_file_list").toStringList();
    QStringList existing_files;

    for (int i = 0; i < files.size(); i++)
    {
        if (QFileInfo(files[i]).isFile())
            existing_files << files[i];
    }

    settings.setValue("recent_file_list", existing_files);
    update_recent_files_menu(existing_files);
}

void MainWindow::slot_open_cornellbox_builtin_project()
{
    if (!can_close_project())
        return;

    APPLESEED_UNUSED const bool successful = m_project_manager.load_builtin_project("cornell_box");
    assert(successful);

    on_project_change();
}

void MainWindow::slot_reload_project()
{
    assert(m_project_manager.is_project_open());
    assert(m_project_manager.get_project()->has_path());

    if (!can_close_project())
        return;

    open_project_async(m_project_manager.get_project()->get_path());
}

namespace
{
    void show_project_file_loading_failed_message_box(QWidget* parent, const QString& filepath)
    {
        QMessageBox msgbox(parent);
        msgbox.setWindowTitle("Loading Error");
        msgbox.setIcon(QMessageBox::Critical);
        msgbox.setText("Failed to load the project file " + filepath + ".");
        msgbox.setInformativeText(
            "The project file may be invalid, corrupted or missing. "
            "Please look at the Log window for details.");
        msgbox.setStandardButtons(QMessageBox::Ok);
        msgbox.exec();
    }
}

void MainWindow::slot_open_project_complete(const QString& filepath, const bool successful)
{
    if (successful)
        on_project_change();
    else
    {
        show_project_file_loading_failed_message_box(this, filepath);
        recreate_render_tabs();
        update_workspace();
    }
}

void MainWindow::slot_save_project()
{
    assert(m_project_manager.is_project_open());

    if (!m_project_manager.get_project()->has_path())
        slot_save_project_as();
    else save_project(m_project_manager.get_project()->get_path());
}

void MainWindow::slot_save_project_as()
{
    assert(m_project_manager.is_project_open());

    QString filepath =
        get_save_filename(
            this,
            "Save As...",
            get_project_files_filter(ProjectFilesFilterPlainProjects),
            m_application_settings,
            SETTINGS_FILE_DIALOG_PROJECTS);

    if (!filepath.isEmpty())
    {
        save_project(filepath);
    }
}

void MainWindow::slot_pack_project_as()
{
    assert(m_project_manager.is_project_open());

    QString filepath =
        get_save_filename(
            this,
            "Pack As...",
            get_project_files_filter(ProjectFilesFilterPackedProjects),
            m_application_settings,
            SETTINGS_FILE_DIALOG_PROJECTS);

    if (!filepath.isEmpty())
    {
        filepath = QDir::toNativeSeparators(filepath);

        pack_project(filepath);

        // Don't update the Recent Files menu.
    }
}

void MainWindow::slot_close_project()
{
    if (!can_close_project())
        return;

    close_project();
}

void MainWindow::initialize_ocio()
{
    // Try first a user specified OCIO config.
    if (getenv("OCIO"))
    {
        try
        {
            m_ocio_config = OCIO::GetCurrentConfig();
            RENDERER_LOG_INFO("using ocio configuration: %s", getenv("OCIO"));
            return;
        }
        catch (const OCIO::Exception&)
        {
        }
    }

    // Try the bundled default OCIO config.
    const bf::path root_path(Application::get_root_path());
    const std::string default_ocio_config = (root_path / "ocio" / "config.ocio").string();

    try
    {
        m_ocio_config = OCIO::Config::CreateFromFile(default_ocio_config.c_str());
        RENDERER_LOG_INFO("using ocio configuration: %s", default_ocio_config.c_str());
        OCIO::SetCurrentConfig(m_ocio_config);
        return;
    }
    catch (const OCIO::Exception&)
    {
    }

    // Default to an empty OCIO config if everything else fails.
    m_ocio_config = OCIO::GetCurrentConfig();
    RENDERER_LOG_ERROR("could not find an ocio configuration, using empty configuration.");
}

void MainWindow::slot_project_modified()
{
    assert(m_project_manager.is_project_open());

    m_project_manager.set_project_dirty_flag();

    update_window_title();
}

void MainWindow::slot_toggle_project_file_monitoring(const bool checked)
{
    if (checked)
        enable_project_file_monitoring();
    else disable_project_file_monitoring();

    m_application_settings.insert_path(
        SETTINGS_WATCH_FILE_CHANGES,
        m_project_file_watcher != nullptr);
}

void MainWindow::slot_project_file_changed(const QString& filepath)
{
    RENDERER_LOG_INFO("project file changed on disk, reloading it.");

    assert(m_project_file_watcher);
    m_project_file_watcher->removePath(filepath);

    slot_reload_project();
}

namespace
{
    const char* SettingsFilename = "appleseed.studio.xml";
}

void MainWindow::slot_load_application_settings()
{
    const QSettings qt_settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    restoreGeometry(qt_settings.value("main_window_geometry").toByteArray());
    restoreState(qt_settings.value("main_window_state").toByteArray());
    m_ui->treewidget_project_explorer_scene->header()->restoreGeometry(
        qt_settings.value("main_window_project_explorer_geometry").toByteArray());
    m_ui->treewidget_project_explorer_scene->header()->restoreState(
        qt_settings.value("main_window_project_explorer_state").toByteArray());

    Dictionary settings;
    if (Application::load_settings(SettingsFilename, settings, global_logger(), LogMessage::Info))
    {
        m_application_settings = settings;
        slot_apply_application_settings();
    }
}

void MainWindow::slot_save_application_settings()
{
    QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    settings.setValue("main_window_geometry", saveGeometry());
    settings.setValue("main_window_state", saveState());
    settings.setValue("main_window_project_explorer_geometry",
        m_ui->treewidget_project_explorer_scene->header()->saveGeometry());
    settings.setValue("main_window_project_explorer_state",
        m_ui->treewidget_project_explorer_scene->header()->saveState());

    Application::save_settings(SettingsFilename, m_application_settings, global_logger(), LogMessage::Info);
}

void MainWindow::slot_apply_application_settings()
{
    if (m_application_settings.strings().exist(SETTINGS_MESSAGE_VERBOSITY))
    {
        const char* level_name = m_application_settings.get(SETTINGS_MESSAGE_VERBOSITY);
        const LogMessage::Category level = LogMessage::get_category_value(level_name);

        if (level < LogMessage::NumMessageCategories)
            global_logger().set_verbosity_level(level);
        else RENDERER_LOG_ERROR("invalid message verbosity level \"%s\".", level_name);
    }

    if (m_application_settings.get_optional<bool>(SETTINGS_WATCH_FILE_CHANGES))
    {
        m_action_monitor_project_file->setChecked(true);
        enable_project_file_monitoring();
    }
    else
    {
        m_action_monitor_project_file->setChecked(false);
        disable_project_file_monitoring();
    }

    emit signal_application_settings_modified();
}

void MainWindow::slot_start_interactive_rendering()
{
    start_rendering(RenderingMode::InteractiveRendering);
}

void MainWindow::slot_start_final_rendering()
{
    start_rendering(RenderingMode::FinalRendering);
}

void MainWindow::slot_start_rendering_once(const QString& filepath, const QString& configuration, const bool successful)
{
    sender()->deleteLater();

    if (successful)
    {
        if (configuration == "interactive")
            start_rendering(RenderingMode::InteractiveRendering);
        else start_rendering(RenderingMode::FinalRendering);
    }
}

void MainWindow::slot_pause_or_resume_rendering(const bool checked)
{
    if (checked)
    {
        assert(!m_rendering_manager.is_rendering_paused());
        m_rendering_manager.pause_rendering();
    }
    else
    {
        m_rendering_manager.resume_rendering();
    }

    update_pause_resume_checkbox(checked);
}

void MainWindow::slot_rendering_end()
{
    // todo: ideally we wouldn't apply false colors when aborting a final render.
    apply_false_colors_settings();

    update_workspace();

    // Restart monitoring the project file if monitoring was enabled
    // (monitoring would have been stopped if rendering in Final mode).
    if (m_project_file_watcher)
        start_monitoring_project_file();
}

void MainWindow::slot_camera_changed()
{
    m_project_manager.set_project_dirty_flag();
}

namespace
{
    class ClearShadingOverrideAction
      : public RenderingManager::IStickyAction
    {
      public:
        void operator()(
            MasterRenderer& master_renderer,
            Project&        project) override
        {
            master_renderer.get_parameters()
                .push("shading_engine")
                .dictionaries().remove("override_shading");
        }
    };

    class SetShadingOverrideAction
      : public RenderingManager::IStickyAction
    {
      public:
        explicit SetShadingOverrideAction(const std::string& shading_mode)
          : m_shading_mode(shading_mode)
        {
        }

        void operator()(
            MasterRenderer& master_renderer,
            Project&        project) override
        {
            master_renderer.get_parameters()
                .push("shading_engine")
                .push("override_shading")
                .insert("mode", m_shading_mode);
        }

      private:
        const std::string m_shading_mode;
    };
}

void MainWindow::slot_clear_shading_override()
{
    m_rendering_manager.set_sticky_action(
        "override_shading",
        std::unique_ptr<RenderingManager::IStickyAction>(
            new ClearShadingOverrideAction()));

    m_rendering_manager.reinitialize_rendering();
}

void MainWindow::slot_set_shading_override()
{
    QAction* action = qobject_cast<QAction*>(sender());
    const std::string shading_mode = action->data().toString().toStdString();

    m_rendering_manager.set_sticky_action(
        "override_shading",
        std::unique_ptr<RenderingManager::IStickyAction>(
            new SetShadingOverrideAction(shading_mode)));

    m_rendering_manager.reinitialize_rendering();
}

void MainWindow::slot_show_false_colors_window()
{
    if (m_false_colors_window.get() == nullptr)
    {
        m_false_colors_window.reset(new FalseColorsWindow(this));

        QObject::connect(
            m_false_colors_window.get(), SIGNAL(signal_applied(foundation::Dictionary)),
            SLOT(slot_apply_false_colors_settings_changes(foundation::Dictionary)));

        QObject::connect(
            m_false_colors_window.get(), SIGNAL(signal_accepted(foundation::Dictionary)),
            SLOT(slot_apply_false_colors_settings_changes(foundation::Dictionary)));

        QObject::connect(
            m_false_colors_window.get(), SIGNAL(signal_canceled(foundation::Dictionary)),
            SLOT(slot_apply_false_colors_settings_changes(foundation::Dictionary)));
    }

    Project* project = m_project_manager.get_project();
    assert(project);

    m_false_colors_window->initialize(
        *project,
        m_application_settings,
        m_application_settings.child("false_colors"));

    m_false_colors_window->showNormal();
    m_false_colors_window->activateWindow();
}

void MainWindow::slot_apply_false_colors_settings_changes(Dictionary values)
{
    m_application_settings.push("false_colors").merge(values);
    apply_false_colors_settings();
}

namespace
{
    class ClearRenderRegionAction
      : public RenderingManager::IScheduledAction
    {
      public:
        explicit ClearRenderRegionAction(const AttributeEditor* attribute_editor)
          : m_attribute_editor(attribute_editor)
        {
        }

        void operator()(
            Project&        project) override
        {
            project.get_frame()->reset_crop_window();

            m_attribute_editor->refresh();
        }

      private:
        const AttributeEditor* m_attribute_editor;
    };

    class SetRenderRegionAction
      : public RenderingManager::IScheduledAction
    {
      public:
        SetRenderRegionAction(
            const QRect&            rect,
            const AttributeEditor*  attribute_editor)
          : m_rect(rect),
            m_attribute_editor(attribute_editor)
        {
        }

        void operator()(
            Project&        project) override
        {
            const int w = m_rect.width();
            const int h = m_rect.height();
            const int x0 = m_rect.x();
            const int y0 = m_rect.y();
            const int x1 = x0 + w - 1;
            const int y1 = y0 + h - 1;

            assert(x0 >= 0);
            assert(y0 >= 0);
            assert(x0 <= x1);
            assert(y0 <= y1);

            project.get_frame()->set_crop_window(
                AABB2i(
                    Vector2i(x0, y0),
                    Vector2i(x1, y1)));

            m_attribute_editor->refresh();
        }

      private:
        const QRect m_rect;
        const AttributeEditor* m_attribute_editor;
    };
}

void MainWindow::slot_clear_render_region()
{
    std::unique_ptr<RenderingManager::IScheduledAction> clear_render_region_action(
        new ClearRenderRegionAction(m_attribute_editor));

    if (m_rendering_manager.is_rendering())
        m_rendering_manager.schedule(std::move(clear_render_region_action));
    else clear_render_region_action->operator()(*m_project_manager.get_project());

    m_rendering_manager.reinitialize_rendering();
}

void MainWindow::slot_set_render_region(const QRect& rect)
{
    std::unique_ptr<RenderingManager::IScheduledAction> set_render_region_action(
        new SetRenderRegionAction(rect, m_attribute_editor));

    if (!m_rendering_manager.is_rendering())
    {
        set_render_region_action->operator()(*m_project_manager.get_project());

        if (m_application_settings.get_path_optional<bool>(SETTINGS_RENDER_REGION_TRIGGERS_RENDERING))
            start_rendering(RenderingMode::InteractiveRendering);
    }
    else
    {
        m_rendering_manager.schedule(std::move(set_render_region_action));
        m_rendering_manager.reinitialize_rendering();
    }
}

void MainWindow::slot_render_widget_context_menu(const QPoint& point)
{
    if (!(QApplication::keyboardModifiers() & Qt::ShiftModifier))
        return;

    if (m_rendering_manager.is_rendering())
        return;

    QMenu* menu = new QMenu(this);
    menu->addAction("Save &Frame...", this, SLOT(slot_save_frame()));
    menu->addAction("Save Frame and &AOVs...", this, SLOT(slot_save_frame_and_aovs()));
    menu->addSeparator();
    menu->addAction("Save &Render Widget Content...", this, SLOT(slot_save_render_widget_content()));
    menu->addSeparator();
    menu->addAction("&Clear All", this, SLOT(slot_clear_frame()));
    menu->exec(point);
}

namespace
{
    QString ask_frame_save_file_path(
        QWidget*        parent,
        const QString&  caption,
        const QString&  filter,
        const QString&  default_ext,
        ParamArray&     settings)
    {
        QString filepath =
            get_save_filename(
                parent,
                caption,
                filter,
                settings,
                SETTINGS_FILE_DIALOG_FRAMES);

        if (!filepath.isEmpty())
        {
            if (QFileInfo(filepath).suffix().isEmpty())
                filepath += default_ext;

            filepath = QDir::toNativeSeparators(filepath);
        }

        return filepath;
    }
}

void MainWindow::slot_save_frame()
{
    assert(m_project_manager.is_project_open());
    assert(!m_rendering_manager.is_rendering());

    const QString filepath =
        ask_frame_save_file_path(
            this,
            "Save Frame As...",
            get_oiio_image_files_filter(),
            ".exr",
            m_application_settings);

    if (filepath.isEmpty())
        return;

    const Frame* frame = m_project_manager.get_project()->get_frame();
    frame->write_main_image(filepath.toUtf8().constData());
}

void MainWindow::slot_save_frame_and_aovs()
{
    assert(m_project_manager.is_project_open());
    assert(!m_rendering_manager.is_rendering());

    const QString filepath =
        ask_frame_save_file_path(
            this,
            "Save Frame and AOVs As...",
            get_oiio_image_files_filter(),
            ".exr",
            m_application_settings);

    if (filepath.isEmpty())
        return;

    const Frame* frame = m_project_manager.get_project()->get_frame();
    frame->write_main_image(filepath.toUtf8().constData());
    frame->write_aov_images(filepath.toUtf8().constData());
}

namespace
{
    void write_main_and_aov_images(
        const Project&  project,
        const bf::path& image_path)
    {
        bf::create_directories(image_path.parent_path());

        const Frame* frame = project.get_frame();
        frame->write_main_image(image_path.string().c_str());
        frame->write_aov_images(image_path.string().c_str());
    }
}

void MainWindow::slot_quicksave_frame_and_aovs()
{
    assert(m_project_manager.is_project_open());
    assert(!m_rendering_manager.is_rendering());

    const Project& project = *m_project_manager.get_project();

    const bf::path project_path(project.get_path());
    const bf::path quicksave_dir = project_path.parent_path() / "quicksaves";

    write_main_and_aov_images(
        project,
        bf::absolute(
            quicksave_dir / "quicksave.exr"));

    write_main_and_aov_images(
        project,
        bf::absolute(
            find_next_available_path(quicksave_dir / "quicksave####.exr")));
}

void MainWindow::slot_save_render_widget_content()
{
    assert(m_project_manager.is_project_open());
    assert(!m_rendering_manager.is_rendering());

    const QString filepath =
        ask_frame_save_file_path(
            this,
            "Save Render Widget Content As...",
            g_qt_image_files_filter,
            ".png",
            m_application_settings);

    if (filepath.isEmpty())
        return;

    // todo: this is sketchy. The render tab should be retrieved from the signal.
    m_render_tabs["RGB"]->get_render_widget()->capture().save(filepath);

    RENDERER_LOG_INFO("wrote image file %s.", filepath.toStdString().c_str());
}

void MainWindow::slot_clear_frame()
{
    Frame* frame = m_project_manager.get_project()->get_frame();
    frame->clear_main_and_aov_images();

    // Clear all render widgets to black.
    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        i->second->clear();
}

void MainWindow::slot_reset_zoom()
{
    const int current_tab_index = m_ui->tab_render_channels->currentIndex();
    const auto render_tab_it = m_tab_index_to_render_tab.find(current_tab_index);
    if (render_tab_it != m_tab_index_to_render_tab.end())
        render_tab_it->second->reset_zoom();
}

void MainWindow::slot_filter_text_changed(const QString& pattern)
{
    m_ui->pushbutton_clear_filter->setEnabled(!pattern.isEmpty());
    m_project_explorer->filter_items(pattern);
}

void MainWindow::slot_clear_filter()
{
    m_ui->lineedit_filter->clear();
}

void MainWindow::slot_frame_modified()
{
    for (each<RenderTabCollection> i = m_render_tabs; i; ++i)
        i->second->update_size();
}

void MainWindow::slot_fullscreen()
{
    m_fullscreen = !m_fullscreen;

    bool all_minimized = true;
    bool not_minimized = false;
    for (const MinimizeButton* button : m_minimize_buttons)
    {
        all_minimized = all_minimized && button->is_on();
        not_minimized = not_minimized || !button->is_on();
    }

    // All were manually minimized, exit full screen mode.
    if (all_minimized)
        m_fullscreen = false;

    // At least one is on screen, enter full screen mode.
    if (not_minimized)
        m_fullscreen = true;

    for (MinimizeButton* button : m_minimize_buttons)
        button->set_fullscreen(m_fullscreen);
}

void MainWindow::slot_check_fullscreen()
{
    const QList<QDockWidget*> dock_widgets = findChildren<QDockWidget*>();

    const bool is_fullscreen =
        std::all_of(std::begin(dock_widgets), std::end(dock_widgets), [](QDockWidget* dock) { return dock->isHidden(); });

    m_action_fullscreen->setChecked(is_fullscreen);
}

void MainWindow::slot_show_application_settings_window()
{
    if (m_application_settings_window.get() == nullptr)
    {
        m_application_settings_window.reset(
            new ApplicationSettingsWindow(m_application_settings, this));

        connect(
            m_application_settings_window.get(), SIGNAL(signal_application_settings_modified()),
            SLOT(slot_save_application_settings()));

        connect(
            m_application_settings_window.get(), SIGNAL(signal_application_settings_modified()),
            SLOT(slot_apply_application_settings()));

        connect(
            this, SIGNAL(signal_application_settings_modified()),
            m_application_settings_window.get(), SLOT(slot_reload_application_settings()));
    }

    m_application_settings_window->showNormal();
    m_application_settings_window->activateWindow();
}

void MainWindow::slot_show_rendering_settings_window()
{
    assert(m_project_manager.is_project_open());

    if (m_rendering_settings_window.get() == nullptr)
    {
        m_rendering_settings_window.reset(
            new RenderingSettingsWindow(
                m_project_manager,
                m_application_settings,
                this));

        connect(
            m_rendering_settings_window.get(), SIGNAL(signal_rendering_settings_modified()),
            SLOT(slot_project_modified()));

        connect(
            this, SIGNAL(signal_application_settings_modified()),
            m_rendering_settings_window.get(), SLOT(slot_reload_application_settings()));
    }

    m_rendering_settings_window->showNormal();
    m_rendering_settings_window->activateWindow();
}

void MainWindow::slot_show_test_window()
{
    if (m_test_window.get() == nullptr)
        m_test_window.reset(new TestWindow(this));

    m_test_window->showNormal();
    m_test_window->activateWindow();
}

void MainWindow::slot_show_benchmark_window()
{
    if (m_benchmark_window.get() == nullptr)
        m_benchmark_window.reset(new BenchmarkWindow(this));

    m_benchmark_window->showNormal();
    m_benchmark_window->activateWindow();
}

void MainWindow::slot_show_about_window()
{
    // This window deletes itself on close.
    AboutWindow* about_window = new AboutWindow(this);
    about_window->showNormal();
    about_window->activateWindow();
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/moc_cpp_mainwindow.cxx"
