
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

// Interface header.
#include "mainwindow.h"

// UI definition header.
#include "ui_mainwindow.h"

// appleseed.studio headers.
#include "help/about/aboutwindow.h"
#include "mainwindow/project/projectexplorer.h"
#include "mainwindow/logwidget.h"
#include "utility/interop.h"
#include "utility/settingskeys.h"
#include "utility/tweaks.h"

// appleseed.shared headers.
#include "application/application.h"

// appleseed.renderer headers.
#include "renderer/api/aov.h"
#include "renderer/api/frame.h"
#include "renderer/api/log.h"
#include "renderer/api/project.h"
#include "renderer/api/rendering.h"
#include "renderer/api/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/system.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/settings.h"

// Qt headers.
#include <QAction>
#include <QActionGroup>
#include <QApplication>
#include <QCloseEvent>
#include <QDir>
#include <QFileDialog>
#include <QFont>
#include <QIcon>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QMenu>
#include <QMessageBox>
#include <QRect>
#include <QSettings>
#include <QStatusBar>
#include <QString>
#include <QStringList>
#include <Qt>

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>

using namespace appleseed::shared;
using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

//
// MainWindow class implementation.
//

MainWindow::MainWindow(QWidget* parent)
  : QMainWindow(parent)
  , m_ui(new Ui::MainWindow())
  , m_rendering_manager(m_status_bar)
  , m_project_explorer(0)
{
    m_ui->setupUi(this);

    statusBar()->addWidget(&m_status_bar);

    build_menus();
    build_toolbar();
    build_log();
    build_project_explorer();

    build_connections();

    print_startup_information();
    slot_load_settings();

    update_project_explorer();
    remove_render_widgets();
    update_workspace();

    showMaximized();
}

MainWindow::~MainWindow()
{
    delete m_project_explorer;
    delete m_ui;
}

void MainWindow::open_project(const QString& filepath)
{
    save_state_before_project_open();

    if (m_rendering_manager.is_rendering())
    {
        m_rendering_manager.abort_rendering();
        m_rendering_manager.wait_until_rendering_end();
    }

    set_file_widgets_enabled(false);
    set_project_explorer_enabled(false);
    set_rendering_widgets_enabled(false, false);

    const filesystem::path path(filepath.toStdString());

    m_settings.insert_path(
        LAST_DIRECTORY_SETTINGS_KEY,
        path.parent_path().string());

    m_project_manager.load_project(filepath.toAscii().constData());
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

void MainWindow::open_and_render_project(const QString& filepath, const QString& configuration)
{
    CustomSignalMapper* mapper = new CustomSignalMapper(this, configuration);

    connect(
        &m_project_manager, SIGNAL(signal_load_project_async_complete(const QString&, const bool)),
        mapper, SLOT(map(const QString&, const bool)));

    connect(
        mapper, SIGNAL(mapped(const QString&, const QString&, const bool)),
        SLOT(slot_start_rendering_once(const QString&, const QString&, const bool)));

    open_project(filepath);
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

    m_ui->action_file_save_project->setShortcut(QKeySequence::Save);
    connect(m_ui->action_file_save_project, SIGNAL(triggered()), SLOT(slot_save_project()));

    m_ui->action_file_save_project_as->setShortcut(QKeySequence::SaveAs);
    connect(m_ui->action_file_save_project_as, SIGNAL(triggered()), SLOT(slot_save_project_as()));

    m_ui->action_file_exit->setShortcut(QKeySequence::Quit);
    connect(m_ui->action_file_exit, SIGNAL(triggered()), SLOT(close()));

    //
    // View menu.
    //

    m_ui->menu_view->addAction(m_ui->project_explorer->toggleViewAction());
    m_ui->menu_view->addAction(m_ui->log->toggleViewAction());

    //
    // Rendering menu.
    //

    connect(m_ui->action_rendering_start_interactive_rendering, SIGNAL(triggered()), SLOT(slot_start_interactive_rendering()));
    connect(m_ui->action_rendering_start_final_rendering, SIGNAL(triggered()), SLOT(slot_start_final_rendering()));
    connect(m_ui->action_rendering_stop_rendering, SIGNAL(triggered()), &m_rendering_manager, SLOT(slot_abort_rendering()));
    connect(m_ui->action_rendering_render_settings, SIGNAL(triggered()), SLOT(slot_show_render_settings_window()));

    //
    // Diagnostics menu.
    //

    build_override_shading_menu_item();

    //
    // Debug menu.
    //

    connect(m_ui->action_debug_tests, SIGNAL(triggered()), SLOT(slot_show_test_window()));
    connect(m_ui->action_debug_benchmarks, SIGNAL(triggered()), SLOT(slot_show_benchmark_window()));

    //
    // Tools menu.
    //

    connect(m_ui->action_tools_save_settings, SIGNAL(triggered()), SLOT(slot_save_settings()));
    connect(m_ui->action_tools_reload_settings, SIGNAL(triggered()), SLOT(slot_load_settings()));

    //
    // Help menu.
    //

    connect(m_ui->action_help_about, SIGNAL(triggered()), SLOT(slot_show_about_window()));
}

void MainWindow::build_override_shading_menu_item()
{
    QActionGroup* action_group = new QActionGroup(this);

    connect(
        m_ui->action_diagnostics_override_shading_no_override, SIGNAL(triggered()),
        SLOT(slot_clear_shading_override()));

    action_group->addAction(m_ui->action_diagnostics_override_shading_no_override);

    for (int i = 0; i < DiagnosticSurfaceShader::ShadingModeCount; ++i)
    {
        const char* shading_mode_value = DiagnosticSurfaceShader::ShadingModeNames[i].m_key;
        const char* shading_mode_name = DiagnosticSurfaceShader::ShadingModeNames[i].m_value;

        QAction* action = new QAction(this);

        action->setCheckable(true);

        action->setObjectName(
            QString::fromAscii("action_diagnostics_override_shading_") + shading_mode_value);

        action->setText(
            QApplication::translate(
                objectName().toAscii().constData(),
                shading_mode_name,
                0,
                QApplication::UnicodeUTF8));

        const int shortcut_number = i + 1;

        if (shortcut_number <= 9)
        {
            const QString shortcut =
                QApplication::translate(
                    objectName().toAscii().constData(),
                    QString::fromAscii("Ctrl+Shift+%1").arg(shortcut_number).toAscii().constData(),
                    0,
                    QApplication::UnicodeUTF8);

            action->setShortcut(shortcut);
        }

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
        const string shading_mode =
            shading_engine_params.child("override_shading").get_optional<string>("mode", "coverage");

        for (const_each<QList<QAction*> > i = m_ui->menu_diagnostics_override_shading->actions(); i; ++i)
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

namespace
{
    const int MaxRecentlyOpenedFiles = 5;
    const char* SettingsOrgString = "com.appleseed.studio";
    const char* SettingsRecentFilesEntryString = "appleseed.studio Recent Files";
    const char* SettingsRecentFileListString = "recent_file_list";
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

    QSettings settings(SettingsOrgString, SettingsRecentFilesEntryString);
    QStringList files = settings.value(SettingsRecentFileListString).toStringList();
    update_recent_files_menu(files);

    m_ui->menu_open_recent->addSeparator();
    m_clear_open_recent_menu = new QAction(this);
    m_clear_open_recent_menu->setText("&Clear Menu");
    connect(m_clear_open_recent_menu, SIGNAL(triggered()), SLOT(slot_clear_open_recent_files_menu()));
    m_ui->menu_open_recent->addAction(m_clear_open_recent_menu);
}

void MainWindow::update_recent_files_menu(const QString& filepath)
{
    QSettings settings(SettingsOrgString, SettingsRecentFilesEntryString);
    QStringList files = settings.value(SettingsRecentFileListString).toStringList();
    files.removeAll(filepath);
    files.prepend(filepath);

    while (files.size() > MaxRecentlyOpenedFiles)
        files.removeLast();

    settings.setValue(SettingsRecentFileListString, files);
    update_recent_files_menu(files);
}

void MainWindow::update_recent_files_menu(const QStringList& files)
{
    const int num_recent_files = min(files.size(), MaxRecentlyOpenedFiles);

    for (int i = 0; i < num_recent_files; ++i)
    {
        const QString filepath = files[i];
        const QString text = tr("&%1 %2").arg(i + 1).arg(filepath);

        m_recently_opened[i]->setText(text);
        m_recently_opened[i]->setData(filepath);
        m_recently_opened[i]->setVisible(true);
    }

    for (int i = num_recent_files; i < MaxRecentlyOpenedFiles; ++i)
        m_recently_opened[i]->setVisible(false);
}

void MainWindow::build_toolbar()
{
    m_action_new_project = new QAction(QIcon(":/icons/page_white.png"), "New", this);
    connect(m_action_new_project, SIGNAL(triggered()), SLOT(slot_new_project()));
    m_ui->main_toolbar->addAction(m_action_new_project);

    m_action_open_project = new QAction(QIcon(":/icons/folder.png"), "Open", this);
    connect(m_action_open_project, SIGNAL(triggered()), SLOT(slot_open_project()));
    m_ui->main_toolbar->addAction(m_action_open_project);

    m_action_save_project = new QAction(QIcon(":/icons/disk.png"), "Save", this);
    connect(m_action_save_project, SIGNAL(triggered()), SLOT(slot_save_project()));
    m_ui->main_toolbar->addAction(m_action_save_project);

    m_ui->main_toolbar->addSeparator();

    m_action_start_interactive_rendering = new QAction(QIcon(":/icons/film_go.png"), "Start Interactive Rendering", this);
    connect(m_action_start_interactive_rendering, SIGNAL(triggered()), SLOT(slot_start_interactive_rendering()));
    m_ui->main_toolbar->addAction(m_action_start_interactive_rendering);

    m_action_start_final_rendering = new QAction(QIcon(":/icons/cog_go.png"), "Start Final Rendering", this);
    connect(m_action_start_final_rendering, SIGNAL(triggered()), SLOT(slot_start_final_rendering()));
    m_ui->main_toolbar->addAction(m_action_start_final_rendering);

    m_action_stop_rendering = new QAction(QIcon(":/icons/cross.png"), "Stop Rendering", this);
    connect(m_action_stop_rendering, SIGNAL(triggered()), &m_rendering_manager, SLOT(slot_abort_rendering()));
    m_ui->main_toolbar->addAction(m_action_stop_rendering);
}

LogWidget* MainWindow::create_log_widget() const
{
    LogWidget* log_widget = new LogWidget(m_ui->log_contents);
    m_ui->log_contents->layout()->addWidget(log_widget);

    log_widget->setObjectName(QString::fromUtf8("textedit_log"));
    log_widget->setUndoRedoEnabled(false);
    log_widget->setLineWrapMode(QTextEdit::NoWrap);
    log_widget->setReadOnly(true);
    log_widget->setTextInteractionFlags(Qt::TextSelectableByMouse);
    log_widget->setStyleSheet("QTextEdit { border: 0px; }");

    QFont font;
    font.setStyleHint(QFont::TypeWriter);
#if defined _WIN32
    font.setFamily(QString::fromUtf8("Consolas"));
#elif defined __APPLE__
    font.setFamily(QString::fromUtf8("Monaco"));
#else
    font.setFamily(QString::fromUtf8("Courier New"));
#endif
    font.setPixelSize(11);
    log_widget->setFont(font);

    return log_widget;
}

void MainWindow::build_log()
{
    LogWidget* log_widget = create_log_widget();

    m_log_target.reset(new QtLogTarget(log_widget));

    global_logger().add_target(m_log_target.get());
}

void MainWindow::build_project_explorer()
{
    m_ui->treewidget_project_explorer_scene->setColumnWidth(0, 220);    // name
    m_ui->treewidget_project_explorer_scene->setColumnWidth(1, 75);     // render layer

    disable_mac_focus_rect(m_ui->treewidget_project_explorer_scene);
    disable_mac_focus_rect(m_ui->treewidget_project_explorer_renders);

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
        &m_project_manager, SIGNAL(signal_load_project_async_complete(const QString&, const bool)),
        SLOT(slot_open_project_complete(const QString&, const bool)));

    connect(
        &m_rendering_manager, SIGNAL(signal_rendering_end()),
        SLOT(slot_rendering_end()));

    connect(
        &m_rendering_manager, SIGNAL(signal_camera_changed()),
        SLOT(slot_camera_changed()));
}

void MainWindow::print_startup_information()
{
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

ParamArray MainWindow::get_project_params(const char* configuration_name) const
{
    ParamArray params;

    Configuration* configuration =
        m_project_manager.is_project_open()
            ? m_project_manager.get_project()->configurations().get_by_name(configuration_name)
            : 0;

    if (configuration && configuration->get_base())
        params = configuration->get_base()->get_parameters();

    params.merge(m_settings);

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
        msgbox.setText("The project has been modified.");
        msgbox.setInformativeText("Do you want to save your changes?");
        msgbox.setStandardButtons(QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
        msgbox.setDefaultButton(QMessageBox::Save);
        return msgbox.exec();
    }

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

void MainWindow::save_state_before_project_open()
{
    m_state_before_project_open.reset(new StateBeforeProjectOpen());

    m_state_before_project_open->m_is_rendering = m_rendering_manager.is_rendering();

    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        m_state_before_project_open->m_render_tab_states[i->first] = i->second->save_state();
}

void MainWindow::restore_state_after_project_open()
{
    assert(m_state_before_project_open.get());

    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
    {
        const RenderTabStateCollection& tab_states = m_state_before_project_open->m_render_tab_states;
        const RenderTabStateCollection::const_iterator tab_state_it = tab_states.find(i->first);

        if (tab_state_it != tab_states.end())
            i->second->load_state(tab_state_it->second);
    }

    if (m_state_before_project_open->m_is_rendering)
        start_rendering(true);
}

bool MainWindow::can_close_project()
{
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
    recreate_render_widgets();

    update_override_shading_menu_item();

    if (m_render_settings_window.get())
        m_render_settings_window->reload();

    m_status_bar.clear();

    update_workspace();

    restore_state_after_project_open();
}

void MainWindow::update_workspace()
{
    update_window_title();
    set_file_widgets_enabled(true);
    set_project_explorer_enabled(true);
    set_rendering_widgets_enabled(true, false);
}

void MainWindow::update_project_explorer()
{
    delete m_project_explorer;

    if (m_project_manager.is_project_open())
    {
        m_project_explorer =
            new ProjectExplorer(
                m_ui->treewidget_project_explorer_scene,    
                *m_project_manager.get_project(),
                m_rendering_manager,
                m_settings);

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

void MainWindow::set_file_widgets_enabled(const bool is_enabled)
{
    const bool is_project_open = m_project_manager.is_project_open();

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
    m_ui->action_file_reload_project->setEnabled(
        is_enabled &&
        is_project_open &&
        m_project_manager.get_project()->has_path());

    // File -> Save Project and Save Project As.
    m_ui->action_file_save_project->setEnabled(is_enabled && is_project_open);
    m_action_save_project->setEnabled(is_enabled && is_project_open);
    m_ui->action_file_save_project_as->setEnabled(is_enabled && is_project_open);
}

void MainWindow::set_project_explorer_enabled(const bool is_enabled)
{
    const bool is_project_open = m_project_manager.is_project_open();

    m_ui->label_filter->setEnabled(is_enabled && is_project_open);
    m_ui->lineedit_filter->setEnabled(is_enabled && is_project_open);
    m_ui->pushbutton_clear_filter->setEnabled(is_enabled && is_project_open);
    m_ui->treewidget_project_explorer_scene->setEnabled(is_enabled && is_project_open);
}

void MainWindow::set_rendering_widgets_enabled(const bool is_enabled, const bool is_rendering)
{
    const bool is_project_open = is_enabled && m_project_manager.is_project_open();
    const bool allow_starting_rendering = is_project_open && !is_rendering;
    const bool allow_stopping_rendering = is_project_open && is_rendering;

    // Rendering -> Start Interactive Rendering.
    m_ui->action_rendering_start_interactive_rendering->setEnabled(allow_starting_rendering);
    m_action_start_interactive_rendering->setEnabled(allow_starting_rendering);

    // Rendering -> Start Final Rendering.
    m_ui->action_rendering_start_final_rendering->setEnabled(allow_starting_rendering);
    m_action_start_final_rendering->setEnabled(allow_starting_rendering);

    // Rendering -> Stop Rendering.
    m_ui->action_rendering_stop_rendering->setEnabled(allow_stopping_rendering);
    m_action_stop_rendering->setEnabled(allow_stopping_rendering);

    // Rendering -> Render Settings.
    m_ui->action_rendering_render_settings->setEnabled(allow_starting_rendering);
}

void MainWindow::recreate_render_widgets()
{
    remove_render_widgets();

    if (m_project_manager.is_project_open())
    {
        add_render_widget("RGB");
        add_render_widget("Alpha");
        add_render_widget("Depth");
        add_render_widget("Anomalies");
    }
}

void MainWindow::remove_render_widgets()
{
    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        delete i->second;

    m_render_tabs.clear();

    while (m_ui->tab_render_channels->count() > 0)
        m_ui->tab_render_channels->removeTab(0);
}

void MainWindow::add_render_widget(const QString& label)
{
    // Create the render tab.
    RenderTab* render_tab =
        new RenderTab(
            *m_project_explorer,
            *m_project_manager.get_project());
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
        render_tab, SIGNAL(signal_save_aovs()),
        SLOT(slot_save_all_aovs()));

    // Add the render tab to the tab bar.
    m_ui->tab_render_channels->addTab(render_tab, label);

    // Update the label -> render tab mapping.
    m_render_tabs[label.toStdString()] = render_tab;
}

void MainWindow::start_rendering(const bool interactive)
{
    assert(m_project_manager.is_project_open());

    // Enable/disable widgets appropriately. File -> Reload is enabled during interactive rendering.
    set_file_widgets_enabled(false);
    if (interactive)
        m_ui->action_file_reload_project->setEnabled(true);
    set_project_explorer_enabled(true);
    set_rendering_widgets_enabled(true, true);

    // Internally, clear the main image to transparent black and delete all AOV images.
    Project* project = m_project_manager.get_project();
    project->get_frame()->clear_main_image();
    project->get_frame()->aov_images().clear();

    // In the UI, darken all render widgets.
    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        i->second->darken();

    const char* configuration_name = interactive ? "interactive" : "final";
    const ParamArray params = get_project_params(configuration_name);

    m_rendering_manager.start_rendering(
        project,
        params,
        interactive,
        m_render_tabs["RGB"]->get_render_widget());
}

namespace
{
    int ask_abort_rendering_confirmation(QWidget* parent)
    {
        QMessageBox msgbox(parent);
        msgbox.setWindowTitle("Abort Rendering?");
        msgbox.setIcon(QMessageBox::Question);
        msgbox.setText("Rendering is in progress.");
        msgbox.setInformativeText("Do you want to abort rendering?");
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

    m_project_manager.close_project();

    slot_save_settings();

    event->accept();
}

void MainWindow::slot_new_project()
{
    if (!can_close_project())
        return;

    m_project_manager.create_project();

    on_project_change();
}

void MainWindow::slot_open_project()
{
    if (!can_close_project())
        return;

    QFileDialog::Options options;
    QString selected_filter;

    QString filepath =
        QFileDialog::getOpenFileName(
            this,
            "Open...",
            m_settings.get_path_optional<QString>(LAST_DIRECTORY_SETTINGS_KEY),
            "Project Files (*.appleseed);;All Files (*.*)",
            &selected_filter,
            options);

    if (!filepath.isEmpty())
    {
        filepath = QDir::toNativeSeparators(filepath);

        open_project(filepath);
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
        open_project(filepath);
    }
}

void MainWindow::slot_clear_open_recent_files_menu()
{
    QSettings settings(SettingsOrgString, SettingsRecentFilesEntryString);
    QStringList files;
    settings.setValue(SettingsRecentFileListString, files);
    update_recent_files_menu(files);
}

void MainWindow::slot_open_cornellbox_builtin_project()
{
    if (!can_close_project())
        return;

    const bool successful = m_project_manager.load_builtin_project("cornell_box");
    assert(successful);

    on_project_change();
}

void MainWindow::slot_reload_project()
{
    assert(m_project_manager.is_project_open());
    assert(m_project_manager.get_project()->has_path());

    if (!can_close_project())
        return;

    open_project(m_project_manager.get_project()->get_path());
}

void MainWindow::slot_open_project_complete(const QString& filepath, const bool successful)
{
    if (successful)
    {
        on_project_change();
    }
    else
    {
        show_project_file_loading_failed_message_box(this, filepath);
        update_workspace();
    }
}

void MainWindow::slot_save_project()
{
    assert(m_project_manager.is_project_open());

    if (!m_project_manager.get_project()->has_path())
    {
        slot_save_project_as();
        return;
    }

    m_project_manager.save_project();

    update_workspace();
}

void MainWindow::slot_save_project_as()
{
    assert(m_project_manager.is_project_open());

    QFileDialog::Options options;
    QString selected_filter;

    QString filepath =
        QFileDialog::getSaveFileName(
            this,
            "Save As...",
            m_settings.get_path_optional<QString>(LAST_DIRECTORY_SETTINGS_KEY),
            "Project Files (*.appleseed)",
            &selected_filter,
            options);

    if (!filepath.isEmpty())
    {
        filepath = QDir::toNativeSeparators(filepath);

        const filesystem::path path(filepath.toStdString());

        m_settings.insert_path(
            LAST_DIRECTORY_SETTINGS_KEY,
            path.parent_path().string());

        m_project_manager.save_project_as(filepath.toAscii().constData());

        update_recent_files_menu(filepath);
        update_workspace();
    }
}

void MainWindow::slot_project_modified()
{
    assert(m_project_manager.is_project_open());

    m_project_manager.set_project_dirty_flag();

    update_window_title();
}

void MainWindow::slot_frame_modified()
{
    for (each<RenderTabCollection> i = m_render_tabs; i; ++i)
        i->second->update_size();
}

void MainWindow::slot_start_interactive_rendering()
{
    start_rendering(true);
}

void MainWindow::slot_start_final_rendering()
{
    start_rendering(false);
}

void MainWindow::slot_start_rendering_once(const QString& filepath, const QString& configuration, const bool successful)
{
    sender()->deleteLater();

    if (successful)
    {
        const bool interactive = configuration == "interactive";
        start_rendering(interactive);
    }
}

void MainWindow::slot_rendering_end()
{
    update_workspace();
}

namespace
{
    class ClearShadingOverrideDelayedAction
      : public RenderingManager::IDelayedAction
    {
      public:
        virtual void operator()(
            MasterRenderer& master_renderer,
            Project&        project) OVERRIDE
        {
            master_renderer.get_parameters()
                .push("shading_engine")
                .dictionaries().remove("override_shading");
        }
    };

    class SetShadingOverrideDelayedAction
      : public RenderingManager::IDelayedAction
    {
      public:
        explicit SetShadingOverrideDelayedAction(const string& shading_mode)
          : m_shading_mode(shading_mode)
        {
        }

        virtual void operator()(
            MasterRenderer& master_renderer,
            Project&        project) OVERRIDE
        {
            master_renderer.get_parameters()
                .push("shading_engine")
                .push("override_shading")
                .insert("mode", m_shading_mode);
        }

      private:
        const string m_shading_mode;
    };
}

void MainWindow::slot_clear_shading_override()
{
    m_rendering_manager.set_permanent_state(
        "override_shading",
        auto_ptr<RenderingManager::IDelayedAction>(
            new ClearShadingOverrideDelayedAction()));

    m_rendering_manager.reinitialize_rendering();
}

void MainWindow::slot_set_shading_override()
{
    QAction* action = qobject_cast<QAction*>(sender());
    const string shading_mode = action->data().toString().toStdString();

    m_rendering_manager.set_permanent_state(
        "override_shading",
        auto_ptr<RenderingManager::IDelayedAction>(
            new SetShadingOverrideDelayedAction(shading_mode)));

    m_rendering_manager.reinitialize_rendering();
}

namespace
{
    class ClearRenderRegionDelayedAction
      : public RenderingManager::IDelayedAction
    {
      public:
        virtual void operator()(
            MasterRenderer& master_renderer,
            Project&        project) OVERRIDE
        {
            project.get_frame()->clear_crop_window();
        }
    };

    class SetRenderRegionDelayedAction
      : public RenderingManager::IDelayedAction
    {
      public:
        explicit SetRenderRegionDelayedAction(const QRect& rect)
          : m_rect(rect)
        {
        }

        virtual void operator()(
            MasterRenderer& master_renderer,
            Project&        project) OVERRIDE
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
        }

      private:
        const QRect m_rect;
    };
}

void MainWindow::slot_clear_render_region()
{
    m_rendering_manager.push_delayed_action(
        auto_ptr<RenderingManager::IDelayedAction>(
            new ClearRenderRegionDelayedAction()));

    m_rendering_manager.reinitialize_rendering();
}

void MainWindow::slot_set_render_region(const QRect& rect)
{
    m_rendering_manager.push_delayed_action(
        auto_ptr<RenderingManager::IDelayedAction>(
            new SetRenderRegionDelayedAction(rect)));

    if (m_settings.get_path_optional<bool>("ui.render_region.triggers_rendering"))
    {
        if (m_rendering_manager.is_rendering())
            m_rendering_manager.reinitialize_rendering();
        else start_rendering(true);
    }
    else m_rendering_manager.reinitialize_rendering();
}

void MainWindow::slot_camera_changed()
{
    m_project_manager.set_project_dirty_flag();
}

void MainWindow::slot_show_render_settings_window()
{
    assert(m_project_manager.is_project_open());

    if (m_render_settings_window.get() == 0)
    {
        m_render_settings_window.reset(new RenderSettingsWindow(m_project_manager, this));

        connect(
            m_render_settings_window.get(), SIGNAL(signal_settings_modified()),
            SLOT(slot_project_modified()));
    }

    m_render_settings_window->showNormal();
    m_render_settings_window->activateWindow();
}

void MainWindow::slot_show_test_window()
{
    if (m_test_window.get() == 0)
        m_test_window.reset(new TestWindow(this));

    m_test_window->showNormal();
    m_test_window->activateWindow();
}

void MainWindow::slot_show_benchmark_window()
{
    if (m_benchmark_window.get() == 0)
        m_benchmark_window.reset(new BenchmarkWindow(this));

    m_benchmark_window->showNormal();
    m_benchmark_window->activateWindow();
}

void MainWindow::slot_show_about_window()
{
    AboutWindow* about_window = new AboutWindow(this);
    about_window->showNormal();
    about_window->activateWindow();
}

void MainWindow::slot_load_settings()
{
    const filesystem::path root_path(Application::get_root_path());
    const string settings_file_path = (root_path / "settings" / "appleseed.studio.xml").string();
    const string schema_file_path = (root_path / "schemas" / "settings.xsd").string();

    SettingsFileReader reader(global_logger());

    Dictionary settings;

    const bool success =
        reader.read(
            settings_file_path.c_str(),
            schema_file_path.c_str(),
            settings);

    if (success)
    {
        RENDERER_LOG_INFO("successfully loaded settings from %s.", settings_file_path.c_str());
        m_settings = settings;
    }
    else
    {
        RENDERER_LOG_ERROR("failed to load settings from %s.", settings_file_path.c_str());
    }
}

void MainWindow::slot_save_settings()
{
    const filesystem::path root_path(Application::get_root_path());
    const string settings_file_path = (root_path / "settings" / "appleseed.studio.xml").string();

    SettingsFileWriter writer;

    const bool success =
        writer.write(
            settings_file_path.c_str(),
            m_settings);

    if (success)
        RENDERER_LOG_INFO("successfully saved settings to %s.", settings_file_path.c_str());
    else RENDERER_LOG_ERROR("failed to save settings to %s.", settings_file_path.c_str());
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

void MainWindow::slot_render_widget_context_menu(const QPoint& point)
{
    if (!(QApplication::keyboardModifiers() & Qt::ShiftModifier))
        return;

    if (m_rendering_manager.is_rendering())
        return;

    QMenu* menu = new QMenu(this);
    menu->addAction("Save Frame...", this, SLOT(slot_save_frame()));
    menu->addAction("Save All AOVs...", this, SLOT(slot_save_all_aovs()));
    menu->addSeparator();
    menu->addAction("Clear Frame", this, SLOT(slot_clear_frame()));

    menu->exec(point);
}

void MainWindow::slot_save_frame()
{
    assert(!m_rendering_manager.is_rendering());

    QFileDialog::Options options;
    QString selected_filter;

    QString filepath =
        QFileDialog::getSaveFileName(
            this,
            "Save Frame As...",
            m_settings.get_path_optional<QString>(LAST_DIRECTORY_SETTINGS_KEY),
            "Image Files (*.exr;*.png);;All Files (*.*)",
            &selected_filter,
            options);

    if (filepath.isEmpty())
        return;

    if (QFileInfo(filepath).suffix().isEmpty())
        filepath += ".exr";

    filepath = QDir::toNativeSeparators(filepath);

    const Frame* frame = m_project_manager.get_project()->get_frame();
    frame->write_main_image(filepath.toAscii().constData());
}

void MainWindow::slot_save_all_aovs()
{
    assert(!m_rendering_manager.is_rendering());

    QFileDialog::Options options;
    QString selected_filter;

    QString filepath =
        QFileDialog::getSaveFileName(
            this,
            "Save All AOVs As...",
            m_settings.get_path_optional<QString>(LAST_DIRECTORY_SETTINGS_KEY),
            "Image Files (*.exr;*.png);;All Files (*.*)",
            &selected_filter,
            options);

    if (filepath.isEmpty())
        return;

    if (QFileInfo(filepath).suffix().isEmpty())
        filepath += ".exr";

    filepath = QDir::toNativeSeparators(filepath);

    const Frame* frame = m_project_manager.get_project()->get_frame();
    frame->write_main_image(filepath.toAscii().constData());
    frame->write_aov_images(filepath.toAscii().constData());
}

void MainWindow::slot_clear_frame()
{
    // Internally, clear the main image to transparent black and delete all AOV images.
    Frame* frame = m_project_manager.get_project()->get_frame();
    frame->clear_main_image();
    frame->aov_images().clear();

    // In the UI, clear all render widgets to black.
    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        i->second->clear();
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/moc_cpp_mainwindow.cxx"
