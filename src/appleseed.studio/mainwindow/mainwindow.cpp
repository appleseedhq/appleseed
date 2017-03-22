
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

// Interface header.
#include "mainwindow.h"

// UI definition header.
#include "ui_mainwindow.h"

// appleseed.studio headers.
#include "help/about/aboutwindow.h"
#include "mainwindow/logwidget.h"
#include "mainwindow/minimizebutton.h"
#include "mainwindow/project/attributeeditor.h"
#include "mainwindow/project/projectexplorer.h"
#include "utility/interop.h"
#include "utility/miscellaneous.h"
#include "utility/settingskeys.h"

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
#include "foundation/utility/log/logmessage.h"
#include "foundation/utility/path.h"
#include "foundation/utility/settings.h"

// Qt headers.
#include <QAction>
#include <QActionGroup>
#include <QApplication>
#include <QCloseEvent>
#include <QDir>
#include <QFileInfo>
#include <QFileSystemWatcher>
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
#include <QToolButton>
#include <QUrl>

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>

using namespace appleseed::shared;
using namespace foundation;
using namespace renderer;
using namespace std;
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
  , m_project_explorer(0)
  , m_attribute_editor(0)
  , m_project_file_watcher(0)
{
    m_ui->setupUi(this);

    statusBar()->addWidget(&m_status_bar);

    build_menus();
    build_toolbar();
    build_log_panel();
    build_project_explorer();

    build_connections();

    const QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    restoreGeometry(settings.value("main_window_geometry").toByteArray());
    restoreState(settings.value("main_window_state").toByteArray());
    m_ui->treewidget_project_explorer_scene->header()->restoreGeometry(
        settings.value("main_window_project_explorer_geometry").toByteArray());
    m_ui->treewidget_project_explorer_scene->header()->restoreState(
        settings.value("main_window_project_explorer_state").toByteArray());

    print_startup_information();
    slot_load_settings();

    update_project_explorer();
    remove_render_widgets();
    update_workspace();

    build_minimize_buttons();

    setAcceptDrops(true);
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

    remove_render_widgets();

    set_file_widgets_enabled(false, NotRendering);
    set_project_explorer_enabled(false);
    set_rendering_widgets_enabled(false, NotRendering);

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

    connect(m_ui->action_monitor_project_file, SIGNAL(toggled(bool)), SLOT(slot_toggle_project_file_monitoring(const bool)));

    m_ui->action_file_exit->setShortcut(QKeySequence::Quit);
    connect(m_ui->action_file_exit, SIGNAL(triggered()), SLOT(close()));

    //
    // View menu.
    //

    m_ui->menu_view->addAction(m_ui->project_explorer->toggleViewAction());
    m_ui->menu_view->addAction(m_ui->attribute_editor->toggleViewAction());
    m_ui->menu_view->addAction(m_ui->log->toggleViewAction());
    m_ui->menu_view->addSeparator();

    QAction* fullscreen_action = m_ui->menu_view->addAction("Fullscreen");
    fullscreen_action->setShortcut(Qt::Key_F11);
    connect(fullscreen_action, SIGNAL(triggered()), SLOT(slot_fullscreen()));

    //
    // Rendering menu.
    //

    connect(m_ui->action_rendering_start_interactive_rendering, SIGNAL(triggered()), SLOT(slot_start_interactive_rendering()));
    connect(m_ui->action_rendering_start_final_rendering, SIGNAL(triggered()), SLOT(slot_start_final_rendering()));
    connect(m_ui->action_rendering_stop_rendering, SIGNAL(triggered()), &m_rendering_manager, SLOT(slot_abort_rendering()));
    connect(m_ui->action_rendering_rendering_settings, SIGNAL(triggered()), SLOT(slot_show_rendering_settings_window()));

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

    QAction* clear_open_recent_menu = new QAction(this);
    clear_open_recent_menu->setText("&Clear Menu");
    connect(clear_open_recent_menu, SIGNAL(triggered()), SLOT(slot_clear_open_recent_files_menu()));
    m_ui->menu_open_recent->addAction(clear_open_recent_menu);
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
    //
    // File actions.
    //

    m_action_new_project = new QAction(load_icons("project_new"), "New Project", this);
    connect(m_action_new_project, SIGNAL(triggered()), SLOT(slot_new_project()));
    m_ui->main_toolbar->addAction(m_action_new_project);

    m_action_open_project = new QAction(load_icons("project_open"), "Open Project...", this);
    connect(m_action_open_project, SIGNAL(triggered()), SLOT(slot_open_project()));
    m_ui->main_toolbar->addAction(m_action_open_project);

    m_action_save_project = new QAction(load_icons("project_save") , "Save Project", this);
    connect(m_action_save_project, SIGNAL(triggered()), SLOT(slot_save_project()));
    m_ui->main_toolbar->addAction(m_action_save_project);

    m_action_reload_project = new QAction(load_icons("project_reload"), "Reload Project", this);
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

    m_action_start_interactive_rendering = new QAction(load_icons("rendering_start_interactive"), "Start Interactive Rendering", this);
    connect(m_action_start_interactive_rendering, SIGNAL(triggered()), SLOT(slot_start_interactive_rendering()));
    m_ui->main_toolbar->addAction(m_action_start_interactive_rendering);

    m_action_start_final_rendering = new QAction(load_icons("rendering_start_final"), "Start Final Rendering", this);
    connect(m_action_start_final_rendering, SIGNAL(triggered()), SLOT(slot_start_final_rendering()));
    m_ui->main_toolbar->addAction(m_action_start_final_rendering);

    m_action_stop_rendering = new QAction(load_icons("rendering_stop"), "Stop Rendering", this);
    connect(m_action_stop_rendering, SIGNAL(triggered()), &m_rendering_manager, SLOT(slot_abort_rendering()));
    m_ui->main_toolbar->addAction(m_action_stop_rendering);

    m_action_rendering_settings = new QAction(load_icons("rendering_settings"), "Rendering Settings...", this);
    connect(m_action_rendering_settings, SIGNAL(triggered()), SLOT(slot_show_rendering_settings_window()));
    m_ui->main_toolbar->addAction(m_action_rendering_settings);
}

void MainWindow::build_log_panel()
{
    LogWidget* log_widget = new LogWidget(m_ui->log_contents);
    m_ui->log_contents->layout()->addWidget(log_widget);

    log_widget->setObjectName("textedit_log");
    log_widget->setUndoRedoEnabled(false);
    log_widget->setLineWrapMode(QTextEdit::NoWrap);
    log_widget->setReadOnly(true);
    log_widget->setTextInteractionFlags(Qt::TextSelectableByMouse);

    m_log_target.reset(new QtLogTarget(log_widget));

    global_logger().add_target(m_log_target.get());
}

void MainWindow::build_project_explorer()
{
    m_ui->treewidget_project_explorer_scene->setColumnWidth(0, 220);    // name
    m_ui->treewidget_project_explorer_scene->setColumnWidth(1, 75);     // render layer

    disable_osx_focus_rect(m_ui->treewidget_project_explorer_scene);

    connect(
        m_ui->lineedit_filter, SIGNAL(textChanged(const QString&)),
        SLOT(slot_filter_text_changed(const QString&)));

    connect(
        m_ui->pushbutton_clear_filter, SIGNAL(clicked()),
        SLOT(slot_clear_filter()));

    m_ui->pushbutton_clear_filter->setEnabled(false);
}

void MainWindow::build_minimize_buttons()
{
    m_minimize_buttons.push_back(new MinimizeButton(m_ui->project_explorer));
    m_minimize_buttons.push_back(new MinimizeButton(m_ui->attribute_editor));
    m_minimize_buttons.push_back(new MinimizeButton(m_ui->log));

    for (size_t i = 0; i < m_minimize_buttons.size(); ++i)
    {
        statusBar()->insertPermanentWidget(
            static_cast<int>(i + 1),
            m_minimize_buttons[i]);
    }
}

void MainWindow::build_connections()
{
    connect(
        m_action_monitor_project_file, SIGNAL(toggled(bool)),
        m_ui->action_monitor_project_file, SLOT(setChecked(bool)));

    connect(
        m_ui->action_monitor_project_file, SIGNAL(toggled(bool)),
        m_action_monitor_project_file, SLOT(setChecked(bool)));

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

void MainWindow::update_workspace()
{
    update_window_title();

    set_file_widgets_enabled(true, NotRendering);
    set_project_explorer_enabled(true);
    set_rendering_widgets_enabled(true, NotRendering);

    m_ui->attribute_editor_scrollarea_contents->setEnabled(true);
}

void MainWindow::update_project_explorer()
{
    delete m_project_explorer;
    m_project_explorer = 0;

    delete m_attribute_editor;
    m_attribute_editor = 0;

    if (m_project_manager.is_project_open())
    {
        m_attribute_editor =
            new AttributeEditor(
                m_ui->attribute_editor_scrollarea_contents,
                *m_project_manager.get_project());

        m_project_explorer =
            new ProjectExplorer(
                m_ui->treewidget_project_explorer_scene,
                m_attribute_editor,
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
    const bool allow_reload = (is_enabled || rendering_mode == InteractiveRendering) && project_has_path;
    m_ui->action_file_reload_project->setEnabled(allow_reload);
    m_action_reload_project->setEnabled(allow_reload);

    // File -> Monitor Project.
    const bool allow_monitor = (is_enabled || rendering_mode == InteractiveRendering) && project_has_path;
    m_ui->action_monitor_project_file->setEnabled(allow_monitor);
    m_action_monitor_project_file->setEnabled(allow_monitor);

    // File -> Save Project and Save Project As.
    const bool allow_save = is_enabled && is_project_open;
    m_ui->action_file_save_project->setEnabled(allow_save);
    m_action_save_project->setEnabled(allow_save);
    m_ui->action_file_save_project_as->setEnabled(allow_save);

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
    const bool allow_start = is_enabled && is_project_open && rendering_mode == NotRendering;
    const bool allow_stop = is_enabled && is_project_open && rendering_mode != NotRendering;

    // Rendering -> Rendering Settings.
    m_ui->action_rendering_rendering_settings->setEnabled(allow_start);
    m_action_rendering_settings->setEnabled(allow_start);

    // Rendering -> Start Interactive Rendering.
    m_ui->action_rendering_start_interactive_rendering->setEnabled(allow_start);
    m_action_start_interactive_rendering->setEnabled(allow_start);

    // Rendering -> Start Final Rendering.
    m_ui->action_rendering_start_final_rendering->setEnabled(allow_start);
    m_action_start_final_rendering->setEnabled(allow_start);

    // Rendering -> Stop Rendering.
    m_ui->action_rendering_stop_rendering->setEnabled(allow_stop);
    m_action_stop_rendering->setEnabled(allow_stop);

    // Rendering -> Render Settings.
    m_ui->action_rendering_rendering_settings->setEnabled(allow_start);

    // Render tab buttons.
    const int current_tab_index = m_ui->tab_render_channels->currentIndex();
    if (current_tab_index != -1)
    {
        RenderTab* render_tab = m_tab_index_to_render_tab[current_tab_index];

        // Clear frame.
        render_tab->set_clear_frame_button_enabled(
            is_enabled && is_project_open && rendering_mode == NotRendering);

        // Set/clear rendering region.
        render_tab->set_render_region_buttons_enabled(
            is_enabled && is_project_open && rendering_mode != FinalRendering);

        // Scene picker.
        render_tab->get_scene_picking_handler()->set_enabled(
            is_enabled && is_project_open && rendering_mode != FinalRendering);
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
            start_rendering(InteractiveRendering);
    }
}

void MainWindow::recreate_render_widgets()
{
    remove_render_widgets();

    if (m_project_manager.is_project_open())
        add_render_widget("RGB");
}

void MainWindow::remove_render_widgets()
{
    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        delete i->second;

    m_render_tabs.clear();
    m_tab_index_to_render_tab.clear();

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
        render_tab, SIGNAL(signal_save_all_aovs()),
        SLOT(slot_save_all_aovs()));
    connect(
        render_tab, SIGNAL(signal_quicksave_all_aovs()),
        SLOT(slot_quicksave_all_aovs()));
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
    connect(
        render_tab, SIGNAL(signal_camera_changed()),
        &m_rendering_manager, SIGNAL(signal_camera_changed()));

    // Add the render tab to the tab bar.
    const int tab_index = m_ui->tab_render_channels->addTab(render_tab, label);

    // Update the mappings.
    m_render_tabs[label.toStdString()] = render_tab;
    m_tab_index_to_render_tab[tab_index] = render_tab;
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
    recreate_render_widgets();

    update_override_shading_menu_item();

    if (m_rendering_settings_window.get())
        m_rendering_settings_window->reload();

    m_status_bar.clear();

    update_workspace();

    restore_state_after_project_open();

    if (m_project_file_watcher)
        start_monitoring_project_file();
}

void MainWindow::enable_project_file_monitoring()
{
    if (m_project_file_watcher == 0)
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
        m_project_file_watcher = 0;

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
    if (event->mimeData()->hasFormat("text/plain") || event->mimeData()->hasFormat("text/uri-list"))
         event->acceptProposedAction();
}

void MainWindow::dropEvent(QDropEvent* event)
{
    if (event->mimeData()->hasFormat("text/uri-list"))
    {
        const QList<QUrl> urls = event->mimeData()->urls();
        QApplication::sendEvent(this, new QCloseEvent());
        open_project(urls[0].toLocalFile());
    }
    else
    {
        const QString text = event->mimeData()->text();
        QApplication::sendEvent(this, new QCloseEvent());
        open_project(text);
    }

     event->accept();
}

void MainWindow::start_rendering(const RenderingMode rendering_mode)
{
    assert(m_project_manager.is_project_open());

    // Enable/disable widgets appropriately.
    set_file_widgets_enabled(false, rendering_mode);
    set_rendering_widgets_enabled(true, rendering_mode);
    set_project_explorer_enabled(rendering_mode == InteractiveRendering);
    m_ui->attribute_editor_scrollarea_contents->setEnabled(rendering_mode == InteractiveRendering);

    // Stop monitoring the project file in Final rendering mode.
    if (rendering_mode == FinalRendering)
    {
        if (m_project_file_watcher)
            stop_monitoring_project_file();
    }

    Project* project = m_project_manager.get_project();
    Frame* frame = project->get_frame();

    // Clear the main image to transparent black.
    frame->clear_main_image();

    // In the UI, darken all render widgets.
    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
    {
        i->second->darken();
        i->second->update();
    }

    // Retrieve the right configuration.
    const char* configuration_name =
        rendering_mode == InteractiveRendering ? "interactive" : "final";
    const ParamArray params = get_project_params(configuration_name);

    // Effectively start rendering.
    m_rendering_manager.start_rendering(
        project,
        params,
        rendering_mode == InteractiveRendering
            ? RenderingManager::InteractiveRendering
            : RenderingManager::FinalRendering,
        m_render_tabs["RGB"]);
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

    QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    settings.setValue("main_window_geometry", saveGeometry());
    settings.setValue("main_window_state", saveState());
    settings.setValue("main_window_project_explorer_geometry",
        m_ui->treewidget_project_explorer_scene->header()->saveGeometry());
    settings.setValue("main_window_project_explorer_state",
        m_ui->treewidget_project_explorer_scene->header()->saveState());

    slot_save_settings();

    if (m_test_window.get())
        m_test_window->close();

    if (m_benchmark_window.get())
        m_benchmark_window->close();

    remove_render_widgets();

    m_project_manager.close_project();

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

    QString filepath =
        get_open_filename(
            this,
            "Open...",
            "Project Files (*.appleseed ; *.appleseedz);;Plain Project Files (*.appleseed);;Packed Project Files (*.appleseedz);;All Files (*.*)",
            m_settings,
            SETTINGS_FILE_DIALOG_PROJECTS);

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
    QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    settings.setValue("recent_file_list", QStringList());

    update_recent_files_menu(QStringList());
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

    open_project(m_project_manager.get_project()->get_path());
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
    {
        on_project_change();
    }
    else
    {
        show_project_file_loading_failed_message_box(this, filepath);
        recreate_render_widgets();
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

    if (m_project_file_watcher)
        stop_monitoring_project_file();

    m_project_manager.save_project();

    if (m_project_file_watcher)
        start_monitoring_project_file();

    update_workspace();
}

void MainWindow::slot_save_project_as()
{
    assert(m_project_manager.is_project_open());

    QString filepath =
        get_save_filename(
            this,
            "Save As...",
            "Project Files (*.appleseed)",
            m_settings,
            SETTINGS_FILE_DIALOG_PROJECTS);

    if (!filepath.isEmpty())
    {
        if (QFileInfo(filepath).suffix().isEmpty())
            filepath += ".appleseed";

        filepath = QDir::toNativeSeparators(filepath);

        if (m_project_file_watcher)
            stop_monitoring_project_file();

        m_project_manager.save_project_as(filepath.toAscii().constData());

        if (m_project_file_watcher)
            start_monitoring_project_file();

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

void MainWindow::slot_toggle_project_file_monitoring(const bool checked)
{
    if (checked)
        enable_project_file_monitoring();
    else disable_project_file_monitoring();

    m_settings.insert_path(
        SETTINGS_WATCH_FILE_CHANGES,
        m_project_file_watcher != 0);
}

void MainWindow::slot_project_file_changed(const QString& filepath)
{
    RENDERER_LOG_INFO("project file changed on disk, reloading it.");

    assert(m_project_file_watcher);
    m_project_file_watcher->removePath(filepath);

    slot_reload_project();
}

void MainWindow::slot_load_settings()
{
    Dictionary settings;

    if (!Application::load_settings("appleseed.studio.xml", settings, global_logger(), LogMessage::Info))
        return;

    m_settings = settings;

    if (m_settings.strings().exist("message_verbosity"))
    {
        const char* level_name = m_settings.get("message_verbosity");
        const LogMessage::Category level = LogMessage::get_category_value(level_name);

        if (level < LogMessage::NumMessageCategories)
            global_logger().set_verbosity_level(level);
        else RENDERER_LOG_ERROR("invalid message verbosity level \"%s\".", level_name);
    }

    if (m_settings.get_optional<bool>(SETTINGS_WATCH_FILE_CHANGES))
    {
        m_action_monitor_project_file->setChecked(true);
        enable_project_file_monitoring();
    }
    else
    {
        m_action_monitor_project_file->setChecked(false);
        disable_project_file_monitoring();
    }
}

void MainWindow::slot_save_settings()
{
    SettingsFileWriter writer;

    // First try to save the settings to the user path.
    if (const char* p = Application::get_user_settings_path())
    {
        try
        {
            const bf::path user_settings_path(p);
            bf::create_directories(user_settings_path);
            const string user_settings_file_path = (user_settings_path / "appleseed.studio.xml").string();

            if (writer.write(user_settings_file_path.c_str(), m_settings))
            {
                RENDERER_LOG_INFO("successfully saved settings to %s.", user_settings_file_path.c_str());
                return;
            }
        }
        catch (const bf::filesystem_error&)
        {
        }
    }

    // As a fallback, try to save the settings to the appleseed's installation directory.
    const bf::path root_path(Application::get_root_path());
    const string settings_file_path = (root_path / "settings" / "appleseed.studio.xml").string();

    if (writer.write(settings_file_path.c_str(), m_settings))
    {
        RENDERER_LOG_INFO("successfully saved settings to %s.", settings_file_path.c_str());
        return;
    }

    RENDERER_LOG_ERROR("failed to save settings to %s.", settings_file_path.c_str());
}

void MainWindow::slot_start_interactive_rendering()
{
    start_rendering(InteractiveRendering);
}

void MainWindow::slot_start_final_rendering()
{
    start_rendering(FinalRendering);
}

void MainWindow::slot_start_rendering_once(const QString& filepath, const QString& configuration, const bool successful)
{
    sender()->deleteLater();

    if (successful)
    {
        if (configuration == "interactive")
            start_rendering(InteractiveRendering);
        else start_rendering(FinalRendering);
    }
}

void MainWindow::slot_rendering_end()
{
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
        virtual void operator()(
            MasterRenderer& master_renderer,
            Project&        project) APPLESEED_OVERRIDE
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
        explicit SetShadingOverrideAction(const string& shading_mode)
          : m_shading_mode(shading_mode)
        {
        }

        virtual void operator()(
            MasterRenderer& master_renderer,
            Project&        project) APPLESEED_OVERRIDE
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
    m_rendering_manager.set_sticky_action(
        "override_shading",
        auto_ptr<RenderingManager::IStickyAction>(
            new ClearShadingOverrideAction()));

    m_rendering_manager.reinitialize_rendering();
}

void MainWindow::slot_set_shading_override()
{
    QAction* action = qobject_cast<QAction*>(sender());
    const string shading_mode = action->data().toString().toStdString();

    m_rendering_manager.set_sticky_action(
        "override_shading",
        auto_ptr<RenderingManager::IStickyAction>(
            new SetShadingOverrideAction(shading_mode)));

    m_rendering_manager.reinitialize_rendering();
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

        virtual void operator()(
            Project&        project) APPLESEED_OVERRIDE
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

        virtual void operator()(
            Project&        project) APPLESEED_OVERRIDE
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
    auto_ptr<RenderingManager::IScheduledAction> clear_render_region_action(
        new ClearRenderRegionAction(m_attribute_editor));

    if (m_rendering_manager.is_rendering())
        m_rendering_manager.schedule(clear_render_region_action);
    else clear_render_region_action.get()->operator()(
        *m_project_manager.get_project());

    m_rendering_manager.reinitialize_rendering();
}

void MainWindow::slot_set_render_region(const QRect& rect)
{
    auto_ptr<RenderingManager::IScheduledAction> set_render_region_action(
        new SetRenderRegionAction(rect, m_attribute_editor));

    if (!m_rendering_manager.is_rendering())
    {
        set_render_region_action.get()->operator()(
            *m_project_manager.get_project());

        if (m_settings.get_path_optional<bool>(SETTINGS_RENDER_REGION_TRIGGERS_RENDERING))
            start_rendering(InteractiveRendering);
    }
    else
    {
        m_rendering_manager.schedule(set_render_region_action);
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
    menu->addAction("Save Frame...", this, SLOT(slot_save_frame()));
    menu->addAction("Save All AOVs...", this, SLOT(slot_save_all_aovs()));
    menu->addSeparator();
    menu->addAction("Clear Frame", this, SLOT(slot_clear_frame()));

    menu->exec(point);
}

namespace
{
    QString ask_frame_save_file_path(
        QWidget*        parent,
        const QString&  caption,
        ParamArray&     settings)
    {
        QString filepath =
            get_save_filename(
                parent,
                caption,
                g_appleseed_image_files_filter,
                settings,
                SETTINGS_FILE_DIALOG_FRAMES);

        if (!filepath.isEmpty())
        {
            if (QFileInfo(filepath).suffix().isEmpty())
                filepath += ".exr";

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
        ask_frame_save_file_path(this, "Save Frame As...", m_settings);

    if (filepath.isEmpty())
        return;

    const Frame* frame = m_project_manager.get_project()->get_frame();
    frame->write_main_image(filepath.toAscii().constData());
}

void MainWindow::slot_save_all_aovs()
{
    assert(m_project_manager.is_project_open());
    assert(!m_rendering_manager.is_rendering());

    const QString filepath =
        ask_frame_save_file_path(this, "Save All AOVs As...", m_settings);

    if (filepath.isEmpty())
        return;

    const Frame* frame = m_project_manager.get_project()->get_frame();
    frame->write_main_image(filepath.toAscii().constData());
    frame->write_aov_images(filepath.toAscii().constData());
}

namespace
{
    void write_all_aovs(
        const Project&  project,
        const bf::path& image_path)
    {
        bf::create_directories(image_path.parent_path());

        const Frame* frame = project.get_frame();

        frame->write_main_image(image_path.string().c_str());
        frame->write_aov_images(image_path.string().c_str());
    }
}

void MainWindow::slot_quicksave_all_aovs()
{
    assert(m_project_manager.is_project_open());
    assert(!m_rendering_manager.is_rendering());

    const Project& project = *m_project_manager.get_project();

    const bf::path project_path(project.get_path());
    const bf::path quicksave_dir = project_path.parent_path() / "quicksaves";

    write_all_aovs(
        project,
        bf::absolute(
            quicksave_dir / "quicksave.exr"));

    write_all_aovs(
        project,
        bf::absolute(
            find_next_available_path(quicksave_dir / "quicksave####.exr")));
}

void MainWindow::slot_clear_frame()
{
    // Clear the main image to transparent black.
    Frame* frame = m_project_manager.get_project()->get_frame();
    frame->clear_main_image();

    // In the UI, clear all render widgets to black.
    for (const_each<RenderTabCollection> i = m_render_tabs; i; ++i)
        i->second->clear();
}

void MainWindow::slot_reset_zoom()
{
    const int current_tab_index = m_ui->tab_render_channels->currentIndex();
    m_tab_index_to_render_tab[current_tab_index]->reset_zoom();
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
    for (each<vector<MinimizeButton*> > button = m_minimize_buttons; button; ++button)
    {
        all_minimized = all_minimized && (*button)->is_on();
        not_minimized = not_minimized || !(*button)->is_on();
    }

    // All were manually minimized, exit full screen mode
    if (all_minimized)
        m_fullscreen = false;

    // At least one is on screen, enter full screen mode
    if (not_minimized)
        m_fullscreen = true;

    for (each<vector<MinimizeButton*> > button = m_minimize_buttons; button; ++button)
        (*button)->set_fullscreen(m_fullscreen);
}

void MainWindow::slot_show_rendering_settings_window()
{
    assert(m_project_manager.is_project_open());

    if (m_rendering_settings_window.get() == 0)
    {
        m_rendering_settings_window.reset(new RenderingSettingsWindow(m_project_manager, this));

        connect(
            m_rendering_settings_window.get(), SIGNAL(signal_settings_modified()),
            SLOT(slot_project_modified()));
    }

    m_rendering_settings_window->showNormal();
    m_rendering_settings_window->activateWindow();
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

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/moc_cpp_mainwindow.cxx"
