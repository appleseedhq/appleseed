
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "mainwindow/logwidget.h"
#include "utility/interop.h"
#include "utility/settingskeys.h"
#include "utility/tweaks.h"

// appleseed.shared headers.
#include "application/application.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/log.h"
#include "renderer/api/project.h"
#include "renderer/api/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/system.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/settings.h"
#include "foundation/utility/string.h"

// Qt headers.
#include <QAction>
#include <QActionGroup>
#include <QApplication>
#include <QDir>
#include <QFileDialog>
#include <QGridLayout>
#include <QIcon>
#include <QMessageBox>
#include <QMetaType>
#include <QRegExp>
#include <QStatusBar>
#include <QString>
#include <QWidget>
#include <QSettings>

// boost headers.
#include "boost/filesystem/path.hpp"

// standard headers
#include <algorithm>

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

const int MainWindow::max_recently_opened_files = 5;

MainWindow::MainWindow(QWidget* parent)
  : QMainWindow(parent)
  , m_ui(new Ui::MainWindow())
  , m_rendering_manager(m_status_bar)
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

    update_workspace();
    update_project_explorer();

    remove_render_widgets();

    showMaximized();
}

MainWindow::~MainWindow()
{
    delete m_ui;
}

void MainWindow::build_menus()
{
    // File menu.
    m_ui->action_file_new_project->setShortcut( QKeySequence::New );
    connect(m_ui->action_file_new_project, SIGNAL(triggered()), SLOT(slot_new_project()));

    m_ui->action_file_open_project->setShortcut( QKeySequence::Open );
    connect(m_ui->action_file_open_project, SIGNAL(triggered()), SLOT(slot_open_project()));

    // open recent
    init_recent_files_menu();

    connect(m_ui->action_file_open_builtin_project_cornellbox, SIGNAL(triggered()), SLOT(slot_open_cornellbox_builtin_project()));
    connect(m_ui->action_file_reload_project, SIGNAL(triggered()), SLOT(slot_reload_project()));

    m_ui->action_file_save_project->setShortcut( QKeySequence::Save );
    connect(m_ui->action_file_save_project, SIGNAL(triggered()), this, SLOT(slot_save_project()));

    m_ui->action_file_save_project_as->setShortcut( QKeySequence::SaveAs );
    connect(m_ui->action_file_save_project_as, SIGNAL(triggered()), this, SLOT(slot_save_project_as()));

    m_ui->action_file_exit->setShortcut( QKeySequence::Quit );
    connect(m_ui->action_file_exit, SIGNAL(triggered()), this, SLOT(close()));

    // Rendering menu.
    connect(m_ui->action_rendering_start_interactive_rendering, SIGNAL(triggered()), this, SLOT(slot_start_interactive_rendering()));
    connect(m_ui->action_rendering_start_final_rendering, SIGNAL(triggered()), this, SLOT(slot_start_final_rendering()));
    connect(m_ui->action_rendering_stop_rendering, SIGNAL(triggered()), this, SLOT(slot_stop_rendering()));
    connect(m_ui->action_rendering_render_settings, SIGNAL(triggered()), this, SLOT(slot_show_render_settings_window()));

    // Diagnostics menu.
    build_override_shading_menu_item();

    // Debug menu.
    connect(m_ui->action_debug_tests, SIGNAL(triggered()), this, SLOT(slot_show_test_window()));
    connect(m_ui->action_debug_benchmarks, SIGNAL(triggered()), this, SLOT(slot_show_benchmark_window()));

    // Tools menu.
    connect(m_ui->action_tools_save_settings, SIGNAL(triggered()), this, SLOT(slot_save_settings()));
    connect(m_ui->action_tools_reload_settings, SIGNAL(triggered()), this, SLOT(slot_load_settings()));

    // Help menu.
    connect(m_ui->action_help_about, SIGNAL(triggered()), this, SLOT(slot_show_about_window()));

    // View menu.
    m_ui->menu_view->addAction(m_ui->project_explorer->toggleViewAction());
    m_ui->menu_view->addAction(m_ui->log->toggleViewAction());
}

void MainWindow::build_override_shading_menu_item()
{
    QActionGroup* action_group = new QActionGroup(this);

    connect(
        m_ui->action_diagnostics_override_shading_no_override, SIGNAL(triggered()),
        &m_rendering_manager, SLOT(slot_clear_shading_override()));

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
            &m_rendering_manager, SLOT(slot_set_shading_override()));

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

void MainWindow::build_toolbar()
{
    m_action_new_project = new QAction(QIcon(":/icons/page_white.png"), "&New", this);
    connect(m_action_new_project, SIGNAL(triggered()), this, SLOT(slot_new_project()));
    m_ui->main_toolbar->addAction(m_action_new_project);

    m_action_open_project = new QAction(QIcon(":/icons/folder.png"), "&Open", this);
    connect(m_action_open_project, SIGNAL(triggered()), this, SLOT(slot_open_project()));
    m_ui->main_toolbar->addAction(m_action_open_project);

    m_action_save_project = new QAction(QIcon(":/icons/disk.png"), "&Save", this);
    connect(m_action_save_project, SIGNAL(triggered()), this, SLOT(slot_save_project()));
    m_ui->main_toolbar->addAction(m_action_save_project);

    m_ui->main_toolbar->addSeparator();

    m_action_start_interactive_rendering = new QAction(QIcon(":/icons/film_go.png"), "Start &Interactive Rendering", this);
    connect(m_action_start_interactive_rendering, SIGNAL(triggered()), this, SLOT(slot_start_interactive_rendering()));
    m_ui->main_toolbar->addAction(m_action_start_interactive_rendering);

    m_action_start_final_rendering = new QAction(QIcon(":/icons/cog_go.png"), "Start &Final Rendering", this);
    connect(m_action_start_final_rendering, SIGNAL(triggered()), this, SLOT(slot_start_final_rendering()));
    m_ui->main_toolbar->addAction(m_action_start_final_rendering);

    m_action_stop_rendering = new QAction(QIcon(":/icons/cross.png"), "S&top Rendering", this);
    connect(m_action_stop_rendering, SIGNAL(triggered()), this, SLOT(slot_stop_rendering()));
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
        this, SLOT(slot_filter_text_changed(const QString&)));

    connect(
        m_ui->pushbutton_clear_filter, SIGNAL(clicked()),
        this, SLOT(slot_clear_filter()));

    m_ui->pushbutton_clear_filter->setEnabled(false);
}

void MainWindow::build_connections()
{
    connect(
        &m_rendering_manager, SIGNAL(signal_rendering_end()),
        this, SLOT(slot_rendering_end()));

    connect(
        &m_rendering_manager, SIGNAL(signal_camera_changed()),
        this, SLOT(slot_camera_changed()));
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

    RENDERER_LOG_INFO(
        "system information:\n"
        "  L1 data cache    size %s, line size %s\n"
        "  L2 cache         size %s, line size %s\n"
        "  L3 cache         size %s, line size %s\n",
        pretty_size(System::get_l1_data_cache_size()).c_str(),
        pretty_size(System::get_l1_data_cache_line_size()).c_str(),
        pretty_size(System::get_l2_cache_size()).c_str(),
        pretty_size(System::get_l2_cache_line_size()).c_str(),
        pretty_size(System::get_l3_cache_size()).c_str(),
        pretty_size(System::get_l3_cache_line_size()).c_str());
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
    recreate_render_widgets();

    update_workspace();
    update_project_explorer();
    update_override_shading_menu_item();

    if (m_render_settings_window.get())
        m_render_settings_window->reload();

    m_ui->lineedit_filter->clear();
    m_status_bar.clear();
}

void MainWindow::update_workspace()
{
    update_window_title();
    enable_disable_widgets(false);
}

void MainWindow::update_project_explorer()
{
    m_ui->treewidget_project_explorer_scene->clear();

    if (m_project_manager.is_project_open())
    {
        m_project_explorer.reset(
            new ProjectExplorer(
                m_ui->treewidget_project_explorer_scene,    
                *m_project_manager.get_project(),
                m_settings));

        QObject::connect(
            m_project_explorer.get(), SIGNAL(signal_project_modified()),
            this, SLOT(slot_project_modified()));
    }
    else
    {
        m_project_explorer.reset();
    }
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

void MainWindow::enable_disable_widgets(const bool rendering)
{
    const bool is_project_open = m_project_manager.is_project_open();

    // Project Explorer.
    m_ui->treewidget_project_explorer_scene->setEnabled(is_project_open);

    enable_disable_menu_items(rendering);
}

void MainWindow::enable_disable_menu_items(const bool rendering)
{
    const bool is_project_open = m_project_manager.is_project_open();

    const bool allow_replacing_project = !rendering;

    // File -> New Project.
    m_ui->action_file_new_project->setEnabled(allow_replacing_project);
    m_action_new_project->setEnabled(allow_replacing_project);

    // File -> Open Project.
    m_ui->action_file_open_project->setEnabled(allow_replacing_project);
    m_action_open_project->setEnabled(allow_replacing_project);

    // File -> Open Built-in Project.
    m_ui->menu_file_open_builtin_project->setEnabled(allow_replacing_project);

    // File -> Reload Project.
    m_ui->action_file_reload_project->setEnabled(
        is_project_open &&
        m_project_manager.get_project()->has_path() &&
        !rendering);

    const bool allow_saving_project = is_project_open && !rendering;

    // File -> Save Project.
    m_ui->action_file_save_project->setEnabled(allow_saving_project);
    m_action_save_project->setEnabled(allow_saving_project);

    // File -> Save Project As.
    m_ui->action_file_save_project_as->setEnabled(allow_saving_project);

    const bool allow_starting_rendering = is_project_open && !rendering;
    const bool allow_stopping_rendering = is_project_open && rendering;

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
    m_ui->action_rendering_render_settings->setEnabled(is_project_open && !rendering);
}

void MainWindow::recreate_render_widgets()
{
    remove_render_widgets();

    if (m_project_manager.is_project_open())
    {
        add_render_widgets();
    }
}

void MainWindow::remove_render_widgets()
{
    for (const_each<RenderWidgetCollection> i = m_render_widgets; i; ++i)
        delete i->second;

    m_render_widgets.clear();

    while (m_ui->tab_render_channels->count() > 0)
        m_ui->tab_render_channels->removeTab(0);
}

void MainWindow::add_render_widgets()
{
    const Project* project = m_project_manager.get_project();
    const Frame* frame = project->get_frame();
    const CanvasProperties& props = frame->image().properties();

    const int width = static_cast<int>(props.m_canvas_width);
    const int height = static_cast<int>(props.m_canvas_height);

    add_render_widget(width, height, "RGB");
    add_render_widget(width, height, "Alpha");
    add_render_widget(width, height, "Depth");
    add_render_widget(width, height, "Anomalies");
}

void MainWindow::add_render_widget(
    const int       width,
    const int       height,
    const QString&  label)
{
    // Create a render widget.
    RenderWidget* render_widget = new RenderWidget(width, height);

    // Attach a contextual menu to the render widget.
    render_widget->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(
        render_widget, SIGNAL(customContextMenuRequested(const QPoint&)),
        this, SLOT(slot_render_widget_context_menu(const QPoint&)));

    // Encapsulate the render widget into another widget that adds a margin around it.
    QWidget* render_widget_wrapper = new QWidget();
    render_widget_wrapper->setLayout(new QGridLayout());
    render_widget_wrapper->layout()->setSizeConstraint(QLayout::SetFixedSize);
    render_widget_wrapper->layout()->setContentsMargins(20, 20, 20, 20);
    render_widget_wrapper->layout()->addWidget(render_widget);

    // Wrap the render widget with a scroll area.
    QScrollArea* scroll_area = new QScrollArea();
    scroll_area->setAlignment(Qt::AlignCenter);
    scroll_area->setWidget(render_widget_wrapper);

    // Associate a zoom handler to the scroll area / render widget.
    m_render_widgets[label.toStdString()] =
        new RenderWidgetRecord(
            scroll_area,
            render_widget,
            width,
            height);

    // Encapsulate the scroll area inside a tab.
    QWidget* tab = new QWidget();
    tab->setLayout(new QGridLayout());
    tab->layout()->addWidget(scroll_area);

    // Add the tab to the render channels tab bar.
    m_ui->tab_render_channels->addTab(tab, label);
}

void MainWindow::start_rendering(const bool interactive)
{
    assert(m_project_manager.is_project_open());

    enable_disable_widgets(true);

    const char* configuration_name = interactive ? "interactive" : "final";
    const ParamArray params = get_project_params(configuration_name);

    m_rendering_manager.start_rendering(
        m_project_manager.get_project(),
        params,
        interactive,
        m_render_widgets["RGB"]->m_render_widget);
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

void MainWindow::init_recent_files_menu()
{
    m_recently_opened.reserve(max_recently_opened_files);
    for(int i = 0; i < max_recently_opened_files; ++i)
    {
        m_recently_opened.push_back(new QAction(this));
        m_recently_opened[i]->setVisible(false);
        connect(m_recently_opened[i], SIGNAL(triggered()), SLOT(slot_open_recent()));
        m_ui->menu_open_recent->addAction(m_recently_opened[i]);
    }

    QSettings settings("com.appleseed.studio", "Appleseed.studio Recent Files");
    QStringList files = settings.value("recent_file_list").toStringList();
    update_recent_files_menu(files);

    m_ui->menu_open_recent->addSeparator();
    m_clear_open_recent_menu = new QAction(this);
    m_clear_open_recent_menu->setText("Clear Menu");
    connect(m_clear_open_recent_menu, SIGNAL(triggered()), SLOT(slot_clear_open_recent_files_menu()));
    m_ui->menu_open_recent->addAction(m_clear_open_recent_menu);
}

void MainWindow::update_recent_files_menu(const QString& filename)
{
    QSettings settings("com.appleseed.studio", "Appleseed.studio Recent Files");
    QStringList files = settings.value("recent_file_list").toStringList();
    files.removeAll(filename);
    files.prepend(filename);

    while(files.size() > max_recently_opened_files)
        files.removeLast();

    settings.setValue("recent_file_list", files);
    update_recent_files_menu(files);
}

void MainWindow::update_recent_files_menu(const QStringList& files)
{
    int num_recent_files = std::min(files.size(), static_cast<int>(max_recently_opened_files));

    for(int i = 0; i < num_recent_files; ++i)
    {
        QString stripped = QFileInfo(files[i]).fileName();
        QString text = tr("&%1 %2").arg(i + 1).arg(stripped);
        m_recently_opened[i]->setText(text);
        m_recently_opened[i]->setData(files[i]);
        m_recently_opened[i]->setVisible(true);
    }

    for(int j = num_recent_files; j < max_recently_opened_files; ++j)
        m_recently_opened[j]->setVisible(false);
}

void MainWindow::slot_new_project()
{
    if (!can_close_project())
        return;

    m_project_manager.create_project();

    on_project_change();
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
            "The project file may be invalid or corrupted. "
            "Please look at the Log window for details.");
        msgbox.setStandardButtons(QMessageBox::Ok);
        msgbox.exec();
    }

    void show_builtin_project_loading_failed_message_box(QWidget* parent, const QString& name)
    {
        QMessageBox msgbox(parent);
        msgbox.setWindowTitle("Loading Error");
        msgbox.setIcon(QMessageBox::Critical);
        msgbox.setText("Failed to load the built-in project '" + name + "'.");
        msgbox.setInformativeText(
            "The project may be invalid or corrupted. "
            "Please look at the Log window for details.");
        msgbox.setStandardButtons(QMessageBox::Ok);
        msgbox.exec();
    }
}

void MainWindow::open_project(QString filepath)
{
    if (!filepath.isEmpty())
    {
        filepath = QDir::toNativeSeparators(filepath);

        const filesystem::path path(filepath.toStdString());

        m_settings.insert_path(
            LAST_DIRECTORY_SETTINGS_KEY,
            path.parent_path().string());

        const bool successful =
            m_project_manager.load_project(filepath.toAscii().constData());

        if (successful)
        {
            on_project_change();
        }
        else
        {
            show_project_file_loading_failed_message_box(this, filepath);
        }
    }
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

    open_project(filepath);
    update_recent_files_menu(filepath);
}

void MainWindow::slot_open_recent()
{
    if (!can_close_project())
        return;

    QAction *action = qobject_cast<QAction *>(sender());

    if(action)
    {
        QString filename = action->data().toString();
        open_project(filename);
    }
}

void MainWindow::slot_clear_open_recent_files_menu()
{
    QSettings settings("com.appleseed.studio", "Appleseed.studio Recent Files");
    QStringList files;
    settings.setValue("recent_file_list", files);
    update_recent_files_menu(files);
}

void MainWindow::slot_open_cornellbox_builtin_project()
{
    if (!can_close_project())
        return;

    const bool successful = m_project_manager.load_builtin_project("cornell_box");

    if (successful)
    {
        on_project_change();
    }
    else
    {
        show_builtin_project_loading_failed_message_box(this, "cornell_box");
    }
}

void MainWindow::slot_reload_project()
{
    assert(m_project_manager.is_project_open());
    assert(m_project_manager.get_project()->has_path());

    if (!can_close_project())
        return;

    const bool successful = m_project_manager.reload_project();

    if (successful)
    {
        on_project_change();
    }
    else
    {
        show_project_file_loading_failed_message_box(
            this,
            m_project_manager.get_project()->get_path());
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
    }

    update_recent_files_menu(filepath);
    update_workspace();
}

void MainWindow::slot_project_modified()
{
    assert(m_project_manager.is_project_open());

    m_project_manager.set_project_dirty_flag();

    update_window_title();
}

void MainWindow::slot_start_interactive_rendering()
{
    start_rendering(true);
}

void MainWindow::slot_start_final_rendering()
{
    start_rendering(false);
}

void MainWindow::slot_stop_rendering()
{
    m_rendering_manager.abort_rendering();
}

void MainWindow::slot_rendering_end()
{
    update_workspace();
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

        QObject::connect(
            m_render_settings_window.get(), SIGNAL(signal_settings_modified()),
            this, SLOT(slot_project_modified()));
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

namespace
{
    bool filter_items(QTreeWidgetItem* item, const QRegExp& regexp)
    {
        bool any_children_visible = false;

        for (int i = 0; i < item->childCount(); ++i)
        {
            if (filter_items(item->child(i), regexp))
                any_children_visible = true;
        }

        const bool visible = any_children_visible || regexp.indexIn(item->text(0)) >= 0;

        item->setHidden(!visible);

        return visible;
    }

    void filter_items(const QTreeWidget* tree_widget, const QRegExp& regexp)
    {
        for (int i = 0; i < tree_widget->topLevelItemCount(); ++i)
            filter_items(tree_widget->topLevelItem(i), regexp);
    }
}

void MainWindow::slot_filter_text_changed(const QString& pattern)
{
    m_ui->pushbutton_clear_filter->setEnabled(!pattern.isEmpty());

    const QRegExp regexp(pattern);

    filter_items(m_ui->treewidget_project_explorer_scene, regexp);
}

void MainWindow::slot_clear_filter()
{
    m_ui->lineedit_filter->clear();
}

void MainWindow::slot_render_widget_context_menu(const QPoint& point)
{
    if (m_rendering_manager.is_rendering())
        return;

    QMenu* menu = new QMenu(this);
    menu->addAction("Save Frame As...", this, SLOT(slot_save_frame()));
    menu->exec(reinterpret_cast<QWidget*>(sender())->mapToGlobal(point));
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
            "OpenEXR (*.exr);;PNG (*.png)",
            &selected_filter,
            options);

    if (!filepath.isEmpty())
    {
        filepath = QDir::toNativeSeparators(filepath);

        const Project* project = m_project_manager.get_project();
        project->get_frame()->write(filepath.toAscii().constData());
    }
}

}   // namespace studio
}   // namespace appleseed
