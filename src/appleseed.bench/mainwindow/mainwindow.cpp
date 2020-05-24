
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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

// appleseed.bench headers.
#include "help/about/aboutwindow.h"
#include "mainwindow/constants.h"
#include "mainwindow/rankingswindow.h"
#include "utility/backendapi.h"
#include "utility/formatrendertime.h"
#include "utility/settingskeys.h"

// appleseed.qtcommon headers.
#include "utility/interop.h"
#include "utility/miscellaneous.h"
#include "widgets/chartwidget.h"
#include "widgets/logwidget.h"

// appleseed.common headers.
#include "application/application.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/log.h"
#include "renderer/api/project.h"
#include "renderer/api/rendering.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/core/appleseed.h"
#include "foundation/log/logmessage.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/path.h"
#include "foundation/platform/system.h"
#include "foundation/string/string.h"

// Qt headers.
#include <QAction>
#include <QByteArray>
#include <QCloseEvent>
#include <QColor>
#include <QDateTime>
#include <QDesktopServices>
#include <QDir>
#include <QFileInfo>
#include <QJsonDocument>
#include <QJsonObject>
#include <QLabel>
#include <QLayout>
#include <QMenu>
#include <QMessageBox>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QSettings>
#include <QShortcut>
#include <QSizePolicy>
#include <QSslSocket>
#include <QStringList>
#include <Qt>
#include <QUrl>

// Standard headers.
#include <cstdlib>
#include <limits>
#include <utility>

using namespace appleseed::common;
using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;
namespace bf = boost::filesystem;

namespace appleseed {
namespace bench {

//
// MainWindow class implementation.
//

// Use a simpler scene and lower rendering quality in debug builds.
#ifdef APPLESEED_DEBUG
    #define FAST_DEBUG_MODE
#endif

// Enable result submission only for properly-configured builds.
#undef ENABLE_RESULT_SUBMISSION
#if defined APPLESEED_DEBUG
    // Enabling result submission in debug builds allows to debug the submission process.
    #define ENABLE_RESULT_SUBMISSION
#elif defined APPLESEED_SHIP
    #ifndef APPLESEED_WITH_SPECTRAL_SUPPORT
        #define ENABLE_RESULT_SUBMISSION
    #endif
#endif

namespace
{
    void show_init_error_and_terminate(QWidget* parent, const QString& text)
    {
        QMessageBox msgbox(parent);
        msgbox.setWindowTitle("Initialization Error");
        msgbox.setIcon(QMessageBox::Critical);
        msgbox.setText(text + "\n\nPlease reinstall the application.");
        msgbox.setStandardButtons(QMessageBox::Ok);
        msgbox.exec();
        std::exit(1);
    }
}

MainWindow::MainWindow(QWidget* parent)
  : QMainWindow(parent)
  , m_ui(new Ui::MainWindow())
  , m_network_access_manager(this)
  , m_root_path(Application::get_root_path())
  , m_scaling_test_params(
        m_system_info.get_enabled_cpu_core_count(),
        m_system_info.get_cpu_thread_count())
{
    initialize_ocio();

    m_ui->setupUi(this);

    m_start_single_render_button_text = m_ui->pushbutton_start_single_render->text();
    m_start_scaling_test_button_text = m_ui->pushbutton_start_scaling_test->text();
    m_start_throttling_test_button_text = m_ui->pushbutton_start_throttling_test->text();

    m_start_single_render_shortcut = create_window_local_shortcut(this, Qt::Key_F5);
    m_start_scaling_test_shortcut = create_window_local_shortcut(this, Qt::Key_F6);
    m_start_throttling_test_shortcut = create_window_local_shortcut(this, Qt::Key_F7);

    build_menus();
    build_log_panel();
    build_system_description();

    m_ui->spinbox_single_render_threads->setValue(m_system_info.get_cpu_thread_count());

    m_rendering_time_display.reset(new RenderingTimeDisplay(m_ui->label_render_time_value));
    m_rendering_manager.reset(new RenderingManager(*m_rendering_time_display));

    build_connections();

    slot_load_all_settings();

    update_benchmark_controls();

#ifdef FAST_DEBUG_MODE
    m_is_genuine_project = true;
    m_project_manager.load_project_async("builtin:cornell_box");
#else
    const std::string project_filepath = (m_root_path / "data" / "scene.appleseed").string();
    const QByteArray project_file_hash = compute_file_hash(QString::fromStdString(project_filepath), QCryptographicHash::Sha1).toBase64();
    m_is_genuine_project = project_file_hash == "r6hBin5zBvQ6O6nU8TfuEVyKJRs=";
    m_project_manager.load_project_async(project_filepath);
#endif

#ifdef ENABLE_RESULT_SUBMISSION
    if (m_is_genuine_project)
        m_ui->label_result_submission_warning->setVisible(false);
    else
    {
        m_ui->label_result_submission_warning->setText("Benchmark scene has been altered. Result submission is disabled.");
        m_ui->label_result_submission_warning->setVisible(true);
    }
#else
    m_ui->label_result_submission_warning->setText("Result submission is disabled in this build.");
    m_ui->label_result_submission_warning->setVisible(true);
#endif

    assert(!m_ui->pushbutton_results_submit->isEnabled());
}

MainWindow::~MainWindow()
{
    delete m_ui;
}

void MainWindow::build_menus()
{
    {
        //
        // File menu.
        //

        connect(m_ui->action_file_save_render, &QAction::triggered, this, &MainWindow::slot_save_render);
        connect(m_ui->action_file_export_chart, &QAction::triggered, this, &MainWindow::slot_export_chart);

        m_ui->action_file_exit->setShortcut(QKeySequence::Quit);
        connect(m_ui->action_file_exit, &QAction::triggered, this, &MainWindow::close);
    }

#ifdef APPLESEED_DEBUG

    {
        //
        // View menu.
        //

        QMenu* menu_view = new QMenu("&View");
        m_ui->main_menubar->insertMenu(m_ui->menu_help->menuAction(), menu_view);

        menu_view->addAction(m_ui->log->toggleViewAction());
    }

    {
        //
        // Tools menu.
        //

        QMenu* menu_tools = new QMenu("&Tools");
        m_ui->main_menubar->insertMenu(m_ui->menu_help->menuAction(), menu_tools);

        QAction* action_tools_settings = new QAction("&Settings...", this);
        connect(action_tools_settings, &QAction::triggered, this, &MainWindow::slot_show_application_settings_window);
        menu_tools->addAction(action_tools_settings);

        QAction* action_tools_save_settings = new QAction("S&ave Settings", this);
        connect(action_tools_save_settings, &QAction::triggered, this, &MainWindow::slot_save_all_settings);
        menu_tools->addAction(action_tools_save_settings);

        QAction* action_tools_reload_settings = new QAction("&Reload Settings", this);
        connect(action_tools_reload_settings, &QAction::triggered, this, &MainWindow::slot_load_all_settings);
        menu_tools->addAction(action_tools_reload_settings);
    }

#endif

    {
        //
        // Help menu.
        //

        connect(
            m_ui->action_help_website, &QAction::triggered,
            this, [=]() { QDesktopServices::openUrl(QUrl("https://appleseedhq.net/")); });

        connect(
            m_ui->action_help_forum, &QAction::triggered,
            this, [=]() { QDesktopServices::openUrl(QUrl("https://forum.appleseedhq.net/")); });

        connect(
            m_ui->action_help_twitter, &QAction::triggered,
            this, [=]() { QDesktopServices::openUrl(QUrl("https://twitter.com/appleseedhq")); });

        connect(
            m_ui->action_help_report_bug, &QAction::triggered,
            this, [=]() { QDesktopServices::openUrl(QUrl("https://github.com/appleseedhq/appleseed/issues/new")); });

        connect(
            m_ui->action_help_github, &QAction::triggered,
            this, [=]() { QDesktopServices::openUrl(QUrl("https://github.com/appleseedhq/appleseed")); });

        connect(
            m_ui->action_help_download_logos, &QAction::triggered,
            this, [=]() { QDesktopServices::openUrl(QUrl("https://github.com/appleseedhq/appleseed/tree/master/resources/logo")); });

        connect(m_ui->action_help_about, &QAction::triggered, this, &MainWindow::slot_show_about_window);
    }
}

void MainWindow::build_log_panel()
{
    m_ui->log->hide();

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

    RENDERER_LOG_INFO("Qt SSL support: %s", QSslSocket::supportsSsl() ? "yes" : "no");

    if (QSslSocket::supportsSsl())
    {
        RENDERER_LOG_INFO(
            "Qt SSL library built against: %s\n"
            "Qt SSL library in use: %s",
            QSslSocket::sslLibraryBuildVersionString().toLocal8Bit().constData(),
            QSslSocket::sslLibraryVersionString().toLocal8Bit().constData());
    }
}

void MainWindow::build_system_description()
{
    m_ui->label_cpu_model_value->setText(SystemInfo::cleanup_cpu_model_string(m_system_info.get_cpu_id()));
    m_ui->label_cores_value->setText(QString::number(m_system_info.get_cpu_core_count()));
    m_ui->label_enabled_cores_value->setText(QString::number(m_system_info.get_enabled_cpu_core_count()));
    m_ui->label_threads_value->setText(QString::number(m_system_info.get_cpu_thread_count()));
    m_ui->label_l1_data_cache_value->setText(
        QString("%1 x %2")
            .arg(System::get_l1_data_cache_count())
            .arg(QString::fromStdString(pretty_size(System::get_l1_data_cache_size(), 0))));
    m_ui->label_l2_cache_value->setText(
        QString("%1 x %2")
            .arg(System::get_l2_cache_count())
            .arg(QString::fromStdString(pretty_size(System::get_l2_cache_size(), 0))));
    m_ui->label_l3_cache_value->setText(
        QString("%1 x %2")
            .arg(System::get_l3_cache_count())
            .arg(QString::fromStdString(pretty_size(System::get_l3_cache_size(), 0))));
    m_ui->label_physical_memory_value->setText(QString::fromStdString(pretty_size(System::get_total_physical_memory_size())));
    m_ui->label_virtual_memory_value->setText(QString::fromStdString(pretty_size(System::get_total_virtual_memory_size())));
    m_ui->label_operating_system_value->setText(m_system_info.get_os_id());
}

void MainWindow::build_render_widget()
{
    const CanvasProperties& props = m_project_manager.get_project()->get_frame()->image().properties();

    m_render_widget.reset(
        new RenderWidget(
            props.m_canvas_width,
            props.m_canvas_height,
            m_ocio_config,
            this));

#ifndef FAST_DEBUG_MODE
    load_render_widget_preview_image();
#endif

    m_render_widget->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(
        m_render_widget.get(), &RenderWidget::customContextMenuRequested,
        this, &MainWindow::slot_render_widget_context_menu);

    m_ui->frame_main_window->layout()->addWidget(m_render_widget.get());
    m_ui->frame_main_window->layout()->setAlignment(m_render_widget.get(), Qt::AlignHCenter);

    m_ui->action_file_save_render->setEnabled(true);
}

void MainWindow::build_chart_widget()
{
    m_chart_widget.reset(new ChartWidget(this));
    m_chart_widget->hide();

    m_chart_widget->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(
        m_chart_widget.get(), &RenderWidget::customContextMenuRequested,
        this, &MainWindow::slot_chart_widget_context_menu);

    m_ui->frame_main_window->layout()->addWidget(m_chart_widget.get());
}

void MainWindow::load_render_widget_preview_image()
{
    const std::string preview_image_filepath = (m_root_path / "data" / "preview.png").string();

    if (!m_render_widget->load(QString::fromStdString(preview_image_filepath)))
        show_init_error_and_terminate(this, "Failed to load preview image.");
}

void MainWindow::build_connections()
{
    connect(
        &m_project_manager, &ProjectManager::signal_load_project_async_complete,
        this, &MainWindow::slot_open_project_complete);

    connect(
        m_ui->pushbutton_start_single_render, &QPushButton::clicked,
        this, &MainWindow::slot_start_single_render);

    connect(
        m_ui->radiobutton_scaling_test_quick, &QRadioButton::clicked,
        this, [this]() { m_scaling_test_params.set_mode(ScalingTestParams::Mode::Quick); });

    connect(
        m_ui->radiobutton_scaling_test_exhaustive, &QRadioButton::clicked,
        this, [this]() { m_scaling_test_params.set_mode(ScalingTestParams::Mode::Exhaustive); });

    connect(
        m_ui->pushbutton_start_scaling_test, &QPushButton::clicked,
        this, &MainWindow::slot_start_scaling_test);

    connect(
        m_ui->pushbutton_start_throttling_test, &QPushButton::clicked,
        this, &MainWindow::slot_start_throttling_test);

    connect(
        m_ui->pushbutton_results_submit, &QPushButton::clicked,
        this, &MainWindow::slot_submit_result);

    connect(
        m_ui->pushbutton_results_view_rankings, &QPushButton::clicked,
        this, [=](){ slot_show_rankings_window(m_user_result_id); });

    connect(
        m_rendering_manager.get(), &RenderingManager::signal_rendering_success,
        this, &MainWindow::slot_rendering_success);

    connect(
        m_rendering_manager.get(), &RenderingManager::signal_rendering_abort,
        this, &MainWindow::slot_rendering_abort);

    connect(
        m_rendering_manager.get(), &RenderingManager::signal_progressive_frame_update,
        this, &MainWindow::slot_throttling_test_graph_update);

    connect(
        m_start_single_render_shortcut, &QShortcut::activated,
        this, &MainWindow::slot_start_single_render);

    connect(
        m_start_scaling_test_shortcut, &QShortcut::activated,
        this, &MainWindow::slot_start_scaling_test);

    connect(
        m_start_throttling_test_shortcut, &QShortcut::activated,
        this, &MainWindow::slot_start_throttling_test);

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), &QShortcut::activated,
        m_rendering_manager.get(), &RenderingManager::slot_abort_rendering);

    connect(
        &m_throttling_test_params, &ThrottlingTestParams::signal_test_complete,
        m_rendering_manager.get(), &RenderingManager::slot_abort_rendering);
}

namespace
{
    const QString g_stop_pushbutton_text = "&Stop [Esc]";
}

void MainWindow::update_benchmark_controls()
{
    const bool is_project_open = m_project_manager.is_project_open();

    const auto set_child_widgets_enabled = [&](const QWidget* parent, const bool enabled)
    {
        for (QWidget* child : parent->findChildren<QWidget*>())
            child->setEnabled(is_project_open && enabled);
    };

    const auto set_all_widgets_enabled = [&](QWidget* parent, const bool enabled)
    {
        parent->setEnabled(is_project_open && enabled);
        set_child_widgets_enabled(parent, enabled);
    };

    switch (m_benchmarking_mode)
    {
      case BenchmarkingMode::None:
        set_all_widgets_enabled(m_ui->groupbox_single_render, true);
        set_all_widgets_enabled(m_ui->groupbox_scaling_test, true);
        set_all_widgets_enabled(m_ui->groupbox_throttling_test, true);
        break;

      case BenchmarkingMode::SingleRender:
        set_child_widgets_enabled(m_ui->groupbox_single_render, false);
        m_ui->pushbutton_start_single_render->setEnabled(true);
        set_all_widgets_enabled(m_ui->groupbox_scaling_test, false);
        set_all_widgets_enabled(m_ui->groupbox_throttling_test, false);
        break;

      case BenchmarkingMode::ScalingTest:
        set_child_widgets_enabled(m_ui->groupbox_scaling_test, false);
        m_ui->pushbutton_start_scaling_test->setEnabled(true);
        set_all_widgets_enabled(m_ui->groupbox_single_render, false);
        set_all_widgets_enabled(m_ui->groupbox_throttling_test, false);
        break;

      case BenchmarkingMode::ThrottlingTest:
        set_child_widgets_enabled(m_ui->groupbox_throttling_test, false);
        m_ui->pushbutton_start_throttling_test->setEnabled(true);
        set_all_widgets_enabled(m_ui->groupbox_single_render, false);
        set_all_widgets_enabled(m_ui->groupbox_scaling_test, false);
        break;
    }

    m_start_single_render_shortcut->setEnabled(is_project_open && m_benchmarking_mode == BenchmarkingMode::None);
    m_start_scaling_test_shortcut->setEnabled(is_project_open && m_benchmarking_mode == BenchmarkingMode::None);
    m_start_throttling_test_shortcut->setEnabled(is_project_open && m_benchmarking_mode == BenchmarkingMode::None);

    m_ui->pushbutton_start_single_render->setText(
        m_benchmarking_mode == BenchmarkingMode::SingleRender
            ? g_stop_pushbutton_text
            : m_start_single_render_button_text);

    m_ui->pushbutton_start_scaling_test->setText(
        m_benchmarking_mode == BenchmarkingMode::ScalingTest
            ? g_stop_pushbutton_text
            : m_start_scaling_test_button_text);

    m_ui->pushbutton_start_throttling_test->setText(
        m_benchmarking_mode == BenchmarkingMode::ThrottlingTest
            ? g_stop_pushbutton_text
            : m_start_throttling_test_button_text);
}

ParamArray MainWindow::get_project_params(const BenchmarkingMode benchmarking_mode) const
{
    ParamArray params;

    // Retrieve the configuration.
    const char* configuration_name =
        benchmarking_mode == BenchmarkingMode::ThrottlingTest ? "interactive" : "final";
    Configuration* configuration =
        m_project_manager.get_project()->configurations().get_by_name(configuration_name);
    assert(configuration != nullptr);

    // Start with the parameters from the base configuration.
    if (configuration->get_base())
        params = configuration->get_base()->get_parameters();

    // Override with application settings.
    params.merge(m_application_settings);

    // Override with parameters from the configuration.
    if (configuration)
        params.merge(configuration->get_parameters());

    // Set thread count.
    switch (benchmarking_mode)
    {
      case BenchmarkingMode::SingleRender:
        params.insert("rendering_threads", m_ui->spinbox_single_render_threads->value());
        break;

      case BenchmarkingMode::ScalingTest:
        params.insert("rendering_threads", m_scaling_test_params.get_thread_count());
        break;

      case BenchmarkingMode::ThrottlingTest:
        params.insert("rendering_threads", m_system_info.get_cpu_thread_count());
        break;
    }

    // Set sampling profile for the progressive renderer's sample generator job.
    params.insert_path("progressive_frame_renderer.samples_in_uninterruptible_phase", 0);
    params.insert_path("progressive_frame_renderer.samples_in_linear_phase", std::numeric_limits<std::uint64_t>::max());
    params.insert_path("progressive_frame_renderer.samples_per_job_in_linear_phase", 2048);

    return params;
}

bool MainWindow::can_close_project()
{
    // Project being loaded: can't close.
    if (m_project_manager.is_project_loading())
        return false;

    return true;
}

void MainWindow::start_rendering(const BenchmarkingMode benchmarking_mode)
{
    assert(m_project_manager.is_project_open());
    assert(benchmarking_mode != BenchmarkingMode::None);

    // Don't start a new render until the previous has completely ended.
    if (m_rendering_manager->is_rendering())
        return;

    // Update UI.
    m_ui->pushbutton_results_submit->setEnabled(false);
    if (m_render_count++ > 0)
    {
#ifndef FAST_DEBUG_MODE
        load_render_widget_preview_image();
#endif
    }

    // Retrieve project.
    Project* project = m_project_manager.get_project();

    // Clear frame.
    Frame* frame = project->get_frame();
    frame->clear_main_and_aov_images();

    // Retrieve the appropriate rendering configuration.
    const ParamArray params = get_project_params(benchmarking_mode);

    // Effectively start rendering.
    // m_priority_context.reset(new ProcessPriorityContext(ProcessPriorityHigh, &global_logger()));
    m_benchmarking_mode = benchmarking_mode;
    m_rendering_manager->start_rendering(
        project,
        params,
        benchmarking_mode == BenchmarkingMode::ThrottlingTest
            ? RenderingManager::RenderingMode::InteractiveRendering
            : RenderingManager::RenderingMode::FinalRendering,
        m_render_widget.get());
}

void MainWindow::reset_chart_for_scaling_test()
{
    const int thread_count = m_system_info.get_cpu_thread_count();

    m_chart_widget->clear();

    {
        std::unique_ptr<LineChart> chart(new LineChart());

        chart->set_horizontal_range(1.0, thread_count);
        chart->set_vertical_range(1.0, thread_count);
        chart->set_vertical_subdivisions(thread_count);

        chart->set_horizontal_window_margins(15.0, 30.0);
        chart->set_vertical_window_margins(15.0, 30.0);

        chart->set_shadow_enabled(true);
        chart->set_curve_brush(QColor(60, 60, 60));
        chart->set_points_highlightable(false);

        chart->set_horizontal_legend_formatter(
            [](const double value) -> QString
            {
                return QString::number(static_cast<int>(value));
            });

        chart->set_vertical_legend_formatter(
            [](const double value) -> QString
            {
                return QString("%1x").arg(value, 2, 'f', 2);
            });

        for (int i = 1; i <= thread_count; ++i)
            chart->add_point(static_cast<double>(i), static_cast<double>(i));

        m_chart_widget->add_chart(std::move(chart));
    }

    {
        std::unique_ptr<LineChart> chart(new LineChart());

        chart->set_horizontal_range(1.0, thread_count);
        chart->set_vertical_range(1.0, thread_count);
        chart->set_vertical_subdivisions(thread_count);

        chart->set_horizontal_window_margins(15.0, 30.0);
        chart->set_vertical_window_margins(15.0, 30.0);

        chart->set_shadow_enabled(true);
        chart->set_curve_brush(QColor(170, 170, 170));

        chart->set_tooltip_formatter(
            [](const Vector2d& point, const QVariant& data) -> QString
            {
                const int thread_count = static_cast<int>(point.x);
                const double speedup = point.y;

                return
                    thread_count == 1
                        ? QString(u8"1 thread \u2192 Baseline\nRender Time: %1")
                              .arg(format_render_time(static_cast<int>(data.toDouble())))
                        : QString(u8"%1 threads \u2192 %2x speed-up\nEfficiency: %3%\nRender Time: %4")
                              .arg(QString::number(thread_count))
                              .arg(speedup, 2, 'f', 2)
                              .arg(100.0 * speedup / thread_count, 1, 'f', 1)
                              .arg(format_render_time(static_cast<int>(data.toDouble())));
            });

        m_chart = m_chart_widget->add_chart(std::move(chart));
    }

    m_chart_widget->update();
}

void MainWindow::reset_chart_for_throtting_test()
{
    std::unique_ptr<LineChart> chart(new LineChart());

    chart->set_horizontal_window_margins(15.0, 30.0);
    chart->set_vertical_window_margins(15.0, 30.0);

    chart->set_shadow_enabled(true);
    chart->set_curve_brush(QColor(170, 170, 170));
    chart->set_points_visible(false);

    chart->set_vertical_legend_formatter(
        [](const double value) -> QString
        {
            return QString::number(static_cast<quint64>(value));
        });

    chart->set_tooltip_formatter(
        [](const Vector2d& point, const QVariant& data) -> QString
        {
            const int render_time = static_cast<int>(point.x);
            const auto samples_per_second = static_cast<std::uint64_t>(point.y);

            return
                QString(u8"Time: %1 \u2192 %2 sample%3/second")
                    .arg(format_render_time(render_time))
                    .arg(QString::fromStdString(pretty_uint(samples_per_second)))
                    .arg(samples_per_second > 1 ? "s": "");
        });

    chart->add_point(Vector2d(0.0, 0.0));  // x = 0 second, y = 0 sample

    m_chart_widget->clear();
    m_chart = m_chart_widget->add_chart(std::move(chart));

    m_chart_widget->update();
}

void MainWindow::submit_result()
{
    assert(m_project_manager.is_project_open());

    BenchmarkResult result;
    result.m_submission_datetime_utc = QDateTime::currentDateTimeUtc();
    result.m_benchmark_version = BenchmarkVersion;
#ifdef FAST_DEBUG_MODE
    result.m_benchmark_scene_id = BenchmarkSceneIdCornellBox;
#else
    result.m_benchmark_scene_id = BenchmarkSceneIdFetch1;
#endif
    result.m_cpu_model = m_system_info.get_cpu_id();
    result.m_enabled_cpu_core_count = m_system_info.get_enabled_cpu_core_count();
    result.m_cpu_thread_count = m_system_info.get_cpu_thread_count();
    result.m_render_thread_count = m_ui->spinbox_single_render_threads->value();
    result.m_render_time = static_cast<int>(m_project_manager.get_project()->get_rendering_timer().get_seconds());

    const QJsonDocument json_doc(result.to_json_object());
    const QByteArray json = json_doc.toJson(QJsonDocument::Compact);

    QNetworkRequest request = make_api_request("/submit");
    request.setHeader(QNetworkRequest::ContentTypeHeader, "application/json");

    RENDERER_LOG_DEBUG("posting %s to %s...", json.constData(), request.url().toString().toLocal8Bit().constData());

    QNetworkReply* reply = m_network_access_manager.post(request, json);
    assert(reply != nullptr);

    connect(
        reply, &QNetworkReply::finished,
        this, &MainWindow::slot_result_submit_query_finished);

    m_ui->pushbutton_results_submit->setEnabled(false);
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
    if (m_rendering_manager->is_rendering())
    {
        if (ask_abort_rendering_confirmation(this) != QMessageBox::Yes)
        {
            event->ignore();
            return;
        }

        m_rendering_manager->abort_rendering();
        m_rendering_manager->wait_until_rendering_end();
    }

    if (!can_close_project())
    {
        event->ignore();
        return;
    }

    save_ui_settings();

    m_project_manager.close_project();

    event->accept();
}

void MainWindow::slot_open_project_complete(const QString& filepath, const bool successful)
{
    if (!successful)
        show_init_error_and_terminate(this, "Failed to load benchmarking project.");

    assert(m_project_manager.is_project_open());

#ifdef FAST_DEBUG_MODE
    Project* project = m_project_manager.get_project();

    Configuration* config = project->configurations().get_by_name("final");
    config->get_parameters().insert_path("uniform_pixel_renderer.samples", 1);

    Frame* frame = project->get_frame();
    ParamArray frame_params = frame->get_parameters();
    frame_params.insert("resolution", "128 128");
    frame_params.insert("tile_size", "16 16");
    project->set_frame(FrameFactory::create(frame->get_name(), frame_params, frame->aovs(), frame->lpe_aovs()));
#endif

    m_rendering_time_display->set_rendering_timer(&m_project_manager.get_project()->get_rendering_timer());
    m_rendering_time_display->update();

    clear_layout(m_ui->frame_main_window->layout());
    build_render_widget();
    build_chart_widget();

    update_benchmark_controls();

    m_ui->pushbutton_start_single_render->setFocus();
}

void MainWindow::initialize_ocio()
{
    try
    {
        const std::string default_ocio_config_filepath = (m_root_path / "ocio" / "config.ocio").string();
        m_ocio_config = OCIO::Config::CreateFromFile(default_ocio_config_filepath.c_str());
        OCIO::SetCurrentConfig(m_ocio_config);
    }
    catch (const OCIO::Exception&)
    {
        show_init_error_and_terminate(this, "Failed to initialize OpenColorIO.");
    }
}

namespace
{
    const char* SettingsFilename = "appleseed.bench.xml";
}

void MainWindow::load_ui_settings()
{
    const QSettings qt_settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    restoreGeometry(qt_settings.value("main_window_geometry").toByteArray());
    restoreState(qt_settings.value("main_window_state").toByteArray());
}

void MainWindow::save_ui_settings()
{
    QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    settings.setValue("main_window_geometry", saveGeometry());
    settings.setValue("main_window_state", saveState());
}

void MainWindow::load_app_settings()
{
    Dictionary settings;
    if (Application::load_settings(SettingsFilename, settings, global_logger(), LogMessage::Info))
    {
        m_application_settings = settings;
        slot_apply_application_settings();
    }
}

void MainWindow::save_app_settings()
{
    Application::save_settings(SettingsFilename, m_application_settings, global_logger(), LogMessage::Info);
}

void MainWindow::slot_load_all_settings()
{
    load_ui_settings();
    load_app_settings();
}

void MainWindow::slot_save_all_settings()
{
    save_ui_settings();
    save_app_settings();
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

    emit signal_application_settings_modified();
}

void MainWindow::slot_start_single_render()
{
    if (m_rendering_manager->is_rendering())
        m_rendering_manager->abort_rendering();
    else
    {
        m_chart_widget->hide();
        m_ui->action_file_export_chart->setEnabled(false);

        start_rendering(BenchmarkingMode::SingleRender);
    }

    update_benchmark_controls();
}

void MainWindow::slot_start_scaling_test()
{
    if (m_rendering_manager->is_rendering())
        m_rendering_manager->abort_rendering();
    else
    {
        reset_chart_for_scaling_test();
        m_chart_widget->show();
        m_ui->action_file_export_chart->setEnabled(false);

        m_scaling_test_params.reset();
        start_rendering(BenchmarkingMode::ScalingTest);
    }

    update_benchmark_controls();
}

void MainWindow::slot_start_throttling_test()
{
    if (m_rendering_manager->is_rendering())
        m_rendering_manager->abort_rendering();
    else
    {
        reset_chart_for_throtting_test();
        m_chart_widget->show();
        m_ui->action_file_export_chart->setEnabled(false);

        m_throttling_test_params.reset();
        start_rendering(BenchmarkingMode::ThrottlingTest);

        m_throttling_test_params.start_timer(m_ui->spinbox_throttling_test_duration->value());
    }

    update_benchmark_controls();
}

void MainWindow::slot_throttling_test_graph_update(
    const double    time,
    const quint64   samples,
    const double    samples_per_pixel,
    const quint64   samples_per_second)
{
    QMutexLocker locker(&m_chart_mutex);

    if (time - m_throttling_test_params.m_last_update_time >= 1.0)
    {
        assert(m_chart != nullptr);

        m_chart->add_point(
            time,
            static_cast<double>(samples_per_second));

        m_chart_widget->update();
        m_throttling_test_params.m_last_update_time = time;
    }
}

void MainWindow::slot_rendering_success()
{
    m_rendering_manager->wait_until_rendering_end();

    m_priority_context.reset();

    switch (m_benchmarking_mode)
    {
      case BenchmarkingMode::SingleRender:
        {
            m_benchmarking_mode = BenchmarkingMode::None;

#ifdef ENABLE_RESULT_SUBMISSION
            m_ui->pushbutton_results_submit->setEnabled(m_is_genuine_project);
#endif

            update_benchmark_controls();
        }
        break;

      case BenchmarkingMode::ScalingTest:
        {
            // Update chart.
            assert(m_chart != nullptr);
            const std::size_t thread_count = m_scaling_test_params.get_thread_count();
            const double render_time = m_project_manager.get_project()->get_rendering_timer().get_seconds();
            if (thread_count == 1)
            {
                m_scaling_test_params.m_single_thread_timing = render_time;
                m_chart->add_point(thread_count, 1.0, render_time);
            }
            else
            {
                const double speedup = m_scaling_test_params.m_single_thread_timing / render_time;
                m_chart->add_point(thread_count, speedup, render_time);
            }
            m_chart_widget->update();

            // Terminate or restart render with more threads.
            m_scaling_test_params.next();
            if (m_scaling_test_params.is_complete())
            {
                m_benchmarking_mode = BenchmarkingMode::None;

                m_ui->action_file_export_chart->setEnabled(true);
                update_benchmark_controls();
            }
            else start_rendering(BenchmarkingMode::ScalingTest);
        }
        break;

      case BenchmarkingMode::ThrottlingTest:
        {
            m_throttling_test_params.stop_timer();
            m_benchmarking_mode = BenchmarkingMode::None;

            m_ui->action_file_export_chart->setEnabled(true);
            update_benchmark_controls();
        }
        break;
    }
}

void MainWindow::slot_rendering_abort()
{
    m_priority_context.reset();

    m_throttling_test_params.stop_timer();
    m_benchmarking_mode = BenchmarkingMode::None;

    m_ui->action_file_export_chart->setEnabled(true);
    update_benchmark_controls();
}

void MainWindow::slot_render_widget_context_menu(const QPoint& point)
{
    if (!m_project_manager.is_project_open() || m_rendering_manager->is_rendering())
        return;

    QMenu* menu = new QMenu(this);
    menu->addAction(QString("Save &Render..."), this, SLOT(slot_save_render()));
    menu->exec(m_render_widget->mapToGlobal(point));
}

void MainWindow::slot_save_render()
{
    assert(m_project_manager.is_project_open());
    assert(!m_rendering_manager->is_rendering());

    QString filepath =
        get_save_filename(
            this,
            "Save Render As...",
            g_qt_image_files_filter,
            m_application_settings,
            SETTINGS_FILE_DIALOG_RENDERS);

    if (filepath.isEmpty())
        return;

    if (QFileInfo(filepath).suffix().isEmpty())
        filepath += ".png";

    m_render_widget->save(filepath);

    RENDERER_LOG_INFO("wrote image file %s.", filepath.toStdString().c_str());
}

void MainWindow::slot_chart_widget_context_menu(const QPoint& point)
{
    assert(m_chart_widget->isVisible());

    if (m_benchmarking_mode != BenchmarkingMode::None)
        return;

    QMenu* menu = new QMenu(this);
    menu->addAction(QString("Export &Chart..."), this, SLOT(slot_export_chart()));
    menu->exec(m_chart_widget->mapToGlobal(point));
}

void MainWindow::slot_export_chart()
{
    assert(m_chart_widget->isVisible());
    assert(m_benchmarking_mode == BenchmarkingMode::None);

    QString filepath =
        get_save_filename(
            this,
            "Export Chart As...",
            g_qt_image_files_filter,
            m_application_settings,
            SETTINGS_FILE_DIALOG_CHARTS);

    if (filepath.isEmpty())
        return;

    if (QFileInfo(filepath).suffix().isEmpty())
        filepath += ".png";

    m_chart_widget->grab().save(filepath);

    RENDERER_LOG_INFO("wrote image file %s.", filepath.toStdString().c_str());
}

void MainWindow::slot_submit_result()
{
#ifdef ENABLE_RESULT_SUBMISSION
    if (m_is_genuine_project)
    {
        QMessageBox msgbox(this);
        msgbox.setWindowTitle("Submit Result?");
        msgbox.setIcon(QMessageBox::Information);
        msgbox.setText(
            "You are about to submit your result to an online database.\n\n"
            "Submitted information is fully anonymous.");
        msgbox.setStandardButtons(QMessageBox::Ok | QMessageBox::Cancel);
        if (msgbox.exec() == QMessageBox::Ok)
            submit_result();
    }
    else
    {
        assert(!"This should never happen.");
    }
#else
    assert(!"This should never happen.");
#endif
}

void MainWindow::slot_result_submit_query_finished()
{
    const auto reply = qobject_cast<QNetworkReply*>(sender());

    if (reply->error() == QNetworkReply::NoError)
    {
        RENDERER_LOG_DEBUG("query was successful.");

        try
        {
            const QJsonDocument json_doc = QJsonDocument::fromJson(reply->readAll());

            if (!json_doc.isObject())
                throw ExceptionJsonValue();

            const BenchmarkResult result = BenchmarkResult::from_json_object(json_doc.object());
            m_user_result_id = result.m_id;

            slot_show_rankings_window(m_user_result_id);
        }
        catch (const ExceptionJsonValue&)
        {
            // todo
        }
    }
    else
    {
        QMessageBox msgbox(this);
        msgbox.setWindowTitle("Result Submission Error");
        msgbox.setIcon(QMessageBox::Critical);
        msgbox.setText(
            QString("The following error occurred while submitting your result: %1")
                .arg(filter_api_key(reply->errorString())));
        msgbox.setStandardButtons(QMessageBox::Ok);
        msgbox.exec();
    }

    reply->deleteLater();
}

void MainWindow::slot_show_application_settings_window()
{
    if (m_application_settings_window.get() == nullptr)
    {
        m_application_settings_window.reset(
            new ApplicationSettingsWindow(m_application_settings, this));

        connect(
            m_application_settings_window.get(), &ApplicationSettingsWindow::signal_application_settings_modified,
            this, &MainWindow::slot_save_all_settings);

        connect(
            m_application_settings_window.get(), &ApplicationSettingsWindow::signal_application_settings_modified,
            this, &MainWindow::slot_apply_application_settings);

        connect(
            this, &MainWindow::signal_application_settings_modified,
            m_application_settings_window.get(), &ApplicationSettingsWindow::slot_reload_application_settings);
    }

    m_application_settings_window->showNormal();
    m_application_settings_window->activateWindow();
}

void MainWindow::slot_show_rankings_window(const QString& user_result_id)
{
    m_rankings_window.reset(new RankingsWindow(user_result_id, this));
    m_rankings_window->showNormal();
    m_rankings_window->activateWindow();
}

void MainWindow::slot_show_about_window()
{
    // This window deletes itself on close.
    AboutWindow* about_window = new AboutWindow(this);
    about_window->showNormal();
    about_window->activateWindow();
}

}   // namespace bench
}   // namespace appleseed

#include "mainwindow/moc_cpp_mainwindow.cxx"
