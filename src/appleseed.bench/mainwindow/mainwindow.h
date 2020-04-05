
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

#pragma once

// appleseed.bench headers.
#include "mainwindow/applicationsettingswindow.h"
#include "mainwindow/rankingswindow.h"
#include "mainwindow/rendering/renderingmanager.h"
#include "mainwindow/renderingtimedisplay.h"
#include "mainwindow/scalingtestparams.h"
#include "mainwindow/throttlingtestparams.h"
#include "utility/systeminfo.h"

// appleseed.qtcommon headers.
#include "project/projectmanager.h"
#include "widgets/qtlogtarget.h"
#include "widgets/renderwidget.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"

// Qt headers.
#include <QMainWindow>
#include <QMutex>
#include <QNetworkAccessManager>
#include <QObject>
#include <QString>
#include <QtGlobal>

// OpenColorIO headers.
#include <OpenColorIO/OpenColorIO.h>
namespace OCIO = OCIO_NAMESPACE;

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>

// Forward declarations.
namespace appleseed { namespace qtcommon { class ChartBase; } }
namespace appleseed { namespace qtcommon { class ChartWidget; } }
namespace renderer  { class Project; }
namespace Ui        { class MainWindow; }
class QCloseEvent;
class QPoint;
class QShortcut;
class QWidget;

namespace appleseed {
namespace bench {

//
// appleseed.bench's main window.
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

  signals:
    void signal_application_settings_modified() const;

  private:
    enum class BenchmarkingMode
    {
        None,
        SingleRender,
        ScalingTest,
        ThrottlingTest
    };

    // Not wrapped in std::unique_ptr<> to avoid pulling in the UI definition code.
    Ui::MainWindow*                             m_ui;

    QString                                     m_start_single_render_button_text;
    QString                                     m_start_scaling_test_button_text;
    QString                                     m_start_throttling_test_button_text;

    QShortcut*                                  m_start_single_render_shortcut;
    QShortcut*                                  m_start_scaling_test_shortcut;
    QShortcut*                                  m_start_throttling_test_shortcut;

    std::unique_ptr<qtcommon::QtLogTarget>      m_log_target;
    std::unique_ptr<qtcommon::RenderWidget>     m_render_widget;
    std::unique_ptr<qtcommon::ChartWidget>      m_chart_widget;
    qtcommon::ChartBase*                        m_chart = nullptr;
    QMutex                                      m_chart_mutex;
    std::unique_ptr<RenderingTimeDisplay>       m_rendering_time_display;

    QNetworkAccessManager                       m_network_access_manager;

    const boost::filesystem::path               m_root_path;
    OCIO::ConstConfigRcPtr                      m_ocio_config;
    const SystemInfo                            m_system_info;

    renderer::ParamArray                        m_application_settings;

    std::unique_ptr<ApplicationSettingsWindow>  m_application_settings_window;
    std::unique_ptr<RankingsWindow>             m_rankings_window;

    qtcommon::ProjectManager                    m_project_manager;
    std::unique_ptr<RenderingManager>           m_rendering_manager;
    bool                                        m_is_genuine_project = false;
    std::unique_ptr<foundation::ProcessPriorityContext>
                                                m_priority_context;

    BenchmarkingMode                            m_benchmarking_mode = BenchmarkingMode::None;
    std::size_t                                 m_render_count = 0;
    ScalingTestParams                           m_scaling_test_params;
    ThrottlingTestParams                        m_throttling_test_params;

    QString                                     m_user_result_id;

    // UI elements.
    void build_menus();
    void build_log_panel();
    void build_system_description();
    void build_render_widget();
    void build_chart_widget();
    void build_connections();
    void load_render_widget_preview_image();

    // UI state management.
    void update_benchmark_controls();

    // Settings I/O.
    void load_ui_settings();
    void save_ui_settings();
    void load_app_settings();
    void save_app_settings();

    // Project file handling.
    renderer::ParamArray get_project_params(const BenchmarkingMode benchmarking_mode) const;
    bool can_close_project();

    // Rendering.
    void start_rendering(const BenchmarkingMode benchmarking_mode);

    // Chart.
    void reset_chart_for_scaling_test();
    void reset_chart_for_throtting_test();

    // Result submission.
    void submit_result();

    // Miscellaneous.
    void initialize_ocio();
    void closeEvent(QCloseEvent* event) override;

  private slots:
    // Project I/O.
    void slot_open_project_complete(const QString& filepath, const bool successful);

    // Settings I/O.
    void slot_load_all_settings();
    void slot_save_all_settings();
    void slot_apply_application_settings();

    // Benchmarks.
    void slot_start_single_render();
    void slot_start_scaling_test();
    void slot_start_throttling_test();
    void slot_throttling_test_graph_update(
        const double    time,
        const quint64   samples,
        const double    samples_per_pixel,
        const quint64   samples_per_second);

    // Rendering.
    void slot_rendering_success();
    void slot_rendering_abort();

    // Render widget actions.
    void slot_render_widget_context_menu(const QPoint& point);
    void slot_save_render();

    // Chart widget actions.
    void slot_chart_widget_context_menu(const QPoint& point);
    void slot_export_chart();

    // Result submission.
    void slot_submit_result();
    void slot_result_submit_query_finished();

    // Child windows.
    void slot_show_application_settings_window();
    void slot_show_rankings_window(const QString& user_result_id);
    void slot_show_about_window();
};

}   // namespace bench
}   // namespace appleseed
