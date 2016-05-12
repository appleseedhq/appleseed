
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_DEBUG_BENCHMARKS_BENCHMARKWINDOW_H
#define APPLESEED_STUDIO_DEBUG_BENCHMARKS_BENCHMARKWINDOW_H

// appleseed.studio headers.
#include "debug/benchmarks/benchmarkrunnerthread.h"
#include "utility/chartwidget.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/benchmark.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QObject>
#include <QWidget>

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace Ui    { class BenchmarkWindow; }
class QCloseEvent;

namespace appleseed {
namespace studio {

class BenchmarkWindow
  : public QWidget
{
    Q_OBJECT

  public:
    // Constructor.
    explicit BenchmarkWindow(QWidget* parent = 0);

    // Destructor.
    ~BenchmarkWindow();

    virtual void closeEvent(QCloseEvent* event) APPLESEED_OVERRIDE;

  private:
    // Not wrapped in std::auto_ptr<> to avoid pulling in the UI definition code.
    Ui::BenchmarkWindow*                m_ui;

    ChartWidget                         m_chart_widget;

    BenchmarkRunnerThread               m_benchmark_runner_thread;
    foundation::BenchmarkAggregator     m_benchmark_aggregator;

    void build_connections();

    void populate_benchmarks_treeview();

    void reload_benchmarks();

    void enable_widgets(const bool enabled);

    std::auto_ptr<ChartBase> create_chart(
        const foundation::UniqueID      case_uid,
        const size_t                    chart_index) const;

  private slots:
    void slot_run_benchmarks();
    void slot_on_benchmarks_execution_complete();
    void slot_rebuild_charts();
    void slot_on_equidistant_checkbox_state_changed(int state);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_DEBUG_BENCHMARKS_BENCHMARKWINDOW_H
