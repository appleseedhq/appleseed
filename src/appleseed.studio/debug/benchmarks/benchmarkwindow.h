
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
#include "debug/benchmarks/benchmarkrunnerthread.h"
#include "utility/windowbase.h"

// appleseed.qtcommon headers.
#include "widgets/chartwidget.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/benchmark.h"
#include "foundation/utility/uid.h"

// Qt headers.
#include <QObject>

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace Ui    { class BenchmarkWindow; }
class QWidget;

namespace appleseed {
namespace studio {

class BenchmarkWindow
  : public WindowBase
{
    Q_OBJECT

  public:
    // Constructor.
    explicit BenchmarkWindow(QWidget* parent = nullptr);

    // Destructor.
    ~BenchmarkWindow() override;

  private:
    // Not wrapped in std::unique_ptr<> to avoid pulling in the UI definition code.
    Ui::BenchmarkWindow*                m_ui;

    qtcommon::ChartWidget               m_chart_widget;

    BenchmarkRunnerThread               m_benchmark_runner_thread;
    foundation::BenchmarkAggregator     m_benchmark_aggregator;

    void build_connections();

    void populate_benchmarks_treeview();

    void reload_benchmarks();

    void enable_widgets(const bool enabled);

    std::unique_ptr<qtcommon::ChartBase> create_chart(
        const foundation::UniqueID      case_uid,
        const size_t                    chart_index) const;

  private slots:
    void slot_run_benchmarks();
    void slot_on_benchmarks_execution_complete();
    void slot_rebuild_charts();
};

}   // namespace studio
}   // namespace appleseed
