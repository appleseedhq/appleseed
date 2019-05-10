
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
#include "benchmarkwindow.h"

// UI definition header.
#include "ui_benchmarkwindow.h"

// appleseed.shared headers.
#include "application/application.h"

// appleseed.foundation headers.
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/countof.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/date_time/posix_time/posix_time.hpp"
#include "boost/filesystem/path.hpp"

// Qt headers.
#include <QKeySequence>
#include <QList>
#include <QPushButton>
#include <QShortcut>
#include <QStringList>
#include <Qt>
#include <QTreeWidget>
#include <QTreeWidgetItem>
#include <QVariant>

// Standard headers.
#include <string>
#include <utility>

using namespace appleseed::shared;
using namespace boost;
using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;

namespace appleseed {
namespace studio {

//
// BenchmarkWindow class implementation.
//

BenchmarkWindow::BenchmarkWindow(QWidget* parent)
  : WindowBase(parent, "benchmark_window")
  , m_ui(new Ui::BenchmarkWindow())
  , m_chart_widget(this)
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    m_ui->splitter->setSizes(QList<int>() << 600 << 300);

    build_connections();

    m_chart_widget.setProperty("hasFrame", true);
    m_ui->graphs_contents->layout()->addWidget(&m_chart_widget);

    m_ui->treewidget_benchmarks->setHeaderLabels(QStringList() << "Benchmark");
    m_ui->treewidget_benchmarks->sortItems(0, Qt::AscendingOrder);

    reload_benchmarks();

    connect(
        &m_benchmark_runner_thread, SIGNAL(signal_finished()),
        this, SLOT(slot_on_benchmarks_execution_complete()));

    WindowBase::load_settings();
}

BenchmarkWindow::~BenchmarkWindow()
{
    delete m_ui;
}

void BenchmarkWindow::build_connections()
{
    connect(
        m_ui->buttonbox->button(QDialogButtonBox::Close), SIGNAL(clicked()),
        this, SLOT(close()));

    connect(
        new QShortcut(QKeySequence(Qt::Key_Escape), this), SIGNAL(activated()),
        this, SLOT(close()));

    connect(
        m_ui->pushbutton_run, SIGNAL(clicked()),
        this, SLOT(slot_run_benchmarks()));

    connect(
        m_ui->treewidget_benchmarks, SIGNAL(itemSelectionChanged()),
        this, SLOT(slot_rebuild_charts()));

    connect(
        m_ui->checkbox_equidistant, SIGNAL(stateChanged(int)),
        this, SLOT(slot_on_equidistant_checkbox_state_changed(int)));
}

namespace
{
    template <typename ParentWidget>
    void add_benchmarks(
        const Dictionary&   benchmarks,
        ParentWidget*       parent)
    {
        for (const_each<DictionaryDictionary> i = benchmarks.dictionaries(); i; ++i)
        {
            QTreeWidgetItem* item =
                new QTreeWidgetItem(
                    parent,
                    QStringList(i->key()));

            add_benchmarks(i->value(), item);
        }

        for (const_each<StringDictionary> i = benchmarks.strings(); i; ++i)
        {
            QTreeWidgetItem* item =
                new QTreeWidgetItem(
                    parent,
                    QStringList(i->key()));

            item->setData(0, Qt::UserRole, QVariant::fromValue(i->value<UniqueID>()));
        }
    }
}

void BenchmarkWindow::populate_benchmarks_treeview()
{
    m_ui->treewidget_benchmarks->clear();

    const Dictionary& benchmarks = m_benchmark_aggregator.get_benchmarks();

    add_benchmarks(benchmarks, m_ui->treewidget_benchmarks);
}

void BenchmarkWindow::reload_benchmarks()
{
    const bf::path benchmarks_path =
          bf::path(Application::get_tests_root_path())
        / "unit benchmarks" / "results";

    m_benchmark_aggregator.clear();
    m_benchmark_aggregator.scan_directory(benchmarks_path.string().c_str());
    m_benchmark_aggregator.sort_series();

    populate_benchmarks_treeview();
}

void BenchmarkWindow::enable_widgets(const bool enabled)
{
    m_ui->pushbutton_run->setEnabled(enabled);
}

namespace
{
    struct ToolTipFormatter
      : public IToolTipFormatter
    {
        QString format(const Vector2d& point) const override
        {
            const uint64 date_microseconds = static_cast<uint64>(point.x);
            const posix_time::ptime date =
                BenchmarkDataPoint::microseconds_to_ptime(date_microseconds);
            const string date_string = posix_time::to_simple_string(date);

            const string ticks_string =
                pretty_scalar(point.y) + " " +
                plural(point.y, "tick");

            return
                QString("%1\n%2")
                    .arg(QString::fromStdString(date_string))
                    .arg(QString::fromStdString(ticks_string));
        }
    };
}

unique_ptr<ChartBase> BenchmarkWindow::create_chart(
    const UniqueID      case_uid,
    const size_t        chart_index) const
{
    unique_ptr<LineChart> chart(new LineChart());

    chart->set_equidistant(m_ui->checkbox_equidistant->isChecked());

    chart->set_grid_brush(QBrush(QColor(60, 60, 60, 255)));

    static QColor CurveColors[] =
    {
        QColor(190, 140,  50, 255),
        QColor(160,  30,  30, 255),
        QColor( 30, 160,  30, 255),
        QColor( 30,  30, 160, 255),
        QColor(160, 160,  30, 255),
        QColor(160,  30, 160, 255),
        QColor( 30, 160, 160, 255),
        QColor(160, 160, 160, 255)
    };

    chart->set_curve_brush(QBrush(CurveColors[chart_index % COUNT_OF(CurveColors)]));

    unique_ptr<IToolTipFormatter> formatter(new ToolTipFormatter());
    chart->set_tooltip_formatter(std::move(formatter));

    const BenchmarkSeries& series = m_benchmark_aggregator.get_series(case_uid);

    for (size_t i = 0; i < series.size(); ++i)
    {
        const BenchmarkDataPoint& point = series[i];

        const double x =
            static_cast<double>(
                BenchmarkDataPoint::ptime_to_microseconds(point.get_date()));

        chart->add_point(x, point.get_ticks());
    }

    return unique_ptr<ChartBase>(std::move(chart));
}

void BenchmarkWindow::slot_run_benchmarks()
{
    if (!m_benchmark_runner_thread.isRunning())
    {
        enable_widgets(false);

        m_benchmark_runner_thread.start();
    }
}

void BenchmarkWindow::slot_on_benchmarks_execution_complete()
{
    reload_benchmarks();

    enable_widgets(true);
}

void BenchmarkWindow::slot_rebuild_charts()
{
    m_chart_widget.clear();

    const QList<QTreeWidgetItem*> items = m_ui->treewidget_benchmarks->selectedItems();

    for (int i = 0; i < items.size(); ++i)
    {
        const QVariant data = items[i]->data(0, Qt::UserRole);

        if (!data.isNull())
        {
            const UniqueID case_uid = data.value<UniqueID>();
            m_chart_widget.add_chart(create_chart(case_uid, i));
        }
    }

    m_chart_widget.update();
}

void BenchmarkWindow::slot_on_equidistant_checkbox_state_changed(int state)
{
    slot_rebuild_charts();
}

}   // namespace studio
}   // namespace appleseed
