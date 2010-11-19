
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

// boost headers.
#include "boost/filesystem/path.hpp"

// Qt headers.
#include <QKeySequence>
#include <QPushButton>
#include <QShortCut>
#include <Qt>

using namespace appleseed::shared;
using namespace boost;

namespace appleseed {
namespace studio {

//
// BenchmarkWindow class implementation.
//

BenchmarkWindow::BenchmarkWindow(QWidget* parent)
  : QWidget(parent)
  , m_ui(new Ui::BenchmarkWindow())
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    m_ui->splitter->setSizes(QList<int>() << 600 << 300);

    build_connections();

    configure_benchmarks_treeview();

    reload_benchmarks();

    connect(
        &m_benchmark_runner_thread, SIGNAL(signal_finished()),
        this, SLOT(slot_on_benchmarks_execution_complete()));
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
}

void BenchmarkWindow::configure_benchmarks_treeview()
{
    m_ui->treewidget_benchmarks->setHeaderLabels(QStringList() << "Benchmark");
    m_ui->treewidget_benchmarks->sortItems(0, Qt::AscendingOrder);
}

void BenchmarkWindow::reload_benchmarks()
{
    const filesystem::path benchmarks_path =
          filesystem::path(Application::get_tests_root_path())
        / "benchmarks/";

    m_benchmark_aggregator.clear();
    m_benchmark_aggregator.scan_directory(benchmarks_path.string().c_str());
}

void BenchmarkWindow::enable_widgets(const bool enabled)
{
    m_ui->pushbutton_run->setEnabled(enabled);
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

}   // namespace studio
}   // namespace appleseed
