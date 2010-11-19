
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

// Qt headers.
#include <QKeySequence>
#include <QPushButton>
#include <QShortCut>
#include <Qt>

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
}

}   // namespace studio
}   // namespace appleseed
