
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "aboutwindow.h"

// UI definition header.
#include "ui_aboutwindow.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QKeySequence>
#include <QShortcut>
#include <QString>
#include <Qt>

using namespace foundation;

namespace appleseed {
namespace studio {

AboutWindow::AboutWindow(QWidget* parent)
  : QWidget(parent)
  , m_ui(new Ui::AboutWindow())
{
    m_ui->setupUi(this);

    setAttribute(Qt::WA_DeleteOnClose);

    setWindowFlags(
        Qt::Dialog |
        Qt::CustomizeWindowHint |
        Qt::WindowCloseButtonHint);

    set_version_string();

    setFixedSize(width(), sizeHint().height());

    connect(m_ui->dialogbuttonbox, SIGNAL(accepted()), this, SLOT(close()));

    connect(
        new QShortcut(QKeySequence(Qt::Key_Escape), this), SIGNAL(activated()),
        this, SLOT(close()));
}

AboutWindow::~AboutWindow()
{
    delete m_ui;
}

void AboutWindow::set_version_string()
{
    const QString version_string =
        QString(
            "<p><b>Version %1</b>, %4 Configuration</p>"
            "<p>Compiled on %5 at %6 using %7 version %8</p>")
            .arg(Appleseed::get_lib_version())
            .arg(Appleseed::get_lib_configuration())
            .arg(Appleseed::get_lib_compilation_date())
            .arg(Appleseed::get_lib_compilation_time())
            .arg(Compiler::get_compiler_name())
            .arg(Compiler::get_compiler_version());

    m_ui->label_version_string->setText(version_string);
}

}   // namespace studio
}   // namespace appleseed
