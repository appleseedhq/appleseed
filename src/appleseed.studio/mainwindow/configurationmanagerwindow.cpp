
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
#include "configurationmanagerwindow.h"

// UI definition header.
#include "ui_configurationmanagerwindow.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// Qt headers.
#include <QShortcut>
#include <Qt>

using namespace appleseed::qtcommon;

namespace appleseed {
namespace studio {

//
// ConfigurationManagerWindow class implementation.
//

ConfigurationManagerWindow::ConfigurationManagerWindow(QWidget* parent)
  : WindowBase(parent, "configuration_manager_window")
  , m_ui(new Ui::ConfigurationManagerWindow())
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    disable_osx_focus_rect(m_ui->treewidget_configurations);

    connect(m_ui->buttonbox, SIGNAL(accepted()), this, SLOT(close()));
    connect(m_ui->buttonbox, SIGNAL(rejected()), this, SLOT(close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Return), SIGNAL(activated()),
        this, SLOT(close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Enter), SIGNAL(activated()),
        this, SLOT(close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), SIGNAL(activated()),
        this, SLOT(close()));

    WindowBase::load_settings();
}

ConfigurationManagerWindow::~ConfigurationManagerWindow()
{
    delete m_ui;
}

}   // namespace studio
}   // namespace appleseed
