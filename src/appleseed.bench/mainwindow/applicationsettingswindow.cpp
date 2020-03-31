
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
#include "applicationsettingswindow.h"

// UI definition header.
#include "ui_applicationsettingswindow.h"

// appleseed.bench headers.
#include "utility/settingskeys.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/log/logmessage.h"
#include "foundation/platform/system.h"
#include "foundation/string/string.h"

// Qt headers.
#include <QDialogButtonBox>
#include <QKeySequence>
#include <QShortcut>
#include <Qt>

// Standard headers.
#include <string>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace bench {

//
// ApplicationSettingsWindow class implementation.
//

ApplicationSettingsWindow::ApplicationSettingsWindow(ParamArray& settings, QWidget* parent)
  : WindowBase(parent, "application_settings_window")
  , m_ui(new Ui::ApplicationSettingsWindow())
  , m_settings(settings)
{
    m_ui->setupUi(this);
    setWindowFlags(Qt::Window);
    build_connections();
    WindowBase::load_settings();
    load_settings();
}

ApplicationSettingsWindow::~ApplicationSettingsWindow()
{
    delete m_ui;
}

void ApplicationSettingsWindow::slot_reload_application_settings()
{
    load_settings();
}

void ApplicationSettingsWindow::build_connections()
{
    connect(
        m_ui->buttonbox, &QDialogButtonBox::accepted,
        this, &ApplicationSettingsWindow::slot_save_configuration_and_close);

    connect(
        m_ui->buttonbox, &QDialogButtonBox::rejected,
        this, &ApplicationSettingsWindow::slot_restore_configuration_and_close);

    connect(
        create_window_local_shortcut(this, Qt::Key_Return), &QShortcut::activated,
        this, &ApplicationSettingsWindow::slot_save_configuration_and_close);

    connect(
        create_window_local_shortcut(this, Qt::Key_Enter), &QShortcut::activated,
        this, &ApplicationSettingsWindow::slot_save_configuration_and_close);

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), &QShortcut::activated,
        this, &ApplicationSettingsWindow::slot_save_configuration_and_close);
}

void ApplicationSettingsWindow::load_settings()
{
    // Message verbosity.
    const std::string message_verbosity =
        m_settings.get_path_optional<std::string>(SETTINGS_MESSAGE_VERBOSITY, "info");
    m_ui->combobox_message_verbosity->setCurrentIndex(
        message_verbosity == "debug"   ? 0 :
        message_verbosity == "info"    ? 1 :
        message_verbosity == "warning" ? 2 :
        message_verbosity == "error"   ? 3 :
        message_verbosity == "fatal"   ? 4 :
        1);     // "info" if an unknown value was found
}

void ApplicationSettingsWindow::save_settings()
{
    // Message verbosity.
    const auto message_verbosity_index = m_ui->combobox_message_verbosity->currentIndex();
    m_settings.insert_path(
        SETTINGS_MESSAGE_VERBOSITY,
        message_verbosity_index == 0 ? "debug" :
        message_verbosity_index == 1 ? "info" :
        message_verbosity_index == 2 ? "warning" :
        message_verbosity_index == 3 ? "error" :
                                       "fatal");

    emit signal_application_settings_modified();
}

void ApplicationSettingsWindow::slot_save_configuration_and_close()
{
    save_settings();
    close();
}

void ApplicationSettingsWindow::slot_restore_configuration_and_close()
{
    load_settings();
    close();
}

}   // namespace bench
}   // namespace appleseed

#include "mainwindow/moc_cpp_applicationsettingswindow.cxx"
