
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.studio headers.
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
#include <QKeySequence>
#include <QShortcut>
#include <Qt>

// Standard headers.
#include <string>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

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
    connect(m_ui->buttonbox, SIGNAL(accepted()), SLOT(slot_save_configuration_and_close()));
    connect(m_ui->buttonbox, SIGNAL(rejected()), SLOT(slot_restore_configuration_and_close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Return), SIGNAL(activated()),
        SLOT(slot_save_configuration_and_close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Enter), SIGNAL(activated()),
        SLOT(slot_save_configuration_and_close()));

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), SIGNAL(activated()),
        SLOT(slot_restore_configuration_and_close()));

    connect(
        m_ui->checkbox_rendering_threads_auto, SIGNAL(stateChanged(int)),
        SLOT(slot_enable_disable_rendering_threads_spinbox()));
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

    // Sampling mode.
    const std::string sampling_mode =
        m_settings.get_path_optional<std::string>(SETTINGS_SAMPLING_MODE, "qmc");
    m_ui->combobox_sampling_mode->setCurrentIndex(
        sampling_mode == "rng" ? 0 :
        sampling_mode == "qmc" ? 1 :
        1);     // "qmc" if an unknown value was found

    // Rendering threads.
    m_ui->spinbox_rendering_threads->setValue(static_cast<int>(System::get_logical_cpu_core_count()));
    m_ui->spinbox_rendering_threads->setEnabled(false);
    m_ui->checkbox_rendering_threads_auto->setChecked(true);
    const std::string rendering_threads_str =
        m_settings.get_path_optional<std::string>(SETTINGS_RENDERING_THREADS, "auto");
    if (rendering_threads_str != "auto")
    {
        try
        {
            const int rendering_threads = from_string<int>(rendering_threads_str);
            if (rendering_threads > 0)
            {
                m_ui->spinbox_rendering_threads->setEnabled(true);
                m_ui->spinbox_rendering_threads->setValue(rendering_threads);
                m_ui->checkbox_rendering_threads_auto->setChecked(false);
            }
        }
        catch (const ExceptionStringConversionError&)
        {
        }
    }

    // Autosave.
    m_ui->checkbox_autosave->setChecked(
        m_settings.get_path_optional<bool>(SETTINGS_AUTOSAVE, false));

    // Render region triggers rendering.
    m_ui->checkbox_render_region_triggers_rendering->setChecked(
        m_settings.get_path_optional<bool>(SETTINGS_RENDER_REGION_TRIGGERS_RENDERING, true));

    // Print final average luminance.
    m_ui->checkbox_print_final_average_luminance->setChecked(
        m_settings.get_path_optional<bool>(SETTINGS_PRINT_FINAL_AVERAGE_LUMINANCE, false));
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

    // Sampling mode.
    m_settings.insert_path(SETTINGS_SAMPLING_MODE,
        m_ui->combobox_sampling_mode->currentIndex() == 0 ? "rng" : "qmc");

    // Rendering threads.
    std::string rendering_threads_str;
    if (m_ui->checkbox_rendering_threads_auto->isChecked())
        rendering_threads_str = "auto";
    else rendering_threads_str = to_string(m_ui->spinbox_rendering_threads->value());
    m_settings.insert_path(SETTINGS_RENDERING_THREADS, rendering_threads_str);

    // Autosave.
    m_settings.insert_path(SETTINGS_AUTOSAVE, m_ui->checkbox_autosave->isChecked());

    // Render region triggers rendering.
    m_settings.insert_path(SETTINGS_RENDER_REGION_TRIGGERS_RENDERING,
        m_ui->checkbox_render_region_triggers_rendering->isChecked());

    // Print final average luminance.
    m_settings.insert_path(SETTINGS_PRINT_FINAL_AVERAGE_LUMINANCE,
        m_ui->checkbox_print_final_average_luminance->isChecked());

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

void ApplicationSettingsWindow::slot_enable_disable_rendering_threads_spinbox()
{
    m_ui->spinbox_rendering_threads->setEnabled(
        m_ui->checkbox_rendering_threads_auto->checkState() != Qt::Checked);
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/moc_cpp_applicationsettingswindow.cxx"
