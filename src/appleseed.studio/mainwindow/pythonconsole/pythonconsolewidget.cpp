
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Gleb Mishchenko, The appleseedhq Organization
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
#include "pythonconsolewidget.h"

// appleseed.studio headers.
#include "mainwindow/mainwindow.h"
#include "mainwindow/pythonconsole/outputredirector.h"
#include "mainwindow/pythonconsole/pythoneditor.h"
#include "mainwindow/pythonconsole/pythonoutput.h"
#include "utility/settingskeys.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/utility.h"

// Qt headers.
#include <QAction>
#include <QApplication>
#include <QMessageBox>
#include <QSplitter>
#include <QTextDocumentFragment>
#include <QToolBar>
#include <QVBoxLayout>
#include <QWheelEvent>

// Standard headers.
#include <fstream>

using namespace appleseed::qtcommon;
using namespace renderer;

namespace appleseed {
namespace studio {

//
// PythonConsoleWidget class implementation.
//

PythonConsoleWidget::PythonConsoleWidget(QWidget* parent)
  : QWidget(parent)
  , m_is_file_dirty(false)
  , m_opened_filepath("")
{
    QSplitter* console_body = new QSplitter(this);
    console_body->setOrientation(Qt::Vertical);

    // Output at the top.
    m_output = new PythonOutput(console_body);
    console_body->addWidget(m_output);

    // Input at the bottom.
    m_input = new PythonEditor(console_body);
    connect(m_input, SIGNAL(textChanged()), this, SLOT(slot_file_changed()));
    connect(m_input, SIGNAL(selectionChanged()), this, SLOT(slot_change_exec_selection_button_state()));
    console_body->addWidget(m_input);

    m_action_new_file =
        new QAction(load_icons("project_new"), "New Python Script", this);
    connect(m_action_new_file, SIGNAL(triggered()), this, SLOT(slot_new_file()));
    addAction(m_action_new_file);

    m_action_open_file =
        new QAction(load_icons("project_open"), "Open Python Script", this);
    connect(m_action_open_file, SIGNAL(triggered()), this, SLOT(slot_open_file()));
    addAction(m_action_open_file);

    m_action_save_file =
        new QAction(load_icons("project_save"), "Save Python Script", this);
    connect(m_action_save_file, SIGNAL(triggered()), this, SLOT(slot_save_file()));
    addAction(m_action_save_file);

    m_action_save_file_as =
        new QAction(load_icons("project_reload"), "Save Python Script As...", this);
    connect(m_action_save_file_as, SIGNAL(triggered()), this, SLOT(slot_save_file_as()));
    addAction(m_action_save_file_as);

    const QKeySequence action_execute_all_shortcut(Qt::CTRL + Qt::SHIFT + Qt::Key_Return);
    m_action_execute_all =
        new QAction(
            load_icons("python_execute_all"),
            combine_name_and_shortcut("Execute All Code", action_execute_all_shortcut),
            this);
    m_action_execute_all->setShortcut(action_execute_all_shortcut);
    m_action_execute_all->setShortcutContext(Qt::WidgetWithChildrenShortcut);
    connect(m_action_execute_all, SIGNAL(triggered()), this, SLOT(slot_execute_all()));
    addAction(m_action_execute_all);

    const QKeySequence action_execute_selection_shortcut(Qt::CTRL + Qt::Key_Return);
    m_action_execute_selection =
        new QAction(
            load_icons("python_execute_selection"),
            combine_name_and_shortcut("Execute Selected Code", action_execute_selection_shortcut),
            this);
    m_action_execute_selection->setShortcut(action_execute_selection_shortcut);
    m_action_execute_selection->setShortcutContext(Qt::WidgetWithChildrenShortcut);
    m_action_execute_selection->setEnabled(false);
    connect(m_action_execute_selection, SIGNAL(triggered()), this, SLOT(slot_execute_selection()));
    addAction(m_action_execute_selection);

    const QKeySequence action_clear_selection_shortcut(Qt::CTRL + Qt::Key_D);
    m_action_clear_selection =
        new QAction(
            load_icons("python_clear_output"),
            combine_name_and_shortcut("Clear Console Output", action_clear_selection_shortcut),
            this);
    m_action_clear_selection->setShortcut(action_clear_selection_shortcut);
    m_action_clear_selection->setShortcutContext(Qt::WidgetWithChildrenShortcut);
    connect(m_action_clear_selection, SIGNAL(triggered()), this, SLOT(slot_clear_output()));
    addAction(m_action_clear_selection);

    const QKeySequence action_focus_on_input_shortcut(Qt::CTRL + Qt::Key_L);
    m_action_focus_on_input =
        new QAction(
            combine_name_and_shortcut("Focus on Console Input", action_focus_on_input_shortcut),
            this);
    m_action_focus_on_input->setShortcut(action_focus_on_input_shortcut);
    connect(m_action_focus_on_input, SIGNAL(triggered()), m_input, SLOT(setFocus()));
    addAction(m_action_focus_on_input);

    QToolBar* toolbar = new QToolBar(this);
    toolbar->setObjectName("python_console_toolbar");
    toolbar->addAction(m_action_new_file);
    toolbar->addAction(m_action_open_file);
    toolbar->addAction(m_action_save_file);
    toolbar->addAction(m_action_save_file_as);
    toolbar->addSeparator();
    toolbar->addAction(m_action_execute_selection);
    toolbar->addAction(m_action_execute_all);
    toolbar->addAction(m_action_clear_selection);

    QVBoxLayout* layout = new QVBoxLayout();
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addWidget(toolbar);
    layout->addWidget(console_body);
    setLayout(layout);

    PythonInterpreter::instance().initialize(OutputRedirector(m_output));
}

void PythonConsoleWidget::slot_execute_selection()
{
    // QTextCursor returned by textCursor() function uses QChar(8233) instead of newline.
    // It breaks Python indentation rules so it has to be replaced.
    const QString selected = m_input->textCursor().selectedText().replace(QChar(8233), "\n");
    execute(selected);
}

void PythonConsoleWidget::slot_execute_all()
{
    execute(m_input->toPlainText());
}

void PythonConsoleWidget::slot_clear_output()
{
    m_output->clear();
}

void PythonConsoleWidget::slot_new_file()
{
    if (!can_close_file())
        return;

    close_file();
}

void PythonConsoleWidget::slot_open_file()
{
    if (!can_close_file())
        return;

    // todo: we shouldn't have to go through the Python interpreter to retrieve application settings.
    ParamArray& settings = PythonInterpreter::instance().get_main_window()->get_application_settings();

    QString filepath =
        get_open_filename(
            this,
            "Open...",
            "Python Script File (*.py)",
            settings,
            SETTINGS_FILE_DIALOG_PYTHON_SCRIPTS);

    if (!filepath.isEmpty())
    {
        open_file(filepath.toStdString());
    }
}

void PythonConsoleWidget::slot_save_file()
{
    if (!has_file_path())
        slot_save_file_as();
    else
        save_file(m_opened_filepath);
}

void PythonConsoleWidget::slot_save_file_as()
{
    // todo: we shouldn't have to go through the Python interpreter to retrieve application settings.
    ParamArray& settings = PythonInterpreter::instance().get_main_window()->get_application_settings();

    const QString filepath =
        get_save_filename(
            this,
            "Save As...",
            "Python Script File (*.py)",
            settings,
            SETTINGS_FILE_DIALOG_PYTHON_SCRIPTS);

    if (!filepath.isEmpty())
        save_file(filepath.toStdString());
}

void PythonConsoleWidget::slot_file_changed()
{
    if (m_input->document()->isModified())
        m_is_file_dirty = true;
}

void PythonConsoleWidget::slot_change_exec_selection_button_state()
{
    const bool is_enabled = !m_input->textCursor().selection().isEmpty();
    m_action_execute_selection->setEnabled(is_enabled);
}

void PythonConsoleWidget::wheelEvent(QWheelEvent* event)
{
    if (QApplication::keyboardModifiers().testFlag(Qt::ControlModifier))
    {
        const int new_font_size = font().pointSize() - event->delta() / 120;

        QFont new_font = m_output->font();
        new_font.setPointSize(new_font_size);
        m_output->setFont(new_font);
    }
    else QWidget::wheelEvent(event);
}

namespace
{
    int show_modified_file_message_box(QWidget* parent)
    {
        QMessageBox msgbox(parent);
        msgbox.setWindowTitle("Save Changes?");
        msgbox.setIcon(QMessageBox::Question);
        msgbox.setText("The Python script has been modified.\n\nDo you want to save your changes?");
        msgbox.setStandardButtons(QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
        msgbox.setDefaultButton(QMessageBox::Save);
        return msgbox.exec();
    }
}

bool PythonConsoleWidget::can_close_file()
{
    // Unmodified file: no problem.
    if (!m_is_file_dirty)
        return true;

    // The current file has been modified, ask the user what to do.
    switch (show_modified_file_message_box(this))
    {
        case QMessageBox::Save:
            slot_save_file();
            return true;

        case QMessageBox::Discard:
            return true;

        case QMessageBox::Cancel:
            return false;
    }

    assert(!"Should never be reached.");
    return false;
}

bool PythonConsoleWidget::has_file_path()
{
    return !m_opened_filepath.empty();
}

void PythonConsoleWidget::open_file(const std::string& filepath)
{
    std::fstream file(filepath, std::fstream::in);
    if (file.bad())
    {
        RENDERER_LOG_ERROR("cannot open Python script \"%s\" for reading.", filepath.c_str());
        return;
    }

    close_file();
    m_opened_filepath = filepath;

    while (!file.eof())
    {
        std::string str;
        std::getline(file, str);
        m_input->appendPlainText(str.c_str());
    }

    m_is_file_dirty = false;
}

void PythonConsoleWidget::save_file(std::string filepath)
{
    const size_t extension_start = filepath.rfind('.') + 1;
    std::string extension = "";
    if (extension_start != std::string::npos)
        extension = filepath.substr(extension_start);

    if (extension != "py")
        filepath += ".py";

    std::fstream file(filepath, std::fstream::out);
    if (file.bad())
    {
        RENDERER_LOG_ERROR("cannot open Python script \"%s\" for writing.", filepath.c_str());
        return;
    }

    std::string text = m_input->toPlainText().toStdString();
    file.write(text.c_str(), text.size());

    m_opened_filepath = filepath;
    m_is_file_dirty = false;
}

void PythonConsoleWidget::close_file()
{
    m_input->clear();
    m_opened_filepath.clear();
    m_is_file_dirty = false;
}

void PythonConsoleWidget::execute(const QString& script)
{
    PythonInterpreter::instance().execute(script.toStdString());
}

}   // namespace studio
}   // namespace appleseed
