
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Gleb Mishchenko, The appleseedhq Organization
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
#include "mainwindow/pythonconsole/outputredirector.h"
#include "mainwindow/pythonconsole/pythoneditor.h"
#include "utility/miscellaneous.h"

// Qt headers.
#include <QAction>
#include <QSplitter>
#include <QToolBar>
#include <QVBoxLayout>

namespace appleseed {
namespace studio {

//
// PythonConsoleWidget class implementation.
//

PythonConsoleWidget::PythonConsoleWidget(QWidget* parent)
  : QWidget(parent)
{
    QSplitter* console_body = new QSplitter(this);
    console_body->setOrientation(Qt::Vertical);

    m_input = new PythonEditor(console_body);

    m_output = new QPlainTextEdit(console_body);
    m_output->setUndoRedoEnabled(false);
    m_output->setLineWrapMode(QPlainTextEdit::WidgetWidth);
    m_output->setReadOnly(true);
    m_output->setTextInteractionFlags(
        Qt::TextSelectableByMouse |
        Qt::TextSelectableByKeyboard);
    m_output->setFont(m_input->font());

    console_body->addWidget(m_input);
    console_body->addWidget(m_output);

    m_action_execute_all =
        new QAction(load_icons("python_execute_all"), "Execute All Code", this);
    m_action_execute_all->setShortcut(Qt::CTRL + Qt::SHIFT + Qt::Key_Return);
    m_action_execute_all->setShortcutContext(Qt::WidgetWithChildrenShortcut);
    connect(m_action_execute_all, SIGNAL(triggered()), this, SLOT(slot_execute_all()));
    addAction(m_action_execute_all);

    m_action_execute_selection =
        new QAction(load_icons("python_execute_selection"), "Execute Selected Code", this);
    m_action_execute_selection->setShortcut(Qt::CTRL + Qt::Key_Return);
    m_action_execute_selection->setShortcutContext(Qt::WidgetWithChildrenShortcut);
    connect(m_action_execute_selection, SIGNAL(triggered()), this, SLOT(slot_execute_selection()));
    addAction(m_action_execute_selection);

    m_action_clear_selection =
        new QAction(load_icons("python_clear_output"), "Clear Console Output", this);
    m_action_clear_selection->setShortcut(Qt::CTRL + Qt::Key_D);
    m_action_clear_selection->setShortcutContext(Qt::WidgetWithChildrenShortcut);
    connect(m_action_clear_selection, SIGNAL(triggered()), this, SLOT(slot_clear_output()));
    addAction(m_action_clear_selection);

    m_action_focus_on_input = new QAction("Focus on Console Input", this);
    m_action_focus_on_input->setShortcut(Qt::CTRL + Qt::Key_L);
    connect(m_action_focus_on_input, SIGNAL(triggered()), m_input, SLOT(setFocus()));
    addAction(m_action_focus_on_input);

    QToolBar* toolbar = new QToolBar(this);
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

void PythonConsoleWidget::execute(const QString& script)
{
    PythonInterpreter::instance().execute(script.toStdString().c_str());
}

}   // namespace studio
}   // namespace appleseed
