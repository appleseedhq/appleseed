
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
#include "consolewidget.h"

// appleseed.studio headers.
#include "outputredirector.h"
#include "pythoninput.h"

// Qt headers.
#include <QAction>
#include <QContextMenuEvent>
#include <QToolBar>

namespace appleseed {
namespace studio {

//
// ConsoleWidget class implementation.
//

ConsoleWidget::ConsoleWidget(QWidget* parent)
  : QSplitter(parent)
{
    output = new QTextEdit(this);
    output->setUndoRedoEnabled(false);
    output->setLineWrapMode(QTextEdit::WidgetWidth);
    output->setReadOnly(true);
    output->setTextInteractionFlags(
        Qt::TextSelectableByMouse |
        Qt::TextSelectableByKeyboard);

    input = new PythonInput(this);

    init_actions();

    QToolBar* toolbar = new QToolBar();
    toolbar->addAction(m_action_execute_selection);
    toolbar->addAction(m_action_execute_all);
    toolbar->addAction(m_action_clear_selection);

    insertWidget(0, toolbar);
    insertWidget(1, output);
    insertWidget(2, input);
    setOrientation(Qt::Vertical);

    PythonInterpreter::instance().redirect_output(OutputRedirector(output));
}

void ConsoleWidget::init_actions()
{
    m_action_execute_selection =
        new QAction(QIcon(":icons/exec_button_icon.png"), "Execute Selection", this);
    m_action_execute_selection->setShortcut(Qt::CTRL + Qt::Key_Return);
    m_action_execute_selection->setShortcutContext(Qt::WidgetWithChildrenShortcut);

    connect(m_action_execute_selection, SIGNAL(triggered()), this, SLOT(slot_execute_selection()));
    addAction(m_action_execute_selection);

    m_action_execute_all =
        new QAction(QIcon(":icons/exec_all_button_icon.png"), "Execute All Text", this);
    m_action_execute_all->setShortcut(Qt::CTRL + Qt::SHIFT + Qt::Key_Return);
    m_action_execute_all->setShortcutContext(Qt::WidgetWithChildrenShortcut);

    connect(m_action_execute_all, SIGNAL(triggered()), this, SLOT(slot_execute_all()));
    addAction(m_action_execute_all);

    m_action_clear_selection =
        new QAction(QIcon(":icons/clear_button_icon.png"), "Clear Console Output", this);
    m_action_clear_selection->setShortcut(Qt::CTRL + Qt::Key_D);
    m_action_clear_selection->setShortcutContext(Qt::WidgetWithChildrenShortcut);

    connect(m_action_clear_selection, SIGNAL(triggered()), this, SLOT(slot_clear_output()));
    addAction(m_action_clear_selection);

    m_action_focus_on_input = new QAction("Focus on Console Input", this);
    m_action_focus_on_input->setShortcut(Qt::CTRL + Qt::Key_L);

    connect(m_action_focus_on_input, SIGNAL(triggered()), input, SLOT(setFocus()));
    addAction(m_action_focus_on_input);
}

void ConsoleWidget::slot_execute_selection()
{
    // QTextCursor returned by textCursor() function uses QChar(8233) instead of newline.
    // It breaks Python indentation rules so it has to be replaced.
    const QString selected = input->textCursor().selectedText().replace(QChar(8233), "\n");
    execute(selected);
}

void ConsoleWidget::slot_execute_all()
{
    execute(input->toPlainText());
}

void ConsoleWidget::slot_clear_output()
{
    output->clear();
}

void ConsoleWidget::execute(const QString& script)
{
    PythonInterpreter::instance().execute_command(script.toStdString().c_str());
}

}   // namespace studio
}   // namespace appleseed
