
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

// Qt headers.
#include <QAction>
#include <QContextMenuEvent>
#include <QTextEdit>

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

    input = new QTextEdit(this);
    input->setUndoRedoEnabled(true);
    input->setLineWrapMode(QTextEdit::WidgetWidth);
    input->setReadOnly(false);
    input->setTextInteractionFlags(
        Qt::TextSelectableByMouse |
        Qt::TextSelectableByKeyboard |
        Qt::TextEditable |
        Qt::TextEditorInteraction);

    this->insertWidget(0, output);
    this->insertWidget(1, input);
    this->setOrientation(Qt::Vertical);

    m_action_execute_selection = new QAction("Execute selection as python script", this);
    m_action_execute_selection->setShortcut(Qt::CTRL + Qt::Key_Return);
    m_action_execute_selection->setShortcutContext(Qt::WidgetWithChildrenShortcut);

    connect(m_action_execute_selection, SIGNAL(triggered()), this, SLOT(slot_execute_selection()));
    addAction(m_action_execute_selection);

    m_action_execute_all = new QAction("Execute all text in input as python script", this);
    m_action_execute_all->setShortcut(Qt::CTRL + Qt::SHIFT + Qt::Key_Return);
    m_action_execute_all->setShortcutContext(Qt::WidgetWithChildrenShortcut);

    connect(m_action_execute_all, SIGNAL(triggered()), this, SLOT(slot_execute_all()));
    addAction(m_action_execute_all);

    m_action_focus_on_input = new QAction("Focus on console input", this);
    m_action_focus_on_input->setShortcut(Qt::CTRL + Qt::Key_L);

    connect(m_action_focus_on_input, SIGNAL(triggered()), input, SLOT(setFocus()));
    addAction(m_action_focus_on_input);

    PythonInterpreter::instance().redirect_output(OutputRedirector(output));
}

void ConsoleWidget::slot_execute_selection()
{
    QString selected = input->textCursor().selectedText().replace(QChar(8233), "\n");
    execute(selected);
}

void ConsoleWidget::slot_execute_all()
{
    QString script = input->toPlainText().replace(QChar(8233), "\n");
    execute(script);
}

void ConsoleWidget::execute(const QString& script)
{
    PythonInterpreter::instance().execute_command(script.toStdString().c_str());

    m_action_execute_selection->setChecked(false);
    m_action_execute_all->setChecked(false);
}

}   // namespace studio
}   // namespace appleseed
