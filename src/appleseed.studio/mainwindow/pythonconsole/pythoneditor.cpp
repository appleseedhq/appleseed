
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
#include "pythoneditor.h"

// appleseed.studio headers.
#include "mainwindow/pythonconsole/linenumberarea.h"
#include "mainwindow/pythonconsole/pythonhighlighter.h"

// Qt headers.
#include <QApplication>
#include <QKeyEvent>
#include <QPainter>
#include <QString>
#include <QStringList>

namespace appleseed {
namespace studio {

PythonEditor::PythonEditor(QWidget* parent)
  : ZoomablePlainTextEdit(parent)
{
    setObjectName("python_editor");
    setUndoRedoEnabled(true);
    setLineWrapMode(QPlainTextEdit::WidgetWidth);
    setReadOnly(false);
    setTextInteractionFlags(
        Qt::TextSelectableByMouse |
        Qt::TextSelectableByKeyboard |
        Qt::TextEditable |
        Qt::TextEditorInteraction);

    new PythonSyntaxHighlighter(this->document());

    m_line_number_area = new LineNumberArea(this);

    connect(this, SIGNAL(cursorPositionChanged()), SLOT(slot_highlight_current_line()));
    slot_highlight_current_line();
}

void PythonEditor::resizeEvent(QResizeEvent* event)
{
    QPlainTextEdit::resizeEvent(event);

    QRect cr = contentsRect();
    m_line_number_area->setGeometry(QRect(cr.left(), cr.top(), m_line_number_area->width(), cr.height()));
}

void PythonEditor::keyPressEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Tab)
        insert_spaces(4);
    else
    {
        if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter)
            event->setModifiers(event->modifiers() & ~Qt::ShiftModifier);
        ZoomablePlainTextEdit::keyPressEvent(event);
        if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter)
            indent();
    }
}

void PythonEditor::indent()
{
    const QStringList text = toPlainText().split('\n');
    const std::string previous = (text.size() >= 2) ? text[text.size() - 2].toStdString() : "";

    indent_like_previous(previous);

    // If last non-space character in previous line is ':' add 4 spaces indentation.
    for (std::string::const_reverse_iterator it = previous.rbegin();
         it != previous.rend(); ++it)
    {
        if (*it == ':')
        {
            insert_spaces(4);
            break;
        }
        else if (*it != ' ')
            break;
    }
}

void PythonEditor::indent_like_previous(const std::string& previous)
{
    size_t indentation = 0;

    for (size_t i = 0; i < previous.size(); ++i)
    {
        if (previous[i] == ' ')
            indentation++;
        else
            break;
    }

    insert_spaces(indentation);
}

void PythonEditor::insert_spaces(const size_t count)
{
    const std::string spaces(count, ' ');
    insertPlainText(spaces.c_str());
}

void PythonEditor::slot_highlight_current_line()
{
    QTextEdit::ExtraSelection selection;
    selection.format.setBackground(QColor(53, 53, 53));
    selection.format.setProperty(QTextFormat::FullWidthSelection, true);
    selection.cursor = textCursor();
    selection.cursor.clearSelection();

    QList<QTextEdit::ExtraSelection> current_line;
    current_line.append(selection);
    setExtraSelections(current_line);
}

}   // namespace studio
}   // namespace appleseed
