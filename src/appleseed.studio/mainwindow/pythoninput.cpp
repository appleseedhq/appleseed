
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
#include "pythoninput.h"

// Qt headers.
#include <QKeyEvent>
#include <QString>
#include <QStringList>

// Standard headers.
#include <string>

namespace appleseed {
namespace studio {

PythonInput::PythonInput(QWidget* parent)
{
    setUndoRedoEnabled(true);
    setLineWrapMode(QTextEdit::WidgetWidth);
    setReadOnly(false);
    setTextInteractionFlags(
        Qt::TextSelectableByMouse |
        Qt::TextSelectableByKeyboard |
        Qt::TextEditable |
        Qt::TextEditorInteraction);
}

void PythonInput::keyPressEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Tab)
    {
        insert_spaces(4);
    }
    else
    {
        QTextEdit::keyPressEvent(event);
        if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter)
            indent_like_previous();
    }
}

void PythonInput::indent_like_previous()
{
    QStringList splitted = toPlainText().split('\n');
    std::string previous = splitted[splitted.size() - 2].toStdString();

    int indentation = 0;
    for (int i = 0; i < previous.size(); ++i)
    {
        if (previous[i] == ' ')
            indentation++;
        else
            break;
    }

    insert_spaces(indentation);
}

void PythonInput::insert_spaces(int count)
{
    std::string spaces = "";
    for (int i = 0; i < count; ++i)
        spaces.push_back(' ');

    insertPlainText(spaces.c_str());
}

} // namespace studio
} // namespace appleseed