
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
#include "logwidget.h"

// Qt headers.
#include <QAction>
#include <QBrush>
#include <QContextMenuEvent>
#include <QKeySequence>
#include <QMenu>
#include <QScrollBar>
#include <QTextCharFormat>
#include <QTextCursor>

namespace appleseed {
namespace qtcommon {

//
// LogWidget class implementation.
//

LogWidget::LogWidget(QWidget* parent)
  : QTextEdit(parent)
{
    m_action_clear_all = new QAction("Clear All", this);
    m_action_clear_all->setShortcut(QKeySequence("Ctrl+K"));
    connect(m_action_clear_all, &QAction::triggered, this, &LogWidget::slot_clear_all);
    addAction(m_action_clear_all);
}

void LogWidget::slot_append_item(const QColor& color, const QString& text)
{
    const QTextCursor old_cursor = textCursor();
    const int old_scrollbar_value = verticalScrollBar()->value();
    const bool is_scrolled_down = old_scrollbar_value == verticalScrollBar()->maximum();

    // Move the cursor to the end of the document.
    moveCursor(QTextCursor::End);

    QTextCharFormat format;
    format.setForeground(QBrush(color));

    // Insert the text at the position of the cursor (which is the end of the document).
    QTextCursor cursor(textCursor());
    cursor.setCharFormat(format);
    cursor.insertText(text);

    if (old_cursor.hasSelection() || !is_scrolled_down)
    {
        // The user has selected text or scrolled away from the bottom: maintain position.
        setTextCursor(old_cursor);
        verticalScrollBar()->setValue(old_scrollbar_value);
    }
    else
    {
        // The user hasn't selected any text and the scrollbar is at the bottom: scroll to the bottom.
        moveCursor(QTextCursor::End);
        verticalScrollBar()->setValue(verticalScrollBar()->maximum());
    }
}

void LogWidget::slot_clear_all()
{
    clear();
}

void LogWidget::contextMenuEvent(QContextMenuEvent* event)
{
    QMenu* context_menu = createStandardContextMenu();
    context_menu->addAction(m_action_clear_all);
    context_menu->exec(event->globalPos());
    delete context_menu;
}

}   // namespace qtcommon
}   // namespace appleseed
