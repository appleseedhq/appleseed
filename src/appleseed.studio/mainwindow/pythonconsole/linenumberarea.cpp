
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
#include "linenumberarea.h"

// appleseed.studio headers.
#include "mainwindow/pythonconsole/pythoneditor.h"

// Qt headers.
#include <QFont>
#include <QPainter>
#include <QTextBlock>

namespace appleseed {
namespace studio {

LineNumberArea::LineNumberArea(PythonEditor* parent)
  : QWidget(parent), editor(parent)
{
    connect(editor, SIGNAL(blockCountChanged(int)), this, SLOT(slot_update_area_width()));
    connect(editor, SIGNAL(updateRequest(QRect, int)), this, SLOT(slot_update_area(QRect, int)));
    connect(editor, SIGNAL(fontChanged(QFont)), this, SLOT(slot_change_font(QFont)));

    slot_update_area_width();
}

void LineNumberArea::slot_update_area_width()
{
    setFixedWidth(area_width());
    editor->setViewportMargins(area_width(), 0, 0, 0);
}

void LineNumberArea::slot_update_area(const QRect& rect, int dy)
{
    if (dy != 0)
        scroll(0, dy);
    else
        update(0, rect.y(), area_width(), rect.height());

    if (rect.contains(editor->viewport()->rect()))
        slot_update_area_width();
}

int LineNumberArea::area_width()
{
    int digits = 1;
    int max = qMax(1, editor->blockCount());
    while (max >= 10)
    {
        max /= 10;
        ++digits;
    }

    return 3 + fontMetrics().boundingRect(QLatin1Char('9')).width() * digits;
}

void LineNumberArea::paintEvent(QPaintEvent* event)
{
    QPainter painter(this);
    painter.fillRect(event->rect(), QColor(53, 53, 53));

    QTextBlock block = editor->firstVisibleBlock();
    int block_number = block.blockNumber();
    int top = static_cast<int>(editor->blockBoundingGeometry(block).
                               translated(editor->contentOffset()).top());
    int bottom = top + static_cast<int>(editor->blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom())
    {
        if (block.isVisible() && bottom >= event->rect().top())
        {
            painter.setPen(QColor(100, 100, 100));
            painter.drawText(
                0, top,
                width(), fontMetrics().height(),
                Qt::AlignRight,
                QString::number(block_number + 1));
        }

        block = block.next();
        top = bottom;
        bottom = top + static_cast<int>(editor->blockBoundingRect(block).height());
        ++block_number;
    }
}

void LineNumberArea::slot_change_font(const QFont& font)
{
    setFont(font);
    slot_update_area_width();
}

}   // namespace studio
}   // namespace appleseed
