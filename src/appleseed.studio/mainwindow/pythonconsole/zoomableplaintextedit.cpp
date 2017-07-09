
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
#include "zoomableplaintextedit.h"

namespace appleseed {
namespace studio {

ZoomablePlainTextEdit::ZoomablePlainTextEdit(QWidget* parent)
  : QPlainTextEdit(parent)
{
}

void ZoomablePlainTextEdit::keyPressEvent(QKeyEvent* event)
{
    if (event->modifiers() & Qt::ControlModifier &&
        (event->key() == Qt::Key_Plus || event->key() == Qt::Key_Equal))
        change_font_size(1);
    else if (event->modifiers() & Qt::ControlModifier && event->key() == Qt::Key_Minus)
        change_font_size(-1);
    else if (event->modifiers() & Qt::ControlModifier && event->key() == Qt::Key_0)
        change_font_size(QFont().pointSize() - font().pointSize());
    else
        QPlainTextEdit::keyPressEvent(event);
}

void ZoomablePlainTextEdit::wheelEvent(QWheelEvent* event)
{
    if (event->modifiers() & Qt::ControlModifier)
        change_font_size(event->delta() / 120);
    else
        QPlainTextEdit::wheelEvent(event);
}

void ZoomablePlainTextEdit::change_font_size(const int delta)
{
    int new_font_size = font().pointSize() + delta;
    QFont new_font = font();
    new_font.setPointSize(new_font_size);
    setFont(new_font);
    emit(fontChanged(new_font));
}

}   // namespace studio
}   // namespace appleseed
