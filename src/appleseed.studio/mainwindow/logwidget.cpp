
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include <QContextMenuEvent>
#include <QMenu>

namespace appleseed {
namespace studio {

//
// LogWidget class implementation.
//

LogWidget::LogWidget(QWidget* parent)
  : QTextEdit(parent)
{
}

void LogWidget::slot_append_item(const QColor& color, const QString& text)
{
    setTextColor(color);
    append(text);
}

void LogWidget::slot_clear_all()
{
    clear();
}

void LogWidget::contextMenuEvent(QContextMenuEvent* event)
{
    QMenu* context_menu = createStandardContextMenu();

    QAction* action_clear_all = context_menu->addAction(tr("Clear All"));
    action_clear_all->setEnabled(!document()->isEmpty());
    connect(action_clear_all, SIGNAL(triggered()), this, SLOT(slot_clear_all()));

    context_menu->exec(event->globalPos());

    delete context_menu;
}

}   // namespace studio
}   // namespace appleseed
