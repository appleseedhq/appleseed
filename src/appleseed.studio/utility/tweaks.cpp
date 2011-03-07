
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "tweaks.h"

// Qt headers.
#include <QGridLayout>
#include <QKeySequence>
#include <QMessageBox>
#include <QShortcut>
#include <QSpacerItem>
#include <QWidget>

namespace appleseed {
namespace studio {

void disable_mac_focus_rect(QWidget& widget)
{
    widget.setAttribute(Qt::WA_MacShowFocusRect, false);
}

void set_minimum_width(QMessageBox& msgbox, const int minimum_width)
{
    QSpacerItem* spacer =
        new QSpacerItem(
            minimum_width,
            0,
            QSizePolicy::Minimum,
            QSizePolicy::Expanding);

    QGridLayout* layout = static_cast<QGridLayout*>(msgbox.layout());

    layout->addItem(
        spacer,
        layout->rowCount(),         // row
        0,                          // column
        1,                          // row span
        layout->columnCount());     // column span
}

QShortcut* create_window_local_shortcut(QWidget* parent, const Qt::Key key)
{
    return
        new QShortcut(
            QKeySequence(key),
            parent,
            0,
            0,
            Qt::WidgetWithChildrenShortcut);
}

}   // namespace studio
}   // namespace appleseed
