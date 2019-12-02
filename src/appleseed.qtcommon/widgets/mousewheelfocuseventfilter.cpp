
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
#include "mousewheelfocuseventfilter.h"

// Qt headers.
#include <QEvent>
#include <QWidget>

namespace appleseed {
namespace qtcommon {

MouseWheelFocusEventFilter::MouseWheelFocusEventFilter(QWidget* parent)
  : QObject(parent)
  , m_old_focus_policy(parent->focusPolicy())
{
    parent->installEventFilter(this);
    parent->setFocusPolicy(Qt::StrongFocus);
}

MouseWheelFocusEventFilter::~MouseWheelFocusEventFilter()
{
    QWidget* parent = qobject_cast<QWidget*>(QObject::parent());

    parent->setFocusPolicy(m_old_focus_policy);
    parent->removeEventFilter(this);
}

bool MouseWheelFocusEventFilter::eventFilter(QObject* object, QEvent* event)
{
    QWidget* parent = qobject_cast<QWidget*>(object);

    switch (event->type())
    {
      case QEvent::FocusIn:
        parent->setFocusPolicy(Qt::WheelFocus);
        event->accept();
        return false;

      case QEvent::FocusOut:
        parent->setFocusPolicy(Qt::StrongFocus);
        event->accept();
        return false;

      case QEvent::Wheel:
        if (parent->focusPolicy() == Qt::WheelFocus)
        {
            event->accept();
            return false;
        }
        else
        {
            event->ignore();
            return true;
        }

      default:
        return QObject::eventFilter(object, event);
    }
}

}   // namespace qtcommon
}   // namespace appleseed
