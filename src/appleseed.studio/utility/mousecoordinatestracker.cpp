
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "mousecoordinatestracker.h"

// Qt headers.
#include <QEvent>
#include <QLabel>
#include <QMouseEvent>
#include <QPoint>
#include <QString>
#include <QWidget>

// Standard headers.
#include <cassert>

using namespace std;

namespace appleseed {
namespace studio {

MouseCoordinatesTracker::MouseCoordinatesTracker(
    QWidget*    widget,
    QLabel*     label,
    const int   content_width,
    const int   content_height)
  : m_widget(widget)
  , m_label(label)
  , m_content_width(content_width)
  , m_content_height(content_height)
  , m_was_mouse_tracked(widget->hasMouseTracking())
{
    m_widget->setMouseTracking(true);
    m_widget->installEventFilter(this);
}

MouseCoordinatesTracker::~MouseCoordinatesTracker()
{
    m_widget->removeEventFilter(this);
    m_widget->setMouseTracking(m_was_mouse_tracked);
}

bool MouseCoordinatesTracker::eventFilter(QObject* object, QEvent* event)
{
    assert(object == m_widget);

    switch (event->type())
    {
      case QEvent::MouseMove:
        set_label_text(static_cast<QMouseEvent*>(event)->pos());
        break;

      case QEvent::Leave:
        m_label->clear();
        break;
    }

    return QObject::eventFilter(object, event);
}

void MouseCoordinatesTracker::set_label_text(const QPoint& point) const
{
    const double ndc_x = static_cast<double>(point.x()) / m_widget->width();
    const double ndc_y = static_cast<double>(point.y()) / m_widget->height();

    const int pix_x = static_cast<int>(ndc_x * m_content_width);
    const int pix_y = static_cast<int>(ndc_y * m_content_height);

    m_label->setText(
        QString("Pixel: %1, %2  -  NDC: %3, %4")
            .arg(QString::number(pix_x))
            .arg(QString::number(pix_y))
            .arg(QString::number(ndc_x))
            .arg(QString::number(ndc_y)));
}

}   // namespace studio
}   // namespace appleseed
