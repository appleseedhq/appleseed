
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
#include "renderregionhandler.h"

// appleseed.qtcommon headers.
#include "widgets/mousecoordinatestracker.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Qt headers.
#include <QEvent>
#include <QMouseEvent>
#include <QRect>
#include <QRubberBand>
#include <QSize>
#include <Qt>
#include <QWidget>

// Standard headers.
#include <algorithm>

using namespace appleseed::qtcommon;
using namespace foundation;

namespace appleseed {
namespace studio {

RenderRegionHandler::RenderRegionHandler(
    QWidget*                        widget,
    const MouseCoordinatesTracker&  mouse_tracker)
  : m_widget(widget)
  , m_mouse_tracker(mouse_tracker)
  , m_enabled(true)
  , m_mode(RectangleSelectionMode)
  , m_rubber_band(new QRubberBand(QRubberBand::Rectangle, widget))
{
    m_widget->installEventFilter(this);
}

RenderRegionHandler::~RenderRegionHandler()
{
    m_widget->removeEventFilter(this);
}

void RenderRegionHandler::set_enabled(const bool enabled)
{
    m_enabled = enabled;
}

void RenderRegionHandler::set_mode(const Mode mode)
{
    m_mode = mode;
}

bool RenderRegionHandler::eventFilter(QObject* object, QEvent* event)
{
    if (m_enabled)
    {
        switch (event->type())
        {
          case QEvent::MouseButtonPress:
            {
                const QMouseEvent* mouse_event = static_cast<QMouseEvent*>(event);

                if (mouse_event->button() == Qt::LeftButton &&
                    !(mouse_event->modifiers() & (Qt::AltModifier | Qt::ShiftModifier | Qt::ControlModifier)))
                {
                    m_origin = mouse_event->pos();
                    m_rubber_band->setGeometry(QRect(m_origin, QSize()));
                    m_rubber_band->show();
                }

                break;
            }

          case QEvent::MouseMove:
            {
                const QMouseEvent* mouse_event = static_cast<QMouseEvent*>(event);
                m_rubber_band->setGeometry(QRect(m_origin, mouse_event->pos()).normalized());
            }

          case QEvent::MouseButtonRelease:
            {
                const QMouseEvent* mouse_event = static_cast<QMouseEvent*>(event);

                if (mouse_event->button() == Qt::LeftButton)
                {
                    m_rubber_band->hide();

                    const Vector2i p0 = m_mouse_tracker.widget_to_pixel(m_origin);
                    const Vector2i p1 = m_mouse_tracker.widget_to_pixel(mouse_event->pos());

                    const int x0 = std::max(std::min(p0.x, p1.x), 0);
                    const int y0 = std::max(std::min(p0.y, p1.y), 0);
                    const int x1 = std::max(p0.x, p1.x);
                    const int y1 = std::max(p0.y, p1.y);
                    const int w = x1 - x0 + 1;
                    const int h = y1 - y0 + 1;

                    if (m_mode == RectangleSelectionMode)
                        emit signal_rectangle_selection(QRect(x0, y0, w, h));
                    else emit signal_render_region(QRect(x0, y0, w, h));
                }

                break;
            }
        }
    }

    return QObject::eventFilter(object, event);
}

}   // namespace studio
}   // namespace appleseed
