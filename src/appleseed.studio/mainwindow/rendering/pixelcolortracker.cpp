
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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
#include "pixelcolortracker.h"

// appleseed.qtcommon headers.
#include "widgets/mousecoordinatestracker.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/vector.h"

// Qt headers.
#include <QEvent>
#include <QLabel>
#include <QMouseEvent>
#include <QString>
#include <QWidget>

// Standard headers.
#include <cstddef>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

PixelColorTracker::PixelColorTracker(
    QWidget*                        widget,
    QLabel*                         r_label,
    QLabel*                         g_label,
    QLabel*                         b_label,
    QLabel*                         a_label,
    const MouseCoordinatesTracker&  mouse_tracker,
    const Project&                  project)
  : m_widget(widget)
  , m_r_label(r_label)
  , m_g_label(g_label)
  , m_b_label(b_label)
  , m_a_label(a_label)
  , m_mouse_tracker(mouse_tracker)
  , m_project(project)
  , m_enabled(true)
{
    m_widget->installEventFilter(this);
}

PixelColorTracker::~PixelColorTracker()
{
    m_widget->removeEventFilter(this);
}

void PixelColorTracker::set_enabled(const bool enabled)
{
    m_enabled = enabled;
}

bool PixelColorTracker::eventFilter(QObject* object, QEvent* event)
{
    if (m_enabled)
    {
        switch (event->type())
        {
          case QEvent::MouseMove:
            set_rgba_labels(static_cast<QMouseEvent*>(event)->pos());
            break;

          case QEvent::Leave:
            clear_rgba_labels();
            break;
        }
    }

    return QObject::eventFilter(object, event);
}

void PixelColorTracker::clear_rgba_labels() const
{
    m_r_label->clear();
    m_g_label->clear();
    m_b_label->clear();
    m_a_label->clear();
}

void PixelColorTracker::set_rgba_labels(const QPoint& point) const
{
    const Image& image = m_project.get_frame()->image();
    const CanvasProperties& props = image.properties();
    const Vector2i pixel = m_mouse_tracker.widget_to_pixel(point);

    if (pixel.x >= 0 &&
        pixel.y >= 0 &&
        pixel.x < static_cast<int>(props.m_canvas_width) &&
        pixel.y < static_cast<int>(props.m_canvas_height))
    {
        Color4f linear_rgba;
        image.get_pixel(
            static_cast<size_t>(pixel.x),
            static_cast<size_t>(pixel.y),
            linear_rgba);

        m_r_label->setText(QString::number(linear_rgba.r, 'f', 3));
        m_g_label->setText(QString::number(linear_rgba.g, 'f', 3));
        m_b_label->setText(QString::number(linear_rgba.b, 'f', 3));
        m_a_label->setText(QString::number(linear_rgba.a, 'f', 3));
    }
    else
    {
        clear_rgba_labels();
    }
}

}   // namespace studio
}   // namespace appleseed
