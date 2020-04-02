
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Freddy Chaleur, The appleseedhq Organization
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
#include "pixelinspectorhandler.h"

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
#include <QCursor>
#include <QEvent>
#include <QPoint>
#include <QString>
#include <QToolTip>
#include <QWidget>

// Standard headers.
#include <cstddef>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

PixelInspectorHandler::PixelInspectorHandler(
    QWidget*                        widget,
    const MouseCoordinatesTracker&  mouse_tracker,
    const Project&                  project)
  : m_widget(widget)
  , m_mouse_tracker(mouse_tracker)
  , m_project(project)
  , m_enabled(true)
{
    m_widget->installEventFilter(this);
}

PixelInspectorHandler::~PixelInspectorHandler()
{
    m_widget->removeEventFilter(this);
}

void PixelInspectorHandler::set_enabled(const bool enabled)
{
    m_enabled = enabled;
}

void PixelInspectorHandler::update_tooltip_visibility()
{
    m_enabled ? show_tooltip() : QToolTip::hideText();
}

bool PixelInspectorHandler::eventFilter(QObject* object, QEvent* event)
{
    if (m_enabled)
    {
        if (event->type() == QEvent::MouseMove)
            show_tooltip();
    }

    return QObject::eventFilter(object, event);
}

void PixelInspectorHandler::show_tooltip()
{
    const QPoint global_point = QCursor::pos();
    const QPoint local_point = m_widget->mapFromGlobal(global_point);

    const Vector2i pix(m_mouse_tracker.widget_to_pixel(local_point));
    const Vector2d ndc(m_mouse_tracker.widget_to_ndc(local_point));

    const Image& image = m_project.get_frame()->image();
    const CanvasProperties& props = image.properties();

    if (pix.x < 0 ||
        pix.y < 0 ||
        pix.x >= static_cast<int>(props.m_canvas_width) ||
        pix.y >= static_cast<int>(props.m_canvas_height))
    {
        QToolTip::hideText();

        return;
    }

    Color4f linear_rgba;
    image.get_pixel(
        static_cast<size_t>(pix.x),
        static_cast<size_t>(pix.y),
        linear_rgba);

    QToolTip::showText(
        global_point,
        QString(
            "Pixel:\n"
            "  X: %1\n"
            "  Y: %2\n\n"
            "NDC:\n"
            "  X: %3\n"
            "  Y: %4\n\n"
            "Color:\n"
            "  R: %5\n"
            "  G: %6\n"
            "  B: %7\n"
            "  A: %8")
        .arg(
            QString::number(pix.x),
            QString::number(pix.y),
            QString::number(ndc.x, 'f', 5),
            QString::number(ndc.y, 'f', 5),
            QString::number(linear_rgba.r, 'f', 3),
            QString::number(linear_rgba.g, 'f', 3),
            QString::number(linear_rgba.b, 'f', 3),
            QString::number(linear_rgba.a, 'f', 3)));
}

}   // namespace studio
}   // namespace appleseed
