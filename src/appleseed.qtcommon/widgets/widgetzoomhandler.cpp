
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
#include "widgetzoomhandler.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

// Qt headers.
#include <QEvent>
#include <QKeyEvent>
#include <QLayout>
#include <QScrollArea>
#include <QScrollBar>
#include <QWheelEvent>
#include <QWidget>

// Standard headers.
#include <algorithm>

using namespace foundation;

namespace appleseed {
namespace qtcommon {

namespace
{
    const double ScaleFactorMultiplier = 1.25;

    const double MinWidgetSize = 1.0;           // in pixels
    const double MaxWidgetSize = 65536.0;       // in pixels

    const double MinScaleFactor = 1.0 / 65536.0;
    const double MaxScaleFactor = 65536.0;
}

WidgetZoomHandler::WidgetZoomHandler(
    QScrollArea*    scroll_area,
    QWidget*        content_widget)
  : m_scroll_area(scroll_area)
  , m_content_widget(content_widget)
  , m_content_width(content_widget->width())
  , m_content_height(content_widget->height())
{
    compute_min_max_scale_factors();

    m_state.m_scale_factor = 1.0;

    m_scroll_area->installEventFilter(this);
    m_scroll_area->viewport()->installEventFilter(this);
}

WidgetZoomHandler::~WidgetZoomHandler()
{
    m_scroll_area->viewport()->removeEventFilter(this);
    m_scroll_area->removeEventFilter(this);
}

WidgetZoomHandler::State WidgetZoomHandler::save_state() const
{
    return m_state;
}

void WidgetZoomHandler::load_state(const State& state)
{
    m_state = state;

    apply_scale_factor();
}

bool WidgetZoomHandler::eventFilter(QObject* object, QEvent* event)
{
    switch (event->type())
    {
      case QEvent::KeyPress:
        if (handle_key_press_event(static_cast<QKeyEvent*>(event)))
            return true;
        break;

      case QEvent::Wheel:
        if (handle_wheel_event(static_cast<QWheelEvent*>(event)))
            return true;
        break;
    }

    return QObject::eventFilter(object, event);
}

bool WidgetZoomHandler::handle_key_press_event(QKeyEvent* event)
{
    switch (event->key())
    {
      case Qt::Key_Plus:
        zoom_in();
        return true;

      case Qt::Key_Minus:
        zoom_out();
        return true;

      case Qt::Key_Equal:
        if (event->modifiers() & Qt::ShiftModifier)
        {
            zoom_in();
            return true;
        }
        break;
    }

    return false;
}

bool WidgetZoomHandler::handle_wheel_event(QWheelEvent* event)
{
    if (event->delta() > 0)
    {
        zoom_in();
        return true;
    }
    else if (event->delta() < 0)
    {
        zoom_out();
        return true;
    }

    return false;
}

void WidgetZoomHandler::zoom_in()
{
    multiply_scale_factor(ScaleFactorMultiplier);
}

void WidgetZoomHandler::zoom_out()
{
    multiply_scale_factor(1.0 / ScaleFactorMultiplier);
}

void WidgetZoomHandler::reset_zoom()
{
    m_state.m_scale_factor = 1.0;
    apply_scale_factor();
}

void WidgetZoomHandler::compute_min_max_scale_factors()
{
    m_min_scale_factor =
        std::max(
            MinScaleFactor,
            std::max(
                MinWidgetSize / m_content_width,
                MinWidgetSize / m_content_height));

    m_max_scale_factor =
        std::min(
            MaxScaleFactor,
            std::min(
                MaxWidgetSize / m_content_width,
                MaxWidgetSize / m_content_height));
}

void WidgetZoomHandler::multiply_scale_factor(const double multiplier)
{
    m_state.m_scale_factor =
        clamp(
            m_state.m_scale_factor * multiplier,
            m_min_scale_factor,
            m_max_scale_factor);

    apply_scale_factor();
}

namespace
{
    double get_slider_center(const QScrollBar* scrollbar)
    {
        return scrollbar->value() + static_cast<double>(scrollbar->pageStep()) / 2;
    }

    void set_slider_center(QScrollBar* scrollbar, const double center)
    {
        scrollbar->setValue(static_cast<int>(center - static_cast<double>(scrollbar->pageStep()) / 2));
    }
}

void WidgetZoomHandler::apply_scale_factor()
{
    const double h_slider_center = get_slider_center(m_scroll_area->horizontalScrollBar());
    const double v_slider_center = get_slider_center(m_scroll_area->verticalScrollBar());

    const int old_widget_width = m_scroll_area->widget()->width();
    const int old_widget_height = m_scroll_area->widget()->height();

    m_content_widget->setFixedSize(
        static_cast<int>(m_content_width * m_state.m_scale_factor),
        static_cast<int>(m_content_height * m_state.m_scale_factor));

    // Make sure the size of the scrollarea widget is up-to-date.
    if (m_scroll_area->widget()->layout())
        m_scroll_area->widget()->layout()->activate();

    const int new_widget_width = m_scroll_area->widget()->width();
    const int new_widget_height = m_scroll_area->widget()->height();

    const double actual_h_multiplier = static_cast<double>(new_widget_width) / old_widget_width;
    const double actual_v_multiplier = static_cast<double>(new_widget_height) / old_widget_height;

    set_slider_center(m_scroll_area->horizontalScrollBar(), actual_h_multiplier * h_slider_center);
    set_slider_center(m_scroll_area->verticalScrollBar(), actual_v_multiplier * v_slider_center);
}

}   // namespace qtcommon
}   // namespace appleseed
