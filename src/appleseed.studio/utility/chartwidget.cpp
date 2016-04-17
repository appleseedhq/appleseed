
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "chartwidget.h"

// appleseed.foundation headers.
#include "foundation/math/distance.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/foreach.h"

// Qt headers.
#include <QColor>
#include <QMouseEvent>
#include <QPainter>
#include <QPen>
#include <QRect>
#include <QStyle>
#include <QStyleOptionFrame>
#include <Qt>

// Standard headers.
#include <cassert>
#include <limits>

using namespace foundation;
using namespace std;

namespace appleseed {
namespace studio {

//
// ChartBase class implementation.
//

ChartBase::ChartBase()
  : m_equidistant(false)
  , m_margin(15.0, 15.0)
  , m_grid_brush(QBrush(QColor(50, 50, 50, 255)))
{
}

void ChartBase::set_equidistant(const bool equidistant)
{
    m_equidistant = equidistant;
}

void ChartBase::set_grid_brush(const QBrush& brush)
{
    m_grid_brush = brush;
}

void ChartBase::set_tooltip_formatter(auto_ptr<IToolTipFormatter> formatter)
{
    m_tooltip_formatter = formatter;
}

void ChartBase::add_point(const Vector2d& p)
{
    m_original_points.push_back(p);
}

void ChartBase::add_point(const double x, const double y)
{
    m_original_points.push_back(Vector2d(x, y));
}

void ChartBase::prepare_drawing(QPainter& painter)
{
    m_points = m_original_points;

    if (m_equidistant)
    {
        for (size_t i = 0; i < m_points.size(); ++i)
            m_points[i].x = static_cast<double>(i);

    }

    m_points_bbox = compute_points_bbox();
    m_rcp_points_bbox_extent = Vector2d(1.0) / m_points_bbox.extent();

    const QRect window = painter.window();

    m_window_origin = Vector2d(window.x(), window.y());
    m_window_size = Vector2d(window.width(), window.height());
}

void ChartBase::draw_grid(QPainter& painter) const
{
    QPen pen;
    pen.setBrush(m_grid_brush);
    pen.setWidthF(1.0);
    painter.setPen(pen);

    draw_horizontal_grid(painter);
    draw_vertical_grid(painter);
}

void ChartBase::draw_tooltip(QPainter& painter, const QPoint& mouse_position) const
{
    size_t point_index;

    if (!on_chart(mouse_position, point_index))
        return;

    const int ShiftX = 2;
    const int ShiftY = 2;
    const int MarginX = 8;
    const int MarginY = 5;
    const int FrameMargin = 3;
    const qreal CornerRadius = 2.0;
    const int Opacity = 180;
    const QColor TextColor(210, 210, 210, Opacity);
    const QColor BorderColor(40, 40, 40, Opacity);
    const QColor BackgroundColor(80, 80, 80, Opacity);

    if (m_tooltip_formatter.get() == 0)
        return;

    const Vector2d& point = m_original_points[point_index];
    const QString text = m_tooltip_formatter->format(point);
    const QRect text_rect = painter.fontMetrics().boundingRect(QRect(), Qt::AlignCenter, text);

    QRect tooltip_rect(
        mouse_position.x() + ShiftX,
        mouse_position.y() - text_rect.height() - 2 * MarginY - ShiftY,
        text_rect.width() + 2 * MarginX,
        text_rect.height() + 2 * MarginY);

    if (tooltip_rect.left() < FrameMargin)
        tooltip_rect.moveLeft(FrameMargin);

    if (tooltip_rect.top() < FrameMargin)
        tooltip_rect.moveTop(FrameMargin);

    const int MaxRight = painter.window().right() - FrameMargin;
    if (tooltip_rect.right() > MaxRight)
        tooltip_rect.moveRight(MaxRight);

    const int MaxBottom = painter.window().bottom() - FrameMargin;
    if (tooltip_rect.bottom() > MaxBottom)
        tooltip_rect.moveBottom(MaxBottom);

    painter.setPen(BorderColor);
    painter.setBrush(QBrush(BackgroundColor));
    painter.drawRoundedRect(tooltip_rect, CornerRadius, CornerRadius);

    painter.setPen(TextColor);
    painter.drawText(tooltip_rect, Qt::AlignCenter, text);
}

void ChartBase::draw_highlight(QPainter& painter, const QPoint& mouse_position) const
{
}

AABB2d ChartBase::compute_points_bbox() const
{
    AABB2d bbox;
    bbox.invalidate();

    bbox.insert(Vector2d(0.0));

    for (size_t i = 0; i < m_points.size(); ++i)
        bbox.insert(m_points[i]);

    return bbox;
}

Vector2d ChartBase::convert_to_frame(const Vector2d& point) const
{
    const Vector2d window_size = m_window_size - 2.0 * m_margin;
    const Vector2d window_origin = m_window_origin + m_margin;

    Vector2d u = (point - m_points_bbox.min) * m_rcp_points_bbox_extent;

    u.y = 1.0 - u.y;

    return window_origin + u * window_size;
}

Vector2d ChartBase::convert_to_data(const Vector2d& point) const
{
    const Vector2d min = m_window_origin + m_margin;
    const Vector2d max = m_window_origin + m_window_size - m_margin - Vector2d(1.0);

    Vector2d p = point;

    if (p.x < min.x) p.x = min.x;
    if (p.x > max.x) p.x = max.x;
    if (p.y < min.y) p.y = min.y;
    if (p.y > max.y) p.y = max.y;

    Vector2d u = (p - min) / (max - min);

    u.y = 1.0 - u.y;

    return u * m_points_bbox.extent() + m_points_bbox.min;
}

void ChartBase::draw_horizontal_grid(QPainter& painter) const
{
    const size_t Subdivisions = 10;

    for (size_t i = 0; i < Subdivisions; ++i)
    {
        const double k = static_cast<double>(i) / (Subdivisions - 1);
        const double y = mix(m_points_bbox.min.y, m_points_bbox.max.y, k);

        const Vector2d p = convert_to_frame(Vector2d(0.0, y));

        painter.drawLine(
            m_window_origin.x, p.y,
            m_window_origin.x + m_window_size.x, p.y);
    }
}

void ChartBase::draw_vertical_grid(QPainter& painter) const
{
    if (m_points.size() > 2)
    {
        for (size_t i = 0; i < m_points.size(); ++i)
        {
            const Vector2d p = convert_to_frame(m_points[i]);

            painter.drawLine(
                p.x, m_window_origin.y,
                p.x, m_window_origin.y + m_window_size.y);
        }
    }
}


//
// LineChart class implementation.
//

LineChart::LineChart()
  : m_curve_brush(QBrush(QColor(255, 255, 255, 255)))
{
}

void LineChart::set_curve_brush(const QBrush& brush)
{
    m_curve_brush = brush;
}

void LineChart::draw_chart(QPainter& painter) const
{
    if (m_points.size() > 1)
    {
        QPen pen;
        pen.setBrush(m_curve_brush);
        pen.setWidthF(2.0);
        painter.setPen(pen);

        for (size_t i = 0; i < m_points.size() - 1; ++i)
        {
            const Vector2d from = convert_to_frame(m_points[i]);
            const Vector2d to = convert_to_frame(m_points[i + 1]);
            painter.drawLine(from.x, from.y, to.x, to.y);
        }
    }
}

bool LineChart::on_chart(const QPoint& mouse_position, size_t& point_index) const
{
    const double MinDistance = 8.0;

    const Vector2d mp(mouse_position.x(), mouse_position.y());

    double closest_square_distance = numeric_limits<double>::max();
    size_t closest_index = 0;

    for (size_t i = 0; i < m_points.size(); ++i)
    {
        const Vector2d p = convert_to_frame(m_points[i]);
        const double d = square_distance(mp, p);

        if (closest_square_distance > d)
        {
            closest_square_distance = d;
            closest_index = i;
        }
    }

    if (closest_square_distance < MinDistance * MinDistance)
    {
        point_index = closest_index;
        return true;
    }

    return false;
}

void LineChart::draw_highlight(QPainter& painter, const QPoint& mouse_position) const
{
    draw_point_highlight(painter, mouse_position);
    draw_cross(painter, mouse_position);
}

void LineChart::draw_point_highlight(QPainter& painter, const QPoint& mouse_position) const
{
    size_t point_index;

    if (on_chart(mouse_position, point_index))
    {
        const QColor DiskColor(140, 200, 255);
        const qreal DiskRadius = 3.0;

        const Vector2d p = convert_to_frame(m_points[point_index]);

        painter.setPen(DiskColor);
        painter.setBrush(QBrush(DiskColor));
        painter.drawEllipse(QPointF(p.x, p.y), DiskRadius, DiskRadius);
    }
}

void LineChart::draw_cross(QPainter& painter, const QPoint& mouse_position) const
{
    const QColor CrossColor(100, 100, 100);

    QPen pen;
    pen.setStyle(Qt::DotLine);
    pen.setColor(CrossColor);
    pen.setWidthF(0.5);
    painter.setPen(pen);

    painter.drawLine(
        m_window_origin.x, mouse_position.y(),
        m_window_origin.x + m_window_size.x, mouse_position.y());

    painter.drawLine(
        mouse_position.x(), m_window_origin.y,
        mouse_position.x(), m_window_origin.y + m_window_size.y);
}


//
// ChartWidget class implementation.
//

ChartWidget::ChartWidget(QWidget* parent)
  : QFrame(parent)
  , m_mouse_inside_widget(false)
{
    setMouseTracking(true);
}

ChartWidget::~ChartWidget()
{
    clear();
}

void ChartWidget::clear()
{
    for (const_each<ChartCollection> i = m_charts; i; ++i)
        delete *i;

    m_charts.clear();
}

void ChartWidget::add_chart(auto_ptr<ChartBase> chart)
{
    assert(chart.get());

    m_charts.push_back(chart.release());
}

void ChartWidget::mouseMoveEvent(QMouseEvent* event)
{
    m_mouse_inside_widget = true;
    m_mouse_position = event->pos();

    update();
}

void ChartWidget::leaveEvent(QEvent* event)
{
    m_mouse_inside_widget = false;

    update();
}

void ChartWidget::paintEvent(QPaintEvent* event)
{
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing, true);

    draw_charts(painter);

    if (m_mouse_inside_widget)
    {
        draw_highlights(painter);
        draw_tooltips(painter);
    }

    draw_frame(painter);
}

void ChartWidget::draw_charts(QPainter& painter) const
{
    for (const_each<ChartCollection> i = m_charts; i; ++i)
    {
        ChartBase* chart = *i;
        chart->prepare_drawing(painter);
        chart->draw_grid(painter);
    }

    for (const_each<ChartCollection> i = m_charts; i; ++i)
    {
        const ChartBase* chart = *i;
        chart->draw_chart(painter);
    }
}

void ChartWidget::draw_highlights(QPainter& painter) const
{
    for (const_each<ChartCollection> i = m_charts; i; ++i)
    {
        const ChartBase* chart = *i;
        chart->draw_highlight(painter, m_mouse_position);
    }
}

void ChartWidget::draw_tooltips(QPainter& painter) const
{
    for (const_each<ChartCollection> i = m_charts; i; ++i)
    {
        const ChartBase* chart = *i;
        chart->draw_tooltip(painter, m_mouse_position);
    }
}

void ChartWidget::draw_frame(QPainter& painter) const
{
    QStyleOptionFrame option;
    option.initFrom(this);
    style()->drawPrimitive(QStyle::PE_Frame, &option, &painter, this);
}

}   // namespace studio
}   // namespace appleseed
