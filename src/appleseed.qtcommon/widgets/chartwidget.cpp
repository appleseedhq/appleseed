
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
#include <utility>

using namespace foundation;

namespace appleseed {
namespace qtcommon {

//
// ChartBase class implementation.
//

void ChartBase::set_equidistant(const bool equidistant)
{
    m_equidistant = equidistant;
}

void ChartBase::set_origin(const Vector2d& origin)
{
    m_origin = origin;
}

void ChartBase::set_grid_brush(const QBrush& brush)
{
    m_grid_brush = brush;
}

void ChartBase::set_legend_brush(const QBrush& brush)
{
    m_legend_brush = brush;
}

void ChartBase::set_horizontal_legend_formatter(const ValueFormatter& formatter)
{
    m_horizontal_legend_formatter = formatter;
}

void ChartBase::set_vertical_legend_formatter(const ValueFormatter& formatter)
{
    m_vertical_legend_formatter = formatter;
}

void ChartBase::set_tooltip_formatter(const TooltipFormatter& formatter)
{
    m_tooltip_formatter = formatter;
}

size_t ChartBase::size() const
{
    return m_original_points.size();
}

void ChartBase::push_back(const Vector2d& p)
{
    m_original_points.push_back(p);
}

void ChartBase::push_back(const double x, const double y)
{
    m_original_points.emplace_back(x, y);
}

void ChartBase::pop_front()
{
    m_original_points.pop_front();
}

void ChartBase::prepare_drawing(QPainter& painter)
{
    m_points = m_original_points;

    if (m_equidistant)
    {
        for (size_t i = 0, e = m_points.size(); i < e; ++i)
            m_points[i].x = static_cast<double>(i);
    }

    m_points_bbox = compute_points_bbox();
    m_rcp_points_bbox_extent = m_points_bbox.is_valid() ? Vector2d(1.0) / m_points_bbox.extent() : Vector2d(1.0);   // todo: fix!

    const QRect window = painter.window();

    m_window_origin = Vector2d(window.x(), window.y());
    m_window_size = Vector2d(window.width(), window.height());
}

void ChartBase::draw_grid(QPainter& painter) const
{
    painter.save();

    QPen pen;
    pen.setBrush(m_grid_brush);
    pen.setWidthF(1.0);
    painter.setPen(pen);

    draw_horizontal_grid(painter);
    draw_vertical_grid(painter);

    painter.restore();
}

void ChartBase::draw_legends(QPainter& painter) const
{
    painter.save();

    QPen pen;
    pen.setBrush(m_legend_brush);
    painter.setPen(pen);

    if (m_horizontal_legend_formatter)
        draw_horizontal_legend(painter);

    if (m_vertical_legend_formatter)
        draw_vertical_legend(painter);

    painter.restore();
}

void ChartBase::draw_tooltip(QPainter& painter, const QPoint& mouse_position) const
{
    constexpr int ShiftX = 2;
    constexpr int ShiftY = 2;
    constexpr int MarginX = 8;
    constexpr int MarginY = 5;
    constexpr int FrameMargin = 3;
    constexpr int Opacity = 180;
    const QColor TextColor(210, 210, 210, Opacity);
    const QColor BorderColor(40, 40, 40, Opacity);
    const QColor BackgroundColor(80, 80, 80, Opacity);

    if (!m_tooltip_formatter)
        return;

    size_t point_index;
    if (!on_chart(mouse_position, point_index))
        return;

    const Vector2d& point = m_original_points[point_index];
    const QString text = m_tooltip_formatter(point);
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

    const int max_right = painter.window().right() - FrameMargin;
    if (tooltip_rect.right() > max_right)
        tooltip_rect.moveRight(max_right);

    const int max_bottom = painter.window().bottom() - FrameMargin;
    if (tooltip_rect.bottom() > max_bottom)
        tooltip_rect.moveBottom(max_bottom);

    painter.save();
    painter.setPen(BorderColor);
    painter.setBrush(BackgroundColor);
    painter.drawRect(tooltip_rect);
    painter.restore();

    painter.save();
    painter.setPen(TextColor);
    painter.drawText(tooltip_rect, Qt::AlignCenter, text);
    painter.restore();
}

void ChartBase::draw_highlight(QPainter& painter, const QPoint& mouse_position) const
{
}

AABB2d ChartBase::compute_points_bbox() const
{
    AABB2d bbox;
    bbox.invalidate();

    bbox.insert(m_origin);

    for (const Vector2d& point : m_points)
        bbox.insert(point);

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
    constexpr size_t Subdivisions = 10;

    for (size_t i = 0; i < Subdivisions; ++i)
    {
        const double y = fit<size_t, double>(i, 0, Subdivisions - 1, m_points_bbox.min.y, m_points_bbox.max.y);
        const Vector2d p = convert_to_frame(Vector2d(0.0, y));

        painter.drawLine(
            static_cast<int>(m_window_origin.x),
            static_cast<int>(p.y),
            static_cast<int>(m_window_origin.x + m_window_size.x),
            static_cast<int>(p.y));
    }
}

void ChartBase::draw_vertical_grid(QPainter& painter) const
{
    if (m_points.size() > 2)
    {
        for (const Vector2d& point : m_points)
        {
            const Vector2d p = convert_to_frame(point);

            painter.drawLine(
                static_cast<int>(p.x),
                static_cast<int>(m_window_origin.y),
                static_cast<int>(p.x),
                static_cast<int>(m_window_origin.y + m_window_size.y));
        }
    }
}

void ChartBase::draw_horizontal_legend(QPainter& painter) const
{
    constexpr size_t TextMargin = 3;    // pixels

    assert(m_horizontal_legend_formatter);

    if (m_points.size() > 2)
    {
        for (const Vector2d& point : m_points)
        {
            const double x = point.x;
            const Vector2d p = convert_to_frame(point);
            const QString text = m_horizontal_legend_formatter(x);

            painter.drawText(
                static_cast<int>(p.x) + TextMargin,
                static_cast<int>(m_window_origin.y + m_window_size.y) - TextMargin,
                text);
        }
    }
}

void ChartBase::draw_vertical_legend(QPainter& painter) const
{
    constexpr size_t Subdivisions = 10;
    constexpr size_t TextMargin = 3;    // pixels

    assert(m_vertical_legend_formatter);

    for (size_t i = 0; i < Subdivisions; ++i)
    {
        const double y = fit<size_t, double>(i, 0, Subdivisions - 1, m_points_bbox.min.y, m_points_bbox.max.y);
        const Vector2d p = convert_to_frame(Vector2d(0.0, y));
        const QString text = m_vertical_legend_formatter(y);

        painter.drawText(
            static_cast<int>(m_window_origin.x) + TextMargin,
            static_cast<int>(p.y) - TextMargin,
            text);
    }
}


//
// LineChart class implementation.
//

void LineChart::set_shadow_enabled(const bool enabled)
{
    m_shadow_enabled = enabled;
}

void LineChart::set_curve_brush(const QBrush& brush)
{
    m_curve_brush = brush;
}

void LineChart::set_shadow_brush(const QBrush& brush)
{
    m_shadow_brush = brush;
}

void LineChart::set_draw_points(const bool draw_points)
{
    m_draw_points = draw_points;
}

void LineChart::draw_chart(QPainter& painter) const
{
    const QPoint ShadowOffset(-2, 3);   // pixels
    constexpr qreal LineWidth = 2.0;    // pixels

    if (m_points.size() > 1)
    {
        QPen pen;
        pen.setWidthF(LineWidth);

        if (m_shadow_enabled)
        {
            painter.save();
            pen.setBrush(m_shadow_brush);
            painter.setPen(pen);
            painter.setBrush(m_shadow_brush);
            draw_lines(painter, ShadowOffset);
            painter.restore();
        }

        painter.save();
        pen.setBrush(m_curve_brush);
        painter.setPen(pen);
        painter.setBrush(m_curve_brush);
        draw_lines(painter);
        painter.restore();
    }
}

void LineChart::draw_lines(QPainter& painter, const QPoint& shadow_offset) const
{
    constexpr qreal DiskRadius = 4.0;

    assert(m_points.size() > 1);

    for (size_t i = 0, e = m_points.size() - 1; i < e; ++i)
    {
        const Vector2d from = convert_to_frame(m_points[i]);
        const Vector2d to = convert_to_frame(m_points[i + 1]);

        painter.drawLine(
            static_cast<int>(from.x) + shadow_offset.x(),
            static_cast<int>(from.y) + shadow_offset.y(),
            static_cast<int>(to.x) + shadow_offset.x(),
            static_cast<int>(to.y) + shadow_offset.y());
    }

    if (m_draw_points)
    {
        for (const Vector2d& point : m_points)
        {
            const Vector2d p = convert_to_frame(point);
            painter.drawEllipse(QPointF(p.x, p.y), DiskRadius, DiskRadius);
        }
    }
}

bool LineChart::on_chart(const QPoint& mouse_position, size_t& point_index) const
{
    constexpr double MinDistance = 20.0;

    const Vector2d mp(mouse_position.x(), mouse_position.y());

    double closest_square_distance = std::numeric_limits<double>::max();
    size_t closest_index = 0;

    for (size_t i = 0, e = m_points.size(); i < e; ++i)
    {
        const Vector2d p = convert_to_frame(m_points[i]);
        const double d = square_distance(mp, p);

        if (closest_square_distance > d)
        {
            closest_square_distance = d;
            closest_index = i;
        }
    }

    if (closest_square_distance < square(MinDistance))
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
    const QColor DiskColor(190, 140, 50);
    constexpr qreal DiskRadius = 6.0;

    size_t point_index;
    if (on_chart(mouse_position, point_index))
    {
        const Vector2d p = convert_to_frame(m_points[point_index]);

        painter.save();
        painter.setPen(DiskColor);
        painter.setBrush(DiskColor);
        painter.drawEllipse(QPointF(p.x, p.y), DiskRadius, DiskRadius);
        painter.restore();
    }
}

void LineChart::draw_cross(QPainter& painter, const QPoint& mouse_position) const
{
    const QColor CrossColor(100, 100, 100);

    painter.save();

    QPen pen;
    pen.setStyle(Qt::DotLine);
    pen.setColor(CrossColor);
    pen.setWidthF(0.5);
    painter.setPen(pen);

    painter.drawLine(
        static_cast<int>(m_window_origin.x),
        static_cast<int>(mouse_position.y()),
        static_cast<int>(m_window_origin.x + m_window_size.x),
        static_cast<int>(mouse_position.y()));

    painter.drawLine(
        static_cast<int>(mouse_position.x()),
        static_cast<int>(m_window_origin.y),
        static_cast<int>(mouse_position.x()),
        static_cast<int>(m_window_origin.y + m_window_size.y));

    painter.restore();
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

void ChartWidget::clear()
{
    m_charts.clear();
}

ChartBase* ChartWidget::add_chart(std::unique_ptr<ChartBase> chart)
{
    assert(chart);
    m_charts.emplace_back(std::move(chart));
    return m_charts.back().get();
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
    for (const std::unique_ptr<ChartBase>& chart : m_charts)
        chart->prepare_drawing(painter);

    for (const std::unique_ptr<ChartBase>& chart : m_charts)
        chart->draw_grid(painter);

    for (const std::unique_ptr<ChartBase>& chart : m_charts)
        chart->draw_legends(painter);

    for (const std::unique_ptr<ChartBase>& chart : m_charts)
        chart->draw_chart(painter);
}

void ChartWidget::draw_highlights(QPainter& painter) const
{
    for (const std::unique_ptr<ChartBase>& chart : m_charts)
        chart->draw_highlight(painter, m_mouse_position);
}

void ChartWidget::draw_tooltips(QPainter& painter) const
{
    for (const std::unique_ptr<ChartBase>& chart : m_charts)
        chart->draw_tooltip(painter, m_mouse_position);
}

void ChartWidget::draw_frame(QPainter& painter) const
{
    QStyleOptionFrame option;
    option.initFrom(this);
    style()->drawPrimitive(QStyle::PE_Frame, &option, &painter, this);
}

}   // namespace qtcommon
}   // namespace appleseed
