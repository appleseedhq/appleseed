
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

void ChartBase::set_auto_horizontal_range()
{
    m_auto_horizontal_range = true;
    m_horizontal_min = 0.0;
    m_horizontal_max = 1.0;
}

void ChartBase::set_auto_vertical_range()
{
    m_auto_vertical_range = true;
    m_vertical_min = 0.0;
    m_vertical_max = 1.0;
}

void ChartBase::set_horizontal_range(const double min, const double max)
{
    m_auto_horizontal_range = false;
    m_horizontal_min = min;
    m_horizontal_max = max;
}

void ChartBase::set_vertical_range(const double min, const double max)
{
    m_auto_vertical_range = false;
    m_vertical_min = min;
    m_vertical_max = max;
}

void ChartBase::set_vertical_subdivisions(const int subdivisions)
{
    m_vertical_subdivisions = subdivisions;
}

void ChartBase::set_horizontal_window_margins(const double left, const double right)
{
    m_window_left_margin = left;
    m_window_right_margin = right;
}

void ChartBase::set_vertical_window_margins(const double bottom, const double top)
{
    m_window_bottom_margin = bottom;
    m_window_top_margin = top;
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

void ChartBase::add_point(const Vector2d& p, const QVariant& data)
{
    m_points.push_back(p);
    m_data.push_back(data);
}

void ChartBase::add_point(const double x, const double y, const QVariant& data)
{
    m_points.emplace_back(x, y);
    m_data.push_back(data);
}

void ChartBase::prepare_drawing(QPainter& painter)
{
    if (m_auto_horizontal_range || m_auto_vertical_range)
    {
        const AABB2d points_bbox = compute_points_bbox();
        const Vector2d points_extent = points_bbox.extent();

        if (m_auto_horizontal_range)
        {
            if (points_extent.x > 0.0)
            {
                m_horizontal_min = points_bbox.min.x;
                m_horizontal_max = points_bbox.max.x;
            }
            else
            {
                m_horizontal_min = 0.0;
                m_horizontal_max = 1.0;
            }
        }

        if (m_auto_vertical_range)
        {
            if (points_extent.y > 0.0)
            {
                m_vertical_min = points_bbox.min.y;
                m_vertical_max = points_bbox.max.y;
            }
            else
            {
                m_vertical_min = 0.0;
                m_vertical_max = 1.0;
            }
        }
    }

    m_rcp_horizontal_extent =
        m_horizontal_max != m_horizontal_min
            ? 1.0 / (m_horizontal_max - m_horizontal_min)
            : 1.0;

    m_rcp_vertical_extent =
        m_vertical_max != m_vertical_min
            ? 1.0 / (m_vertical_max - m_vertical_min)
            : 1.0;

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
    constexpr int Opacity = 255;
    const QColor TextColor(210, 210, 210, Opacity);
    const QColor BorderColor(40, 40, 40, Opacity);
    const QColor BackgroundColor(80, 80, 80, Opacity);

    if (!m_tooltip_formatter)
        return;

    std::size_t point_index;
    if (!on_chart(mouse_position, point_index))
        return;

    assert(m_points.size() == m_data.size());

    const Vector2d& point = m_points[point_index];
    const QVariant& data = m_data[point_index];

    const QString text = m_tooltip_formatter(point, data);
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

    for (const Vector2d& point : m_points)
        bbox.insert(point);

    return bbox;
}

Vector2d ChartBase::convert_to_frame(const Vector2d& point) const
{
    const Vector2d window_min = m_window_origin + Vector2d(m_window_left_margin, m_window_top_margin);
    const Vector2d window_max = m_window_origin + m_window_size - Vector2d(m_window_right_margin, m_window_bottom_margin);
    const Vector2d window_extent = window_max - window_min;

    Vector2d u(
        (point.x - m_horizontal_min) * m_rcp_horizontal_extent,
        (point.y - m_vertical_min) * m_rcp_vertical_extent);

    u.y = 1.0 - u.y;

    return window_min + u * window_extent;
}

Vector2d ChartBase::convert_to_data(const Vector2d& point) const
{
    const Vector2d window_min = m_window_origin + Vector2d(m_window_left_margin, m_window_top_margin);
    const Vector2d window_max = m_window_origin + m_window_size - Vector2d(m_window_right_margin, m_window_bottom_margin);
    const Vector2d window_extent = window_max - window_min;

    Vector2d p = point;

    if (p.x < window_min.x) p.x = window_min.x;
    if (p.x > window_max.x) p.x = window_max.x;
    if (p.y < window_min.y) p.y = window_min.y;
    if (p.y > window_max.y) p.y = window_max.y;

    Vector2d u = (p - window_min) / window_extent;

    u.y = 1.0 - u.y;

    return
        Vector2d(
            m_horizontal_min + u.x * (m_horizontal_max - m_horizontal_min),
            m_vertical_min + u.y * (m_vertical_max - m_vertical_min));
}

void ChartBase::draw_horizontal_grid(QPainter& painter) const
{
    for (int i = 0; i < m_vertical_subdivisions; ++i)
    {
        const double y = fit(i, 0, m_vertical_subdivisions - 1, m_vertical_min, m_vertical_max);
        const Vector2d p = convert_to_frame(Vector2d(0.0, y));

        painter.drawLine(
            QPointF(m_window_origin.x, p.y),
            QPointF(m_window_origin.x + m_window_size.x, p.y));
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
                QPointF(p.x, m_window_origin.y),
                QPointF(p.x, m_window_origin.y + m_window_size.y));
        }
    }
}

void ChartBase::draw_horizontal_legend(QPainter& painter) const
{
    constexpr qreal TextMargin = 3.0;   // pixels

    assert(m_horizontal_legend_formatter);

    if (m_points.size() > 2)
    {
        for (const Vector2d& point : m_points)
        {
            const double x = point.x;
            const Vector2d p = convert_to_frame(point);
            const QString text = m_horizontal_legend_formatter(x);

            painter.drawText(
                QPointF(p.x + TextMargin, m_window_origin.y + m_window_size.y - m_window_bottom_margin - TextMargin),
                text);
        }
    }
}

void ChartBase::draw_vertical_legend(QPainter& painter) const
{
    constexpr qreal TextMargin = 3.0;   // pixels

    assert(m_vertical_legend_formatter);

    for (int i = 0; i < m_vertical_subdivisions; ++i)
    {
        const double y = fit(i, 0, m_vertical_subdivisions - 1, m_vertical_min, m_vertical_max);
        const Vector2d p = convert_to_frame(Vector2d(0.0, y));
        const QString text = m_vertical_legend_formatter(y);

        painter.drawText(
            QPointF(m_window_origin.x + m_window_left_margin + TextMargin, p.y - TextMargin),
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

void LineChart::set_points_visible(const bool visible)
{
    m_points_visible = visible;
}

void LineChart::set_points_highlightable(const bool highlightable)
{
    m_points_highlightable = highlightable;
}

void LineChart::draw_chart(QPainter& painter) const
{
    const QPointF ShadowOffset(3.0, 3.0);   // pixels
    constexpr qreal LineWidth = 2.0;        // pixels

    QPen pen;
    pen.setWidthF(LineWidth);

    if (m_shadow_enabled)
    {
        painter.save();
        pen.setBrush(m_shadow_brush);
        painter.setPen(pen);
        painter.setBrush(m_shadow_brush);
        if (m_points.size() > 1)
            draw_lines(painter, ShadowOffset);
        if (m_points_visible)
            draw_points(painter, ShadowOffset);
        painter.restore();
    }

    painter.save();
    pen.setBrush(m_curve_brush);
    painter.setPen(pen);
    painter.setBrush(m_curve_brush);
    if (m_points.size() > 1)
        draw_lines(painter);
    if (m_points_visible)
        draw_points(painter);
    painter.restore();
}

void LineChart::draw_lines(QPainter& painter, const QPointF& shadow_offset) const
{
    assert(m_points.size() > 1);

    for (std::size_t i = 0, e = m_points.size() - 1; i < e; ++i)
    {
        const Vector2d from = convert_to_frame(m_points[i]);
        const Vector2d to = convert_to_frame(m_points[i + 1]);

        painter.drawLine(
            QPointF(from.x, from.y) + shadow_offset,
            QPointF(to.x, to.y) + shadow_offset);
    }
}

void LineChart::draw_points(QPainter& painter, const QPointF& shadow_offset) const
{
    constexpr qreal DiskRadius = 4.0;

    for (const Vector2d& point : m_points)
    {
        const Vector2d p = convert_to_frame(point);

        painter.drawEllipse(
            QPointF(p.x, p.y) + shadow_offset,
            DiskRadius,
            DiskRadius);
    }
}

bool LineChart::on_chart(const QPoint& mouse_position, std::size_t& point_index) const
{
    constexpr double MinDistance = 15.0;

    const Vector2d mp(mouse_position.x(), mouse_position.y());

    double closest_square_distance = std::numeric_limits<double>::max();
    std::size_t closest_index = 0;

    for (std::size_t i = 0, e = m_points.size(); i < e; ++i)
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
    if (m_points_highlightable)
    {
        draw_cross(painter, mouse_position);
        draw_point_highlight(painter, mouse_position);
    }
}

void LineChart::draw_point_highlight(QPainter& painter, const QPoint& mouse_position) const
{
    const QColor DiskColor(190, 140, 50);
    constexpr qreal DiskRadius = 6.0;

    std::size_t point_index;
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
        QPointF(m_window_origin.x, mouse_position.y()),
        QPointF(m_window_origin.x + m_window_size.x, mouse_position.y()));

    painter.drawLine(
        QPointF(mouse_position.x(), m_window_origin.y),
        QPointF(mouse_position.x(), m_window_origin.y + m_window_size.y));

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
