
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "foundation/math/scalar.h"
#include "foundation/utility/foreach.h"

// Qt headers.
#include <QColor>
#include <QImage>
#include <QPainter>
#include <QPen>
#include <QStyle>
#include <QStyleOptionFrame>
#include <Qt>

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace foundation;
using namespace std;

namespace appleseed {
namespace studio {

//
// ChartBase class implementation.
//

void ChartBase::add_point(const Vector2d& p)
{
    m_points.push_back(p);
}

void ChartBase::add_point(const double x, const double y)
{
    m_points.push_back(Vector2d(x, y));
}

AABB2d ChartBase::compute_points_bbox() const
{
    AABB2d bbox;
    bbox.invalidate();

    for (size_t i = 0; i < m_points.size(); ++i)
        bbox.insert(m_points[i]);

    return bbox;
}

void ChartBase::prepare_rendering(QPainter& painter)
{
    m_points_bbox = compute_points_bbox();
    m_rcp_points_bbox_extent = Vector2d(1.0) / m_points_bbox.extent();

    const QRect window = painter.window();

    m_window_origin = Vector2d(window.x(), window.y());
    m_window_size = Vector2d(window.width(), window.height());
}

Vector2d ChartBase::convert_to_frame(const Vector2d& point) const
{
    return convert_to_frame(point, 0.0, 0.0);
}

Vector2d ChartBase::convert_to_frame(
    const Vector2d&     point,
    const double        margin_x,
    const double        margin_y) const
{
    Vector2d u = (point - m_points_bbox.min) * m_rcp_points_bbox_extent;

    u.y = 1.0 - u.y;

    const Vector2d window_size(
        m_window_size.x - 2.0 * margin_x,
        m_window_size.y - 2.0 * margin_y);

    const Vector2d window_origin(
        m_window_origin.x + margin_x,
        m_window_origin.y + margin_y);

    return u * window_size + window_origin;
}


//
// LineChart class implementation.
//

LineChart::LineChart()
  : m_grid_brush(QBrush(QColor(50, 50, 50, 255)))
  , m_curve_brush(QBrush(QColor(255, 255, 255, 255)))
  , m_curve_margin_x(5.0)
  , m_curve_margin_y(15.0)
{
}

void LineChart::set_grid_brush(const QBrush& brush)
{
    m_grid_brush = brush;
}

void LineChart::set_curve_brush(const QBrush& brush)
{
    m_curve_brush = brush;
}

void LineChart::render(QPainter& painter)
{
    prepare_rendering(painter);
    render_grids(painter);
    render_curve(painter);
}

void LineChart::render_grids(QPainter& painter) const
{
    QPen pen;
    pen.setBrush(m_grid_brush);
    pen.setWidthF(1.0);
    painter.setPen(pen);

    render_horizontal_grid(painter);
    render_vertical_grid(painter);
}

void LineChart::render_horizontal_grid(QPainter& painter) const
{
    const size_t Subdivisions = 10;

    for (size_t i = 0; i < Subdivisions; ++i)
    {
        const double k = static_cast<double>(i) / (Subdivisions - 1);
        const double y = mix(m_points_bbox.min.y, m_points_bbox.max.y, k);

        const Vector2d p =
            convert_to_frame(
                Vector2d(0.0, y),
                m_curve_margin_x,
                m_curve_margin_y);

        painter.drawLine(
            m_window_origin.x, p.y,
            m_window_origin.x + m_window_size.x, p.y);
    }
}

void LineChart::render_vertical_grid(QPainter& painter) const
{
    if (m_points.size() > 2)
    {
        for (size_t i = 1; i < m_points.size() - 1; ++i)
        {
            const Vector2d p = convert_to_frame(m_points[i]);

            painter.drawLine(
                p.x, m_window_origin.y,
                p.x, m_window_origin.y + m_window_size.y);
        }
    }
}

void LineChart::render_curve(QPainter& painter) const
{
    if (m_points.size() > 1)
    {
        QPen pen;
        pen.setBrush(m_curve_brush);
        pen.setWidthF(2.0);
        painter.setPen(pen);

        for (size_t i = 0; i < m_points.size() - 1; ++i)
        {
            const Vector2d from =
                convert_to_frame(
                    m_points[i],
                    m_curve_margin_x,
                    m_curve_margin_y);

            const Vector2d to =
                convert_to_frame(
                    m_points[i + 1],
                    m_curve_margin_x,
                    m_curve_margin_y);

            painter.drawLine(from.x, from.y, to.x, to.y);
        }
    }
}


//
// ChartWidget class implementation.
//

ChartWidget::ChartWidget(QWidget* parent)
  : QFrame(parent)
{
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

void ChartWidget::paintEvent(QPaintEvent* event)
{
    // Render the charts into a QImage.
    QImage image(size(), QImage::Format_ARGB32);
    image.fill(QColor(0, 0, 0, 0).rgba());
    render(image);

    QPainter painter(this);

    // Draw the QImage.
    painter.drawImage(0, 0, image);

    // Draw the frame using the current style.
    QStyleOptionFrame option;
    option.initFrom(this);
    style()->drawPrimitive(QStyle::PE_Frame, &option, &painter, this);
}

void ChartWidget::render(QImage& image) const
{
    QPainter painter(&image);
    painter.initFrom(this);
    painter.setRenderHint(QPainter::Antialiasing, true);

    for (const_each<ChartCollection> i = m_charts; i; ++i)
        (*i)->render(painter);
}

}   // namespace studio
}   // namespace appleseed
