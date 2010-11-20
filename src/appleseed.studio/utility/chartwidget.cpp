
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
#include "foundation/utility/foreach.h"

// Qt headers.
#include <QBrush>
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

AABB2d ChartBase::get_bbox() const
{
    AABB2d bbox;
    bbox.invalidate();

    for (size_t i = 0; i < m_points.size(); ++i)
        bbox.insert(m_points[i]);

    return bbox;
}


//
// LineChart class implementation.
//

void LineChart::render(QPainter& painter) const
{
    render_curve(painter);
}

void LineChart::render_curve(QPainter& painter) const
{
    if (m_points.size() < 2)
        return;

    const AABB2d bbox = get_bbox();
    const Vector2d rcp_bbox_extent = Vector2d(1.0) / bbox.extent();

    const QRect window = painter.window();
    const Vector2d window_origin(window.x(), window.y());
    const Vector2d window_size(window.width(), window.height());

    QPen pen;
    pen.setBrush(QBrush(QColor(255, 0, 0)));
    pen.setWidth(2);
    pen.setCapStyle(Qt::RoundCap);
    painter.setPen(pen);

    for (size_t i = 0; i < m_points.size() - 1; ++i)
    {
        const Vector2d from = (m_points[i] - bbox.min) * rcp_bbox_extent * window_size + window_origin;
        const Vector2d to = (m_points[i + 1] - bbox.min) * rcp_bbox_extent * window_size + window_origin;

        painter.drawLine(from.x, from.y, to.x, to.y);
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
