
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
#include <QImage>
#include <QPainter>
#include <QStyle>
#include <QStyleOptionFrame>

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


//
// LineChart class implementation.
//

void LineChart::paint(QPainter& painter) const
{
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
    for (const_each<ChartCollection> i = m_charts; i; ++i)
        delete *i;
}

void ChartWidget::add(auto_ptr<ChartBase> chart)
{
    m_charts.push_back(chart.release());
}

void ChartWidget::paintEvent(QPaintEvent* event)
{
    // Render the charts into a QImage.
    QImage image(size(), QImage::Format_RGB32);
    paint(image);

    QPainter painter(this);

    // Draw the QImage.
    painter.drawImage(0, 0, image);

    // Draw the frame using the current style.
    QStyleOptionFrame option;
    option.initFrom(this);
    style()->drawPrimitive(QStyle::PE_Frame, &option, &painter, this);
}

void ChartWidget::paint(QImage& image) const
{
    QPainter painter(&image);
    painter.initFrom(this);
    painter.setRenderHint(QPainter::Antialiasing, true);

    painter.setBackground(QBrush(QColor(255, 0, 0)));
    painter.eraseRect(rect());

    for (const_each<ChartCollection> i = m_charts; i; ++i)
        (*i)->paint(painter);
}

}   // namespace studio
}   // namespace appleseed
