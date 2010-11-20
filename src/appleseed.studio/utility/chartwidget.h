
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

#ifndef APPLESEED_STUDIO_UTILITY_CHARTWIDGET_H
#define APPLESEED_STUDIO_UTILITY_CHARTWIDGET_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Qt headers.
#include <QBrush>
#include <QFrame>
#include <QImage>
#include <QObject>

// Standard headers.
#include <memory>
#include <vector>

// Forward declarations.
class QPainter;
class QPaintEvent;
class QWidget;

namespace appleseed {
namespace studio {

//
// Base class for charts.
//

class ChartBase
  : public foundation::NonCopyable
{
  public:
    virtual ~ChartBase() {}

    void add_point(const foundation::Vector2d& p);
    void add_point(const double x, const double y);

    virtual void render(QPainter& painter) = 0;

  protected:
    std::vector<foundation::Vector2d>   m_points;

    foundation::AABB2d                  m_points_bbox;
    foundation::Vector2d                m_rcp_points_bbox_extent;
    foundation::Vector2d                m_window_origin;
    foundation::Vector2d                m_window_size;

    foundation::AABB2d compute_points_bbox() const;

    void prepare_rendering(QPainter& painter);

    foundation::Vector2d convert_to_frame(
        const foundation::Vector2d&     point) const;

    foundation::Vector2d convert_to_frame(
        const foundation::Vector2d&     point,
        const double                    margin_x,
        const double                    margin_y) const;
};


//
// A line chart.
//

class LineChart
  : public ChartBase
{
  public:
    LineChart();

    void set_grid_brush(const QBrush& brush);
    void set_curve_brush(const QBrush& brush);

    virtual void render(QPainter& painter);

  private:
    QBrush  m_grid_brush;
    QBrush  m_curve_brush;
    double  m_curve_margin_x;
    double  m_curve_margin_y;

    void render_grids(QPainter& painter) const;
    void render_horizontal_grid(QPainter& painter) const;
    void render_vertical_grid(QPainter& painter) const;
    void render_curve(QPainter& painter) const;
};


//
// A widget to display charts.
//

class ChartWidget
  : public QFrame
{
    Q_OBJECT

  public:
    explicit ChartWidget(QWidget* parent);

    ~ChartWidget();

    void clear();

    void add_chart(std::auto_ptr<ChartBase> chart);

  private:
    typedef std::vector<ChartBase*> ChartCollection;

    ChartCollection m_charts;

    virtual void paintEvent(QPaintEvent* event);

    void render(QImage& image) const;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_UTILITY_CHARTWIDGET_H
