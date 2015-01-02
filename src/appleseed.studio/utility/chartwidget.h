
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include <QObject>
#include <QPoint>
#include <QString>

// Standard headers.
#include <cstddef>
#include <memory>
#include <vector>

// Forward declarations.
class QEvent;
class QMouseEvent;
class QPainter;
class QPaintEvent;
class QWidget;

namespace appleseed {
namespace studio {

//
// Interface for tooltip formatters.
//

class IToolTipFormatter
{
  public:
    virtual ~IToolTipFormatter() {}

    virtual QString format(const foundation::Vector2d& point) const = 0;
};


//
// Base class for charts.
//

class ChartBase
  : public foundation::NonCopyable
{
  public:
    ChartBase();

    virtual ~ChartBase() {}

    void set_equidistant(const bool equidistant);

    void set_grid_brush(const QBrush& brush);

    void set_tooltip_formatter(std::auto_ptr<IToolTipFormatter> formatter);

    void add_point(const foundation::Vector2d& p);

    void add_point(const double x, const double y);

    void prepare_drawing(QPainter& painter);

    virtual void draw_grid(QPainter& painter) const;

    virtual void draw_chart(QPainter& painter) const = 0;

    virtual void draw_tooltip(
        QPainter&       painter,
        const QPoint&   mouse_position) const;

    virtual void draw_highlight(
        QPainter&       painter,
        const QPoint&   mouse_position) const;

    virtual bool on_chart(
        const QPoint&   mouse_position,
        size_t&         point_index) const = 0;

  protected:
    bool                                m_equidistant;
    foundation::Vector2d                m_margin;
    QBrush                              m_grid_brush;
    std::auto_ptr<IToolTipFormatter>    m_tooltip_formatter;

    std::vector<foundation::Vector2d>   m_original_points;
    std::vector<foundation::Vector2d>   m_points;

    foundation::AABB2d                  m_points_bbox;
    foundation::Vector2d                m_rcp_points_bbox_extent;
    foundation::Vector2d                m_window_origin;
    foundation::Vector2d                m_window_size;

    foundation::AABB2d compute_points_bbox() const;

    foundation::Vector2d convert_to_frame(
        const foundation::Vector2d&     point) const;

    foundation::Vector2d convert_to_data(
        const foundation::Vector2d&     point) const;

    void draw_horizontal_grid(QPainter& painter) const;
    void draw_vertical_grid(QPainter& painter) const;
};


//
// A line chart.
//

class LineChart
  : public ChartBase
{
  public:
    LineChart();

    void set_curve_brush(const QBrush& brush);

    virtual void draw_chart(QPainter& painter) const;

    virtual void draw_highlight(
        QPainter&       painter,
        const QPoint&   mouse_position) const;

    virtual bool on_chart(
        const QPoint&   mouse_position,
        size_t&         point_index) const;

  private:
    QBrush  m_curve_brush;

    void draw_point_highlight(
        QPainter&       painter,
        const QPoint&   mouse_position) const;

    void draw_cross(
        QPainter&       painter,
        const QPoint&   mouse_position) const;
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

    ChartCollection     m_charts;
    bool                m_mouse_inside_widget;
    QPoint              m_mouse_position;

    virtual void mouseMoveEvent(QMouseEvent* event);
    virtual void leaveEvent(QEvent* event);

    virtual void paintEvent(QPaintEvent* event);

    void draw_charts(QPainter& painter) const;
    void draw_highlights(QPainter& painter) const;
    void draw_tooltips(QPainter& painter) const;
    void draw_frame(QPainter& painter) const;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_UTILITY_CHARTWIDGET_H
