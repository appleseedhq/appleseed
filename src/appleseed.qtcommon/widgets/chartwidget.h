
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

#pragma once

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
#include <QVariant>

// Standard headers.
#include <cstddef>
#include <functional>
#include <memory>
#include <vector>

// Forward declarations.
class QEvent;
class QMouseEvent;
class QPainter;
class QPaintEvent;
class QWidget;

namespace appleseed {
namespace qtcommon {

//
// Base class for charts.
//

class ChartBase
  : public foundation::NonCopyable
{
  public:
    using ValueFormatter = std::function<QString(const double)>;
    using TooltipFormatter = std::function<QString(const foundation::Vector2d&, const QVariant&)>;

    virtual ~ChartBase() = default;

    void set_auto_horizontal_range();
    void set_auto_vertical_range();
    void set_horizontal_range(const double min, const double max);
    void set_vertical_range(const double min, const double max);

    void set_vertical_subdivisions(const int subdivisions);

    void set_horizontal_window_margins(const double left, const double right);
    void set_vertical_window_margins(const double bottom, const double top);

    void set_grid_brush(const QBrush& brush);
    void set_legend_brush(const QBrush& brush);

    void set_horizontal_legend_formatter(const ValueFormatter& formatter);
    void set_vertical_legend_formatter(const ValueFormatter& formatter);
    void set_tooltip_formatter(const TooltipFormatter& formatter);

    void add_point(const foundation::Vector2d& p, const QVariant& data = QVariant());
    void add_point(const double x, const double y, const QVariant& data = QVariant());

    void prepare_drawing(QPainter& painter);

    virtual void draw_grid(QPainter& painter) const;
    virtual void draw_legends(QPainter& painter) const;

    virtual void draw_chart(QPainter& painter) const = 0;

    virtual void draw_tooltip(
        QPainter&       painter,
        const QPoint&   mouse_position) const;

    virtual void draw_highlight(
        QPainter&       painter,
        const QPoint&   mouse_position) const;

    virtual bool on_chart(
        const QPoint&   mouse_position,
        std::size_t&    point_index) const = 0;

  protected:
    bool                                m_auto_horizontal_range = true;
    bool                                m_auto_vertical_range = true;
    double                              m_horizontal_min = 0.0;
    double                              m_horizontal_max = 1.0;
    double                              m_vertical_min = 0.0;
    double                              m_vertical_max = 1.0;
    int                                 m_vertical_subdivisions = 10;
    double                              m_window_left_margin = 15.0;
    double                              m_window_right_margin = 15.0;
    double                              m_window_bottom_margin = 15.0;
    double                              m_window_top_margin = 15.0;
    QBrush                              m_grid_brush = QColor(50, 50, 50, 255);
    QBrush                              m_legend_brush = QColor(100, 100, 100, 255);
    ValueFormatter                      m_horizontal_legend_formatter;
    ValueFormatter                      m_vertical_legend_formatter;
    TooltipFormatter                    m_tooltip_formatter;

    std::vector<foundation::Vector2d>   m_points;
    std::vector<QVariant>               m_data;

    foundation::Vector2d                m_window_origin;
    foundation::Vector2d                m_window_size;
    double                              m_rcp_horizontal_extent;
    double                              m_rcp_vertical_extent;

    foundation::AABB2d compute_points_bbox() const;

    foundation::Vector2d convert_to_frame(
        const foundation::Vector2d&     point) const;

    foundation::Vector2d convert_to_data(
        const foundation::Vector2d&     point) const;

    void draw_horizontal_grid(QPainter& painter) const;
    void draw_vertical_grid(QPainter& painter) const;

    void draw_horizontal_legend(QPainter& painter) const;
    void draw_vertical_legend(QPainter& painter) const;
};


//
// A line chart.
//

class LineChart
  : public ChartBase
{
  public:
    void set_shadow_enabled(const bool enabled);

    void set_curve_brush(const QBrush& brush);
    void set_shadow_brush(const QBrush& brush);

    void set_points_visible(const bool visible);
    void set_points_highlightable(const bool highlightable);

    void draw_chart(QPainter& painter) const override;

    void draw_highlight(
        QPainter&       painter,
        const QPoint&   mouse_position) const override;

    bool on_chart(
        const QPoint&   mouse_position,
        std::size_t&    point_index) const override;

  private:
    bool    m_shadow_enabled = false;
    QBrush  m_curve_brush = QColor(255, 255, 255, 255);
    QBrush  m_shadow_brush = QColor(30, 30, 30, 255);
    bool    m_points_visible = true;
    bool    m_points_highlightable = true;

    void draw_lines(
        QPainter&       painter,
        const QPointF&  shadow_offset = QPointF(0.0, 0.0)) const;

    void draw_points(
        QPainter&       painter,
        const QPointF&  shadow_offset = QPointF(0.0, 0.0)) const;

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

    void clear();

    ChartBase* add_chart(std::unique_ptr<ChartBase> chart);

  private:
    using ChartCollection = std::vector<std::unique_ptr<ChartBase>>;

    ChartCollection     m_charts;
    bool                m_mouse_inside_widget;
    QPoint              m_mouse_position;

    void mouseMoveEvent(QMouseEvent* event) override;
    void leaveEvent(QEvent* event) override;

    void paintEvent(QPaintEvent* event) override;

    void draw_charts(QPainter& painter) const;
    void draw_highlights(QPainter& painter) const;
    void draw_tooltips(QPainter& painter) const;
    void draw_frame(QPainter& painter) const;
};

}   // namespace qtcommon
}   // namespace appleseed
