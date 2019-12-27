
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

// Standard headers.
#include <cstddef>
#include <deque>
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
    using TooltipFormatter = std::function<QString(const foundation::Vector2d&)>;

    virtual ~ChartBase() = default;

    void set_equidistant(const bool equidistant);
    void set_origin(const foundation::Vector2d& origin);

    void set_grid_brush(const QBrush& brush);
    void set_legend_brush(const QBrush& brush);

    void set_horizontal_legend_formatter(const ValueFormatter& formatter);
    void set_vertical_legend_formatter(const ValueFormatter& formatter);
    void set_tooltip_formatter(const TooltipFormatter& formatter);

    size_t size() const;
    void push_back(const foundation::Vector2d& p);
    void push_back(const double x, const double y);
    void pop_front();

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
        size_t&         point_index) const = 0;

  protected:
    bool                                m_equidistant = false;
    foundation::Vector2d                m_origin = foundation::Vector2d(0.0);
    foundation::Vector2d                m_margin = foundation::Vector2d(15.0);
    QBrush                              m_grid_brush = QColor(50, 50, 50, 255);
    QBrush                              m_legend_brush = QColor(100, 100, 100, 255);
    ValueFormatter                      m_horizontal_legend_formatter;
    ValueFormatter                      m_vertical_legend_formatter;
    TooltipFormatter                    m_tooltip_formatter;

    std::deque<foundation::Vector2d>    m_original_points;
    std::deque<foundation::Vector2d>    m_points;

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
    void set_draw_points(const bool draw_points);

    void draw_chart(QPainter& painter) const override;

    void draw_highlight(
        QPainter&       painter,
        const QPoint&   mouse_position) const override;

    bool on_chart(
        const QPoint&   mouse_position,
        size_t&         point_index) const override;

  private:
    bool    m_shadow_enabled = false;
    QBrush  m_curve_brush = QColor(255, 255, 255, 255);
    QBrush  m_shadow_brush = QColor(30, 30, 30, 255);
    bool    m_draw_points = true;

    void draw_lines(
        QPainter&       painter,
        const QPoint&   shadow_offset = QPoint(0, 0)) const;

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
