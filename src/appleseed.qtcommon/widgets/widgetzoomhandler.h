
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

// Qt headers.
#include <QObject>

// Forward declarations.
class QEvent;
class QKeyEvent;
class QScrollArea;
class QScrollBar;
class QWheelEvent;
class QWidget;

namespace appleseed {
namespace qtcommon {

class WidgetZoomHandler
  : public QObject
{
    Q_OBJECT

  public:
    WidgetZoomHandler(
        QScrollArea*    scroll_area,
        QWidget*        content_widget);

    ~WidgetZoomHandler() override;

    struct State
    {
        double          m_scale_factor;
    };

    State save_state() const;
    void load_state(const State& state);
    void reset_zoom();

  private:
    QScrollArea*        m_scroll_area;
    QWidget*            m_content_widget;
    const int           m_content_width;
    const int           m_content_height;
    double              m_min_scale_factor;
    double              m_max_scale_factor;
    State               m_state;

    bool eventFilter(QObject* object, QEvent* event) override;

    bool handle_key_press_event(QKeyEvent* event);
    bool handle_wheel_event(QWheelEvent* event);

    void zoom_in();
    void zoom_out();

    void compute_min_max_scale_factors();
    void multiply_scale_factor(const double multiplier);
    void apply_scale_factor();
};

}   // namespace qtcommon
}   // namespace appleseed
