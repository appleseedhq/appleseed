
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
#include <QPoint>

// Forward declarations.
namespace appleseed { namespace qtcommon { class MouseCoordinatesTracker; } }
class QEvent;
class QRect;
class QRubberBand;
class QWidget;

namespace appleseed {
namespace studio {

class RenderRegionHandler
  : public QObject
{
    Q_OBJECT

  public:
    // Default mode is RectangleSelectionMode.
    RenderRegionHandler(
        QWidget*                                    widget,
        const qtcommon::MouseCoordinatesTracker&    mouse_tracker);

    ~RenderRegionHandler() override;

    void set_enabled(const bool enabled);

    enum Mode
    {
        RectangleSelectionMode,
        RenderRegionMode
    };

    void set_mode(const Mode mode);

  signals:
    void signal_rectangle_selection(const QRect& rect);
    void signal_render_region(const QRect& rect);

  private:
    QWidget*                                        m_widget;
    const qtcommon::MouseCoordinatesTracker&        m_mouse_tracker;
    bool                                            m_enabled;
    Mode                                            m_mode;
    QRubberBand*                                    m_rubber_band;
    QPoint                                          m_origin;

    bool eventFilter(QObject* object, QEvent* event) override;
};

}   // namespace studio
}   // namespace appleseed
