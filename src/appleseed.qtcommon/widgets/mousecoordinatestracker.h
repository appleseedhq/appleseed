
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
#include "foundation/math/vector.h"

// Qt headers.
#include <QObject>

// Forward declarations.
class QEvent;
class QLabel;
class QPoint;
class QWidget;

namespace appleseed {
namespace qtcommon {

class MouseCoordinatesTracker
  : public QObject
{
    Q_OBJECT

  public:
    MouseCoordinatesTracker(
        QWidget*    widget,
        QLabel*     label);

    ~MouseCoordinatesTracker() override;

    foundation::Vector2d widget_to_ndc(const QPoint& point) const;
    foundation::Vector2i widget_to_pixel(const QPoint& point) const;

  private:
    QWidget*        m_widget;
    QLabel*         m_label;
    const int       m_content_width;
    const int       m_content_height;

    bool eventFilter(QObject* object, QEvent* event) override;

    void set_label_text(const QPoint& point) const;

};

}   // namespace qtcommon
}   // namespace appleseed
