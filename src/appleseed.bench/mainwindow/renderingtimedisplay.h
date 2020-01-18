
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/api/project.h"

// Qt headers.
#include <QObject>

// Forward declarations.
class QLabel;
class QTimerEvent;

namespace appleseed {
namespace bench {

class RenderingTimeDisplay
  : public QObject
{
    Q_OBJECT

  public:
    explicit RenderingTimeDisplay(QLabel* label);

    void set_rendering_timer(renderer::RenderingTimer* rendering_timer);

    void start();

    // It is safe to call this method even if rendering time display
    // hasn't started or has already been stopped.
    void stop();

    void update();

  private:
    QLabel*                     m_label;
    renderer::RenderingTimer*   m_rendering_timer = nullptr;
    int                         m_timer_id = -1;

    void timerEvent(QTimerEvent* event) override;
};

}   // namespace bench
}   // namespace appleseed
