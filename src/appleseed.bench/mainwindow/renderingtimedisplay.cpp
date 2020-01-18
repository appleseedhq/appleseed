
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

// Interface header.
#include "renderingtimedisplay.h"

// appleseed.bench headers.
#include "utility/formatrendertime.h"

// Qt headers.
#include <QChar>
#include <QLabel>
#include <QString>

// Standard headers.
#include <cassert>
#include <cstdint>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace bench {

//
// RenderingTimeDisplay class implementation.
//

RenderingTimeDisplay::RenderingTimeDisplay(QLabel* label)
  : m_label(label)
{
}

void RenderingTimeDisplay::set_rendering_timer(RenderingTimer* rendering_timer)
{
    m_rendering_timer = rendering_timer;
}

void RenderingTimeDisplay::start()
{
    assert(m_rendering_timer != nullptr);
    assert(m_timer_id == -1);

    m_timer_id = startTimer(1000 / 4);
}

void RenderingTimeDisplay::stop()
{
    if (m_timer_id != -1)
    {
        killTimer(m_timer_id);
        m_timer_id = -1;
    }
}

void RenderingTimeDisplay::timerEvent(QTimerEvent* event)
{
    assert(m_rendering_timer);

    // todo: possible race condition. It would be neat if measuring a timer
    // was an immutable operation on the timer.
    if (m_rendering_timer->get_state() == RenderingTimer::State::Running)
        m_rendering_timer->measure();

    update();
}

void RenderingTimeDisplay::update()
{
    assert(m_rendering_timer);

    m_label->setText(
        format_render_time(
            static_cast<std::uint64_t>(m_rendering_timer->get_seconds())));
}

}   // namespace bench
}   // namespace appleseed
