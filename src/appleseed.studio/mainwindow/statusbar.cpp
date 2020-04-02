
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

// Interface header.
#include "statusbar.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

//
// StatusBar class implementation.
//

void StatusBar::set_text(const std::string& text)
{
    setText(QString::fromStdString(text));
}

void StatusBar::start_rendering_time_display(RenderingTimer* rendering_timer)
{
    assert(m_rendering_timer == nullptr);
    assert(m_timer_id == -1);
    assert(rendering_timer);

    m_rendering_timer = rendering_timer;
    m_timer_id = startTimer(1000 / 4);
}

void StatusBar::stop_rendering_time_display()
{
    if (m_rendering_timer != nullptr)
    {
        assert(m_timer_id != -1);
        killTimer(m_timer_id);

        m_rendering_timer = nullptr;
        m_timer_id = -1;
    }
}

void StatusBar::timerEvent(QTimerEvent* event)
{
    assert(m_rendering_timer);

    // todo: possible race condition. It would be neat if measuring a timer
    // was an immutable operation on the timer.
    m_rendering_timer->measure();

    const double rendering_time = m_rendering_timer->get_seconds();
    const std::string rendering_time_string = pretty_time(rendering_time, 0);

    set_text("Rendering time: " + rendering_time_string);
}

}   // namespace studio
}   // namespace appleseed
