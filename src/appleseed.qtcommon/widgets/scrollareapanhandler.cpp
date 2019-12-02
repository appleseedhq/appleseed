
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
#include "scrollareapanhandler.h"

// Qt headers.
#include <QEvent>
#include <QMouseEvent>
#include <QScrollArea>
#include <QScrollBar>
#include <Qt>

// Standard headers.
#include <cassert>

namespace appleseed {
namespace qtcommon {

ScrollAreaPanHandler::ScrollAreaPanHandler(QScrollArea* scroll_area)
  : m_scroll_area(scroll_area)
  , m_is_dragging(false)
{
    m_scroll_area->installEventFilter(this);
}

ScrollAreaPanHandler::~ScrollAreaPanHandler()
{
    m_scroll_area->removeEventFilter(this);
}

ScrollAreaPanHandler::State ScrollAreaPanHandler::save_state() const
{
    State state;
    state.m_hbar_value = m_scroll_area->horizontalScrollBar()->value();
    state.m_vbar_value = m_scroll_area->verticalScrollBar()->value();
    return state;
}

void ScrollAreaPanHandler::load_state(const State& state)
{
    m_state = state;

    m_scroll_area->horizontalScrollBar()->setValue(m_state.m_hbar_value);
    m_scroll_area->verticalScrollBar()->setValue(m_state.m_vbar_value);
}

bool ScrollAreaPanHandler::eventFilter(QObject* object, QEvent* event)
{
    assert(object == m_scroll_area);

    switch (event->type())
    {
      case QEvent::MouseButtonPress:
      case QEvent::MouseButtonDblClick:
        if (handle_mouse_button_press_event(static_cast<QMouseEvent*>(event)))
            return true;
        break;

      case QEvent::MouseButtonRelease:
        if (handle_mouse_button_release_event(static_cast<QMouseEvent*>(event)))
            return true;
        break;

      case QEvent::MouseMove:
        if (handle_mouse_move_event(static_cast<QMouseEvent*>(event)))
            return true;
        break;
    }

    return QObject::eventFilter(object, event);
}

bool ScrollAreaPanHandler::handle_mouse_button_press_event(QMouseEvent* event)
{
    if (!(event->modifiers() & Qt::ShiftModifier))
        return false;

    if (event->button() != Qt::LeftButton)
        return false;

    if (m_is_dragging)
        return false;

    m_is_dragging = true;

    m_initial_mouse_position = event->pos();

    m_initial_hbar_value = m_scroll_area->horizontalScrollBar()->value();
    m_initial_vbar_value = m_scroll_area->verticalScrollBar()->value();

    m_scroll_area->setCursor(Qt::ClosedHandCursor);

    return true;
}

bool ScrollAreaPanHandler::handle_mouse_button_release_event(QMouseEvent* event)
{
    if (event->button() != Qt::LeftButton)
        return false;

    if (!m_is_dragging)
        return false;

    m_scroll_area->setCursor(Qt::ArrowCursor);

    m_is_dragging = false;

    return true;
}

bool ScrollAreaPanHandler::handle_mouse_move_event(QMouseEvent* event)
{
    if (!m_is_dragging)
        return false;

    const QPoint delta = event->pos() - m_initial_mouse_position;

    m_scroll_area->horizontalScrollBar()->setValue(m_initial_hbar_value - delta.x());
    m_scroll_area->verticalScrollBar()->setValue(m_initial_vbar_value - delta.y());

    return true;
}

}   // namespace qtcommon
}   // namespace appleseed
