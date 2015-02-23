
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Marius Avram, The appleseedhq Organization
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
#include "minimizebutton.h"

// Qt headers.
#include <QAction>
#include <QDockWidget>
#include <QMouseEvent>
#include <QStyle>

namespace appleseed {
namespace studio {

//
// MinimizeButton class implementation.
//

MinimizeButton::MinimizeButton(QDockWidget* dock_widget, QWidget* parent)
  : QPushButton(dock_widget->windowTitle(), parent)
  , m_dock_widget(dock_widget)
  , m_on(true)
  , m_minimized(false)
{
    setObjectName("toggle_button_on");
    connect(
        m_dock_widget->toggleViewAction(), SIGNAL(toggled(bool)),
        SLOT(slot_minimize()));
}

bool MinimizeButton::is_on() const
{
    return m_on;
}

void MinimizeButton::set_fullscreen(const bool on)
{
    if (on)
    {
        // Setting fullscreen on.
        m_minimized = m_on;
        if (!m_on)
            m_dock_widget->toggleViewAction()->activate(QAction::Trigger);
    }
    else
    {
        // Deactivating fullscreen. Keep state before fullscreen.
        if (!m_minimized)
            m_dock_widget->toggleViewAction()->activate(QAction::Trigger);
    }
}

void MinimizeButton::mousePressEvent(QMouseEvent* event)
{
    if (event->buttons() & Qt::LeftButton)
        m_dock_widget->toggleViewAction()->activate(QAction::Trigger);
}

void MinimizeButton::slot_minimize()
{
    m_on = !m_on;

    setObjectName(m_on ? "toggle_button_on" : "toggle_button_off");

    // Force stylesheet reloading for this widget.
    style()->unpolish(this);
    style()->polish(this);
}

}   // namespace studio
}   // namespace appleseed
