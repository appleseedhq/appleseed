
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

// appleseed.studio headers.
#include "mainwindow/rendering/viewportcanvas.h"

// appleseed.qtcommon headers.
#include "widgets/scrollareapanhandler.h"
#include "widgets/widgetzoomhandler.h"

// Qt headers.
#include <QObject>
#include <QWidget>

// Standard headers.
#include <memory>

// Forward declarations.
namespace renderer  { class Project; }

namespace appleseed {
namespace studio {

//
// A tab wrapping a render widget and its toolbar.
//

class ViewportTab
  : public QWidget
{
    Q_OBJECT

  public:
    ViewportTab(renderer::Project& project);

    virtual ViewportCanvas* get_viewport_canvas() const = 0;

    void clear();

    virtual void render_began();

    void update();
    virtual void update_size();

    virtual void on_tab_selected();

    struct State
    {
        qtcommon::WidgetZoomHandler::State      m_zoom_handler_state;
        qtcommon::ScrollAreaPanHandler::State   m_pan_handler_state;
    };

    State save_state() const;
    void load_state(const State& state);

  protected slots:
    void slot_reset_zoom();

  protected:
    renderer::Project&                                  m_project;

    std::unique_ptr<qtcommon::WidgetZoomHandler>        m_zoom_handler;
    std::unique_ptr<qtcommon::ScrollAreaPanHandler>     m_pan_handler;
};

}   // namespace studio
}   // namespace appleseed
