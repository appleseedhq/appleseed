
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
#include "viewporttab.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

//
// ViewportTab class implementation.
//

ViewportTab::ViewportTab(Project& project)
  : m_project(project)
{
}

void ViewportTab::clear()
{
    ViewportCanvas* viewport_canvas = get_viewport_canvas();
    viewport_canvas->get_render_layer()->clear();
    viewport_canvas->repaint();
}

void ViewportTab::render_began()
{
    ViewportCanvas* viewport_canvas = get_viewport_canvas();
    viewport_canvas->get_render_layer()->darken();
    viewport_canvas->get_light_paths_layer()->update_render_camera_transform();
}

void ViewportTab::reset_zoom()
{
    m_zoom_handler->reset_zoom();
}

void ViewportTab::update()
{
    ViewportCanvas* viewport_canvas = get_viewport_canvas();
    viewport_canvas->update();
}

void ViewportTab::update_size()
{
    const CanvasProperties& props = m_project.get_frame()->image().properties();

    ViewportCanvas* viewport_canvas = get_viewport_canvas();
    viewport_canvas->resize(
        props.m_canvas_width,
        props.m_canvas_height);
}

void ViewportTab::on_tab_selected()
{
}

ViewportTab::State ViewportTab::save_state() const
{
    State state;
    state.m_zoom_handler_state = m_zoom_handler->save_state();
    state.m_pan_handler_state = m_pan_handler->save_state();
    return state;
}

void ViewportTab::load_state(const State& state)
{
    // The order matters here.
    m_zoom_handler->load_state(state.m_zoom_handler_state);
    m_pan_handler->load_state(state.m_pan_handler_state);
}

}   // namespace studio
}   // namespace appleseed

