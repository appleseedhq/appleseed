
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "serialrenderercontroller.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/itilecallback.h"

// appleseed.foundation headers.
#include "foundation/utility/otherwise.h"

namespace renderer
{

//
// SerialRendererController class implementation.
//

SerialRendererController::SerialRendererController(
    IRendererController*    controller,
    ITileCallback*          tile_callback)
  : m_controller(controller)
  , m_tile_callback(tile_callback)
{
}

void SerialRendererController::on_rendering_begin()
{
    m_controller->on_rendering_begin();
}

void SerialRendererController::on_rendering_success()
{
    m_controller->on_rendering_success();
}

void SerialRendererController::on_rendering_abort()
{
    m_controller->on_rendering_abort();
}

void SerialRendererController::on_frame_begin()
{
    m_controller->on_frame_begin();
}

void SerialRendererController::on_frame_end()
{
    m_controller->on_frame_end();
}

void SerialRendererController::on_progress()
{
    {
        boost::mutex::scoped_lock lock(m_mutex);

        while (!m_pending_callbacks.empty())
        {
            exec_callback(m_pending_callbacks.front());
            m_pending_callbacks.pop_front();
        }
    }

    m_controller->on_progress();
}

IRendererController::Status SerialRendererController::get_status() const
{
    return m_controller->get_status();
}

void SerialRendererController::add_pre_render_tile_callback(
    const size_t            x,
    const size_t            y,
    const size_t            width,
    const size_t            height)
{
    boost::mutex::scoped_lock lock(m_mutex);

    PendingTileCallback callback;
    callback.m_type = PendingTileCallback::PreRender;
    callback.m_frame = 0;
    callback.m_x = x;
    callback.m_y = y;
    callback.m_width = width;
    callback.m_height = height;

    m_pending_callbacks.push_back(callback);
}

void SerialRendererController::add_post_render_tile_callback(
    const Frame*            frame,
    const size_t            tile_x,
    const size_t            tile_y)
{
    boost::mutex::scoped_lock lock(m_mutex);

    PendingTileCallback callback;
    callback.m_type = PendingTileCallback::PostRenderTile;
    callback.m_frame = frame;
    callback.m_x = tile_x;
    callback.m_y = tile_y;
    callback.m_width = 0;
    callback.m_height = 0;

    m_pending_callbacks.push_back(callback);
}

void SerialRendererController::add_post_render_tile_callback(const Frame* frame)
{
    boost::mutex::scoped_lock lock(m_mutex);

    PendingTileCallback callback;
    callback.m_type = PendingTileCallback::PostRender;
    callback.m_frame = frame;
    callback.m_x = 0;
    callback.m_y = 0;
    callback.m_width = 0;
    callback.m_height = 0;

    m_pending_callbacks.push_back(callback);
}

void SerialRendererController::exec_callback(const PendingTileCallback& call)
{
    switch (call.m_type)
    {
      case PendingTileCallback::PreRender:
        m_tile_callback->pre_render(call.m_x, call.m_y, call.m_width, call.m_height);
        break;

      case PendingTileCallback::PostRenderTile:
        m_tile_callback->post_render_tile(call.m_frame, call.m_x, call.m_y);
        break;

      case PendingTileCallback::PostRender:
        m_tile_callback->post_render(call.m_frame);
        break;

      assert_otherwise;
    }
}

}   // namespace renderer
