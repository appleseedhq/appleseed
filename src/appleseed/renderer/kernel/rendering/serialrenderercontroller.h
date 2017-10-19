
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_SERIALRENDERERCONTROLLER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_SERIALRENDERERCONTROLLER_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/irenderercontroller.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"

// Standard headers.
#include <cstddef>
#include <deque>

// Forward declarations.
namespace renderer  { class Frame; }
namespace renderer  { class ITileCallback; }

namespace renderer
{

//
// A renderer controller that can queue tile callback updates and execute them
// later in the master renderer's thread. Needed by the Python bindings.
//

class SerialRendererController
  : public IRendererController
{
  public:
    SerialRendererController(
        IRendererController*    controller,
        ITileCallback*          tile_callback);

    void on_rendering_begin() override;
    void on_rendering_success() override;
    void on_rendering_abort() override;
    void on_frame_begin() override;
    void on_frame_end() override;
    void on_progress() override;
    Status get_status() const override;

    void add_on_tiled_frame_begin_callback(
        const Frame*            frame);

    void add_on_tiled_frame_end_callback(
        const Frame*            frame);

    void add_on_tile_begin_callback(
        const Frame*            frame,
        const size_t            tile_x,
        const size_t            tile_y);

    void add_on_tile_end_callback(
        const Frame*            frame,
        const size_t            tile_x,
        const size_t            tile_y);

    void add_on_progressive_frame_begin_callback(
        const Frame*            frame);

    void add_on_progressive_frame_end_callback(
        const Frame*            frame);

  private:
    struct PendingTileCallback
    {
        enum CallbackType
        {
            OnTiledFrameBegin,
            OnTiledFrameEnd,
            OnTileBegin,
            OnTileEnd,
            OnProgressiveFrameBegin,
            OnProgressiveFrameEnd
        };

        CallbackType    m_type;
        const Frame*    m_frame;
        size_t          m_tile_x;
        size_t          m_tile_y;
    };

    IRendererController*                m_controller;
    ITileCallback*                      m_tile_callback;
    boost::mutex                        m_mutex;
    std::deque<PendingTileCallback>     m_pending_callbacks;

    void exec_callback(const PendingTileCallback& cb);
    void exec_callbacks();
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_SERIALRENDERERCONTROLLER_H
