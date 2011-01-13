
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "tilejob.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/tile.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// TileJob class implementation.
//

// Constructor.
TileJob::TileJob(
    const TileRendererVector&   tile_renderers,
    const TileCallbackVector&   tile_callbacks,
    const Frame&                frame,
    const size_t                tile_x,
    const size_t                tile_y)
  : m_tile_renderers(tile_renderers)
  , m_tile_callbacks(tile_callbacks)
  , m_frame(frame)
  , m_tile_x(tile_x)
  , m_tile_y(tile_y)
{
    // Either there is no tile callback, or there is the same number
    // of tile callbacks and rendering threads.
    assert(
           m_tile_callbacks.size() == 0
        || m_tile_callbacks.size() == tile_renderers.size());
}

// Execute the job.
void TileJob::execute(const size_t thread_index)
{
    assert(thread_index < m_tile_renderers.size());

    // Retrieve the tile callback.
    ITileCallback* tile_callback =
        m_tile_callbacks.size() == m_tile_renderers.size()
            ? m_tile_callbacks[thread_index]
            : 0;

    // Call the pre-render tile callback.
    if (tile_callback)
    {
        const CanvasProperties& frame_props = m_frame.properties();
        const size_t x = m_tile_x * frame_props.m_tile_width;
        const size_t y = m_tile_y * frame_props.m_tile_height;

        const Tile& tile = m_frame.tile(m_tile_x, m_tile_y);
        const size_t width = tile.get_width();
        const size_t height = tile.get_height();

        tile_callback->pre_render(x, y, width, height);
    }

    // Render the tile.
    m_tile_renderers[thread_index]->render_tile(m_frame, m_tile_x, m_tile_y);

    // Call the post-render tile callback.
    if (tile_callback)
        tile_callback->post_render(m_frame, m_tile_x, m_tile_y);
}

}   // namespace renderer
