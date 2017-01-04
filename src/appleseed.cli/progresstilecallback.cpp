
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

// Interface header.
#include "progresstilecallback.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/log.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace cli {

//
// ProgressTileCallback class implementation.
//

ProgressTileCallback::ProgressTileCallback(Logger& logger)
  : m_logger(logger)
  , m_rendered_pixels(0)
{
}

void ProgressTileCallback::release()
{
    // Do nothing.
}

void ProgressTileCallback::post_render_tile(
    const Frame*    frame,
    const size_t    tile_x,
    const size_t    tile_y)
{
    boost::mutex::scoped_lock lock(m_mutex);
    do_post_render_tile(frame, tile_x, tile_y);
}

void ProgressTileCallback::do_post_render_tile(
    const Frame*    frame,
    const size_t    tile_x,
    const size_t    tile_y)
{
    // Keep track of the total number of rendered pixels.
    const Tile& tile = frame->image().tile(tile_x, tile_y);
    m_rendered_pixels += tile.get_pixel_count();

    // Retrieve the total number of pixels in the frame.
    const size_t total_pixels = frame->image().properties().m_pixel_count;

    // Print a progress message.
    LOG_INFO(m_logger, "rendering, %s done", pretty_percent(m_rendered_pixels, total_pixels).c_str());
}


//
// ProgressTileCallbackFactory class implementation.
//

ProgressTileCallbackFactory::ProgressTileCallbackFactory(Logger& logger)
  : m_callback(new ProgressTileCallback(logger))
{
}

void ProgressTileCallbackFactory::release()
{
    delete this;
}

ITileCallback* ProgressTileCallbackFactory::create()
{
    return m_callback.get();
}

}   // namespace cli
}   // namespace appleseed
