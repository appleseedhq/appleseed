
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
#include "progresstilecallback.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/log.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cstddef>
#include <memory>
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace shared {

namespace
{
    //
    // ProgressTileCallback.
    //

    class ProgressTileCallback
      : public TileCallbackBase
    {
      public:
        explicit ProgressTileCallback(Logger& logger)
          : m_logger(logger)
          , m_rendered_pixels(0)
          , m_rendered_tiles(0)
        {
        }

        void release() override
        {
            // The factory always return the same tile callback instance.
            // Prevent this instance from being destroyed by doing nothing here.
        }

        void on_tiled_frame_begin(const Frame* frame) override
        {
            m_stopwatch.start();
        }

        void on_tiled_frame_end(const Frame* frame) override
        {
            m_stopwatch.clear();
        }

        void on_tile_end(
            const Frame*    frame,
            const size_t    tile_x,
            const size_t    tile_y) override
        {
            boost::mutex::scoped_lock lock(m_mutex);

            // Keep track of the total number of rendered pixels.
            const Tile& tile = frame->image().tile(tile_x, tile_y);
            m_rendered_pixels += tile.get_pixel_count();

            // Retrieve the total number of pixels in the frame.
            const size_t total_pixels = frame->image().properties().m_pixel_count;

            // Keep track of the total number of rendered tiles.
            ++m_rendered_tiles;

            // Retrieve the total number of tiles in the frame.
            const size_t total_tiles = frame->image().properties().m_tile_count;

            // Estimate remaining render time.
            m_stopwatch.measure();
            const double elapsed_time = m_stopwatch.get_seconds();
            const double remaining_time = (elapsed_time / m_rendered_tiles) * (total_tiles - m_rendered_tiles);

            // Print a progress message.
            LOG_INFO(
                m_logger,
                "rendering, %s done; about %s remaining...",
                pretty_percent(m_rendered_pixels, total_pixels).c_str(),
                pretty_time(remaining_time).c_str());
        }

      private:
        Logger&                             m_logger;
        boost::mutex                        m_mutex;
        size_t                              m_rendered_pixels;
        size_t                              m_rendered_tiles;
        Stopwatch<DefaultWallclockTimer>    m_stopwatch;
    };
}


//
// ProgressTileCallbackFactory class implementation.
//

struct ProgressTileCallbackFactory::Impl
{
    unique_ptr<ITileCallback> m_callback;
};

ProgressTileCallbackFactory::ProgressTileCallbackFactory(Logger& logger)
  : impl(new Impl())
{
    impl->m_callback.reset(new ProgressTileCallback(logger));
}

ProgressTileCallbackFactory::~ProgressTileCallbackFactory()
{
    delete impl;
}

void ProgressTileCallbackFactory::release()
{
    delete this;
}

ITileCallback* ProgressTileCallbackFactory::create()
{
    return impl->m_callback.get();
}

}   // namespace shared
}   // namespace appleseed
