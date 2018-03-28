
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Francois Beaune, The appleseedhq Organization
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
#include "stdouttilecallback.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/kernel/aov/imagestack.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/platform/thread.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>
#include <cstdio>

// Platform headers.
#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace cli {

namespace
{
    //
    // StdOutTileCallbackFactory.
    //

    class StdOutTileCallback
      : public TileCallbackBase
    {
      public:
        StdOutTileCallback(const bool single_plane = false)
          : m_single_plane(single_plane)
          , m_header_sent(false)
        {
        }

        void release() override
        {
            // The factory always return the same tile callback instance.
            // Prevent this instance from being destroyed by doing nothing here.
        }

        void on_tile_begin(
            const Frame*    frame,
            const size_t    tile_x,
            const size_t    tile_y) override
        {
            boost::mutex::scoped_lock lock(m_mutex);

#ifdef _WIN32
            const int old_stdout_mode = _setmode(_fileno(stdout), _O_BINARY);
#endif
            send_highlight_tile(*frame, tile_x, tile_y);
#ifdef _WIN32
            _setmode(_fileno(stdout), old_stdout_mode);
#endif
        }

        void on_tile_end(
            const Frame*    frame,
            const size_t    tile_x,
            const size_t    tile_y) override
        {
            boost::mutex::scoped_lock lock(m_mutex);

#ifdef _WIN32
            const int old_stdout_mode = _setmode(_fileno(stdout), _O_BINARY);
#endif
            send_header(*frame);
            send_tile(*frame, tile_x, tile_y);

            fflush(stdout);
#ifdef _WIN32
            _setmode(_fileno(stdout), old_stdout_mode);
#endif
        }

      private:
        // Do not change the values of the enumerators as this WILL break client compabitility.
        enum ChunkType
        {
            // Protocol v2
            ChunkTypeTileHighlight          = 10,
            ChunkTypeTilesHeader            = 11,
            ChunkTypePlaneDefinition        = 12,
            ChunkTypeTileData               = 13
        };

        boost::mutex m_mutex;

        bool m_header_sent;
        const bool m_single_plane;

        void send_header(const Frame& frame)
        {
            if (m_header_sent) return;

            // Build and write tiles header.
            // This header is sent only once and can contains aovs and frame informations.
            const size_t chunk_size = 1 * sizeof(uint32);
            const size_t plane_count = m_single_plane ? 1 : 1 + frame.aov_images().size();
            const uint32 header[] =
            {
                static_cast<uint32>(ChunkTypeTilesHeader),
                static_cast<uint32>(chunk_size),
                static_cast<uint32>(plane_count),
            };
            fwrite(header, sizeof(header), 1, stdout);

            send_plane_definition(frame.image(), "beauty", 0);

            if (!m_single_plane)
            {
                for (size_t i = 0, e = frame.aov_images().size(); i < e; ++i)
                {
                    send_plane_definition(
                                frame.aov_images().get_image(i),
                                frame.aov_images().get_name(i),
                                i + 1);
                }
            }

            m_header_sent = true;
        }

        void send_plane_definition(
            const Image&            img,
            const char*             name,
            const size_t            index) const
        {
            // Build and write aov header.
            const size_t name_len = strlen(name);
            const size_t chunk_size = 3 * sizeof(uint32) + name_len * sizeof(char);
            const uint32 header[] =
            {
                static_cast<uint32>(ChunkTypePlaneDefinition),
                static_cast<uint32>(chunk_size),
                static_cast<uint32>(index),
                static_cast<uint32>(name_len),
                static_cast<uint32>(img.properties().m_channel_count)
            };
            fwrite(header, sizeof(header), 1, stdout);
            fwrite(name, sizeof(char), name_len, stdout);
        }

        void send_highlight_tile(
            const Frame&        frame,
            const size_t        tile_x,
            const size_t        tile_y) const
        {
            // Compute the coordinates in the image of the top-left corner of the tile.
            const CanvasProperties& frame_props = frame.image().properties();
            const size_t x = tile_x * frame_props.m_tile_width;
            const size_t y = tile_y * frame_props.m_tile_height;

            // Retrieve the source tile and its dimensions.
            const Tile& tile = frame.image().tile(tile_x, tile_y);
            const size_t w = tile.get_width();
            const size_t h = tile.get_height();

            // Build and write highlight tile header.
            // This header is sent for allowing to displaying marks arround the tile being renderer.
            const size_t chunk_size = 4 * sizeof(uint32);
            const uint32 header[] =
            {
                static_cast<uint32>(ChunkTypeTileHighlight),
                static_cast<uint32>(chunk_size),
                static_cast<uint32>(x),
                static_cast<uint32>(y),
                static_cast<uint32>(w),
                static_cast<uint32>(h)
            };
            fwrite(header, sizeof(header), 1, stdout);
        }

        void send_tile(
            const Frame&        frame,
            const size_t        tile_x,
            const size_t        tile_y) const
        {
            // We assume all AOV images have the same properties as the main image.
            const CanvasProperties& props = frame.image().properties();

            // Send beauty tile.
            do_send_tile(
                props,
                frame.image().tile(tile_x, tile_y),
                tile_x,
                tile_y,
                0);

            if (!m_single_plane)
            {
                // Send AOV tiles.
                for (size_t i = 0, e = frame.aov_images().size(); i < e; ++i)
                {
                    do_send_tile(
                        props,
                        frame.aov_images().get_image(i).tile(tile_x, tile_y),
                        tile_x,
                        tile_y,
                        i + 1);
                }
            }
        }

        void do_send_tile(
            const CanvasProperties& properties,
            const Tile&             tile,
            const size_t            tile_x,
            const size_t            tile_y,
            const size_t            plane_index) const
        {
            const size_t x = tile_x * properties.m_tile_width;
            const size_t y = tile_y * properties.m_tile_height;

            // Retrieve the tile dimensions
            const size_t w = tile.get_width();
            const size_t h = tile.get_height();
            const size_t c = tile.get_channel_count();

            // Build and write tile header.
            // This header contains information about the tile aov that will be written.
            const size_t chunk_size = 6 * sizeof(uint32) + w * h * c * sizeof(float);
            const uint32 header[] =
            {
                static_cast<uint32>(ChunkTypeTileData),
                static_cast<uint32>(chunk_size),
                static_cast<uint32>(plane_index),
                static_cast<uint32>(x),
                static_cast<uint32>(y),
                static_cast<uint32>(w),
                static_cast<uint32>(h),
                static_cast<uint32>(c),
            };
            fwrite(header, sizeof(header), 1, stdout);

            // Send tile pixels.
            if (properties.m_pixel_format != PixelFormatFloat)
            {
                const Tile tmp(tile, PixelFormatFloat);
                fwrite(tmp.get_storage(), 1, tmp.get_size(), stdout);
            }
            else
            {
                fwrite(tile.get_storage(), 1, tile.get_size(), stdout);
            }
        }
    };
}


//
// StdOutTileCallbackFactory class implementation.
//

StdOutTileCallbackFactory::StdOutTileCallbackFactory()
  : m_callback(new StdOutTileCallback(true))
{
}

void StdOutTileCallbackFactory::release()
{
    delete this;
}

ITileCallback* StdOutTileCallbackFactory::create()
{
    return m_callback.get();
}

}   // namespace cli
}   // namespace appleseed
