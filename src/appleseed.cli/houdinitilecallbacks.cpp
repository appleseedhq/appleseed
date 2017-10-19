
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Hans Hoogenboom, Esteban Tovagliari, The appleseedhq Organization
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
#include "houdinitilecallbacks.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/log.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cstdio>
#include <cstring>
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace cli {

namespace
{
    //
    // HoudiniTileCallback.
    //
    // This code is based on SideFX's tomdisplay and deepmplay examples distributed with Houdini.
    //

    class HoudiniTileCallback
      : public TileCallbackBase
    {
      public:
        static HoudiniTileCallback* connect_to_mplay(
            const char*             scene_name,
            const bool              progressive_mode,
            Logger&                 logger)
        {
            string cmd("imdisplay -p -f ");

            if (progressive_mode)
                cmd += "-k ";

            cmd += "-n ";
            cmd += scene_name;

            LOG_DEBUG(logger, "starting mplay with command: %s", cmd.c_str());

            FILE* fp = open_pipe(cmd.c_str());

            if (fp == nullptr)
                LOG_FATAL(logger, "unable to start mplay.");

            return new HoudiniTileCallback(false, fp, logger);
        }

        static HoudiniTileCallback* connect_to_hrmanpipe(
            const int               socket_number,
            const bool              progressive_mode,
            Logger&                 logger)
        {
            string cmd("hrmanpipe -f ");

            if (progressive_mode)
                cmd += "-m ";

            cmd += to_string(socket_number);

            LOG_DEBUG(logger, "starting hrmanpipe with command: %s", cmd.c_str());

            FILE* fp = open_pipe(cmd.c_str());

            if (fp == nullptr)
                LOG_FATAL(logger, "unable to start hrmanpipe.");

            return new HoudiniTileCallback(true, fp, logger);
        }

        ~HoudiniTileCallback() override
        {
            if (m_fp)
                close_pipe(m_fp);
        }

        void release() override
        {
            // The factory always return the same tile callback instance.
            // Prevent this instance from being destroyed by doing nothing here.
        }

        void on_tile_end(
            const Frame*            frame,
            const size_t            tile_x,
            const size_t            tile_y) override
        {
            boost::mutex::scoped_lock lock(m_mutex);

            send_header(*frame);
            send_tile(*frame, tile_x, tile_y);
        }

        void on_progressive_frame_end(const Frame* frame) override
        {
            boost::mutex::scoped_lock lock(m_mutex);

            send_header(*frame);

            const CanvasProperties& frame_props = frame->image().properties();

            for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
                for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
                    send_tile(*frame, tx, ty);
        }

      private:
        const bool      m_single_plane;
        FILE*           m_fp;
        Logger&         m_logger;

        bool            m_header_sent;
        boost::mutex    m_mutex;

        static FILE* open_pipe(const char* command)
        {
#ifdef _WIN32
            return _popen(command, "wb");
#else
            return popen(command, "w");
#endif
        }

        static void close_pipe(FILE* pipe)
        {
#ifdef _WIN32
            _pclose(pipe);
#else
            pclose(pipe);
#endif
        }

        static int map_pixel_format(const PixelFormat pixel_format)
        {
            switch (pixel_format)
            {
              case PixelFormatUInt8:
                return 1;

              case PixelFormatUInt16:
                return 2;

              case PixelFormatUInt32:
                return 4;

              case PixelFormatHalf:
              case PixelFormatFloat:
              case PixelFormatDouble:
                return 0;

              assert_otherwise;
            }

            return -1;
        }

        HoudiniTileCallback(
            const bool              single_plane,
            FILE*                   fp,
            Logger&                 logger)
          : m_single_plane(single_plane)
          , m_fp(fp)
          , m_logger(logger)
          , m_header_sent(false)
        {
        }

        void send_header(const Frame& frame)
        {
            if (!m_header_sent)
            {
                {
                    int header[8];
                    memset(header, 0, sizeof(header));

                    header[0] = (('h' << 24) + ('M' << 16) + ('P'<< 8) + ('0'));
                    header[1] = static_cast<int>(frame.image().properties().m_canvas_width);
                    header[2] = static_cast<int>(frame.image().properties().m_canvas_height);

                    if (m_single_plane)
                    {
                        header[3] = map_pixel_format(frame.image().properties().m_pixel_format);
                        header[4] = static_cast<int>(frame.image().properties().m_channel_count);
                    }
                    else
                    {
                        header[5] = static_cast<int>(1 + frame.aov_images().size());
                    }

                    if (fwrite(header, sizeof(int), 8, m_fp) != 8)
                        LOG_FATAL(m_logger, "unable to write header.");
                }

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
        }

        void send_plane_definition(
            const Image&            img,
            const char*             name,
            const size_t            index) const
        {
            int plane_def[8];
            memset(plane_def, 0, sizeof(plane_def));

            plane_def[0] = static_cast<int>(index);
            plane_def[1] = static_cast<int>(strlen(name));
            plane_def[2] = map_pixel_format(img.properties().m_pixel_format);

            if (img.properties().m_pixel_format == PixelFormatDouble)
                LOG_WARNING(m_logger, "Houdini does not support double precision pixels, converting them to single precision.");

            plane_def[3] = static_cast<int>(img.properties().m_channel_count);

            if (fwrite(plane_def, sizeof(int), 8, m_fp) != 8)
                LOG_FATAL(m_logger, "error sending plane definition.");

            if (fwrite(name, sizeof(char), plane_def[1], m_fp) != plane_def[1])
                LOG_FATAL(m_logger, "error sending plane name.");
        }

        void send_tile(
            const Frame&            frame,
            const size_t            tile_x,
            const size_t            tile_y) const
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
            int tile_head[4];

            // First, tell the reader what the plane index is for the data being
            // sent.  This is done by sending a special tile header.  The x0
            // coordinate is set to -1 to indicate the special header.  The x1
            // coordinate is set to the plane index.  The Y coordinates must be
            // zero.
            tile_head[0] = -1;
            tile_head[1] = static_cast<int>(plane_index);
            tile_head[2] = 0;
            tile_head[3] = 0;
            if (fwrite(tile_head, sizeof(int), 4, m_fp) != 4)
                LOG_FATAL(m_logger, "error sending tile index.");

            // Send tile header.
            tile_head[0] = static_cast<int>(tile_x * properties.m_tile_width);
            tile_head[1] = static_cast<int>(tile_head[0] + tile.get_width() - 1);
            tile_head[2] = static_cast<int>(tile_y * properties.m_tile_height);
            tile_head[3] = static_cast<int>(tile_head[2] + tile.get_height() - 1);
            if (fwrite(tile_head, sizeof(int), 4, m_fp) != 4)
                LOG_FATAL(m_logger, "error sending tile header.");

            // Send tile pixels.
            if (properties.m_pixel_format != PixelFormatFloat)
            {
                const Tile tmp(tile, PixelFormatFloat);
                if (fwrite(tmp.get_storage(), 1, tmp.get_size(), m_fp) != tmp.get_size())
                    LOG_FATAL(m_logger, "error sending tile pixels.");
            }
            else
            {
                if (fwrite(tile.get_storage(), 1, tile.get_size(), m_fp) != tile.get_size())
                    LOG_FATAL(m_logger, "error sending tile pixels.");
            }

            fflush(m_fp);
        }
    };
}


//
// MPlayTileCallbackFactory class implementation.
//

MPlayTileCallbackFactory::MPlayTileCallbackFactory(
    const char*   scene_name,
    const bool    progressive_mode,
    Logger&       logger)
  : m_callback(
        HoudiniTileCallback::connect_to_mplay(
            scene_name,
            progressive_mode,
            logger))
{
}

void MPlayTileCallbackFactory::release()
{
    delete this;
}

ITileCallback* MPlayTileCallbackFactory::create()
{
    return m_callback.get();
}


//
// HRmanPipeTileCallbackFactory class implementation.
//

HRmanPipeTileCallbackFactory::HRmanPipeTileCallbackFactory(
    const int   socket_number,
    const bool  progressive_mode,
    Logger&     logger)
  : m_callback(
        HoudiniTileCallback::connect_to_hrmanpipe(
            socket_number,
            progressive_mode,
            logger))
{
}

void HRmanPipeTileCallbackFactory::release()
{
    delete this;
}

ITileCallback* HRmanPipeTileCallbackFactory::create()
{
    return m_callback.get();
}

}   // namespace cli
}   // namespace appleseed
