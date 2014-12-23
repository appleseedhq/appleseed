
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

//
// The ContinuousSavingTileCallback can work in one of two possible ways:
//
// - if APPLESEED_CLI_CONTINUOUSSAVINGTILECALLBACK_TILED is defined,
// and the output file format is EXR, the tile callback will create a
// tiled EXR file and write tiles as they are renderered.
// This is the preferred way, if applications support
// reading incomplete EXR files (they should).
//
// - if APPLESEED_CLI_CONTINUOUSSAVINGTILECALLBACK_TILED is not defined,
// the tile callback writes complete images after each tile is renderered.
// This is helpful for applications that can't read incomplete images.
// Because Blenderseed is the biggest user of this tile callback and Blender
// does not read incomplete EXR files correctly, this is the default mode.
//
// PNG files are always written complete, independent of the define,
// as PNG does not support tiles.
//

// Interface header.
#include "continuoussavingtilecallback.h"

// appleseed.cli headers.
#include "progresstilecallback.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/progressiveexrimagefilewriter.h"

// boost libraries
#include "boost/thread/mutex.hpp"
#include "boost/filesystem/operations.hpp"
#include "boost/random/mersenne_twister.hpp"
#include "boost/uuid/uuid.hpp"
#include "boost/uuid/uuid_io.hpp"
#include "boost/uuid/random_generator.hpp"

// Standard headers.
#include <cstddef>

using namespace foundation;
using namespace renderer;
using namespace boost;
using namespace std;

namespace appleseed {
namespace cli {

//
// ContinuousSavingTileCallback.
//

namespace
{
    class ContinuousSavingTileCallback
      : public ProgressTileCallback
    {
      public:
        ContinuousSavingTileCallback(const string& output_filename, Logger& logger)
          : ProgressTileCallback(logger)
          , m_output_path(output_filename)
#ifdef APPLESEED_CLI_CONTINUOUSSAVINGTILECALLBACK_TILED
          , m_exr_writer(&logger)
          , m_write_tiled_image(false)
#endif
        {
            filesystem::path ext = m_output_path.extension();

#ifdef APPLESEED_CLI_CONTINUOUSSAVINGTILECALLBACK_TILED
            // Only write tiled images if the format is EXR.
            m_write_tiled_image = ext.string() == ".exr";
#endif
            if (!m_write_tiled_image)
            {
                m_output_tmp_path = m_output_path.parent_path();
                mt19937 rng;
                rng.seed(time(0));
                uuids::uuid u = uuids::basic_random_generator<boost::mt19937>(&rng)();
                string tmp_filename = uuids::to_string(u);
                tmp_filename.append(ext.string());
                m_output_tmp_path /= tmp_filename;
            }
        }

        ~ContinuousSavingTileCallback()
        {
#ifdef APPLESEED_CLI_CONTINUOUSSAVINGTILECALLBACK_TILED
            if (m_exr_writer.is_open())
                m_exr_writer.close();
#endif
        }

      private:
        mutex                           m_mutex;
        filesystem::path                m_output_path;
        filesystem::path                m_output_tmp_path;
        bool                            m_write_tiled_image;
#ifdef APPLESEED_CLI_CONTINUOUSSAVINGTILECALLBACK_TILED
        ProgressiveEXRImageFileWriter   m_exr_writer;
#endif

        virtual void do_post_render_tile(
            const Frame*    frame,
            const size_t    tile_x,
            const size_t    tile_y) APPLESEED_OVERRIDE
        {
            mutex::scoped_lock lock(m_mutex);

            ProgressTileCallback::do_post_render_tile(frame, tile_x, tile_y);

#ifdef APPLESEED_CLI_CONTINUOUSSAVINGTILECALLBACK_TILED
            if ( m_write_tiled_image)
            {
                if (!m_exr_writer.is_open())
                {
                    m_exr_writer.open(
                        m_output_path.c_str(),
                        frame->image().properties(),
                        ImageAttributes::create_default_attributes());
                }

                const Tile& tile = frame->image().tile(tile_x, tile_y);
                m_exr_writer.write_tile(tile, tile_x, tile_y);
            }
            else
#endif
            {
                frame->write_main_image(m_output_tmp_path.c_str());
                filesystem::rename(m_output_tmp_path, m_output_path);
            }
        }
    };
}


//
// ContinuousSavingTileCallbackFactory class implementation.
//

ContinuousSavingTileCallbackFactory::ContinuousSavingTileCallbackFactory(
    const string&   output_filename,
    Logger&         logger)
  : m_callback(new ContinuousSavingTileCallback(output_filename, logger))
{
}

void ContinuousSavingTileCallbackFactory::release()
{
    delete this;
}

ITileCallback* ContinuousSavingTileCallbackFactory::create()
{
    return m_callback.get();
}

}   // namespace cli
}   // namespace appleseed
