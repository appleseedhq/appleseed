
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

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace renderer;
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
          , m_output_filename(output_filename)
          , m_exr_writer(&logger)
        {
        }

        ~ContinuousSavingTileCallback()
        {
            m_exr_writer.close();
        }

      private:
        const string                    m_output_filename;
        ProgressiveEXRImageFileWriter   m_exr_writer;
        size_t                          m_tile_count_x;
        size_t                          m_tile_count_y;
        vector<size_t>                  m_tiles_done;

        virtual void do_post_render_tile(
            const Frame*    frame,
            const size_t    tile_x,
            const size_t    tile_y) OVERRIDE
        {
            ProgressTileCallback::do_post_render_tile(frame, tile_x, tile_y);

            const CanvasProperties& props = frame->image().properties();

            if (!m_exr_writer.is_open())
            {
                m_exr_writer.open(
                    m_output_filename.c_str(),
                    props,
                    ImageAttributes::create_default_attributes());

                m_tile_count_x = props.m_tile_count_x;
                m_tile_count_y = props.m_tile_count_y;
                m_tiles_done.resize(m_tile_count_y, 0);
            }

            m_tiles_done[tile_y] += 1;

            for( size_t j = 0; j < m_tile_count_y; ++j)
            {
                if (m_tiles_done[j] == m_tile_count_x)
                {
                    for (size_t i = 0; i < m_tile_count_x; ++i)
                    {
                        const Tile& tile = frame->image().tile(i, j);
                        m_exr_writer.write_tile(tile, i, j);
                    }

                    m_tiles_done[j] = 0;
                }
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
