
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
#include "continuoussavingtilecallback.h"

// appleseed.cli headers.
#include "progresstilecallback.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/progressiveexrimagefilewriter.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/random/mersenne_twister.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/uuid/random_generator.hpp"
#include "boost/uuid/uuid.hpp"
#include "boost/uuid/uuid_io.hpp"

// Standard headers.
#include <cstddef>
#include <ctime>

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
        ContinuousSavingTileCallback(const string& output_path, Logger& logger)
          : ProgressTileCallback(logger)
          , m_output_path(output_path)
        {
            mt19937 rng(static_cast<uint32_t>(time(0)));
            const uuids::uuid u = uuids::basic_random_generator<boost::mt19937>(&rng)();
            const filesystem::path ext = m_output_path.extension();
            const string tmp_filename = uuids::to_string(u) + ext.string();

            m_tmp_output_path = m_output_path.parent_path() / tmp_filename;
        }

      private:
        mutex               m_mutex;
        filesystem::path    m_output_path;
        filesystem::path    m_tmp_output_path;
        bool                m_write_tiled_image;

        virtual void do_post_render_tile(
            const Frame*    frame,
            const size_t    tile_x,
            const size_t    tile_y) APPLESEED_OVERRIDE
        {
            mutex::scoped_lock lock(m_mutex);
            ProgressTileCallback::do_post_render_tile(frame, tile_x, tile_y);
            frame->write_main_image(m_tmp_output_path.string().c_str());
            filesystem::rename(m_tmp_output_path, m_output_path);
        }
    };
}


//
// ContinuousSavingTileCallbackFactory class implementation.
//

ContinuousSavingTileCallbackFactory::ContinuousSavingTileCallbackFactory(
    const string&   output_path,
    Logger&         logger)
  : m_callback(new ContinuousSavingTileCallback(output_path, logger))
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
