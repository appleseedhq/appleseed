
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

#ifndef APPLESEED_CLI_PROGRESSTILECALLBACK_H
#define APPLESEED_CLI_PROGRESSTILECALLBACK_H

// appleseed.renderer headers.
#include "renderer/api/rendering.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace foundation    { class Logger; }
namespace renderer      { class Frame; }

namespace appleseed {
namespace cli {

class ProgressTileCallback
  : public renderer::TileCallbackBase
{
  public:
    explicit ProgressTileCallback(foundation::Logger& logger);

    virtual void release() APPLESEED_OVERRIDE;

    virtual void post_render_tile(
        const renderer::Frame*  frame,
        const size_t            tile_x,
        const size_t            tile_y) APPLESEED_OVERRIDE;

  protected:
    foundation::Logger&         m_logger;

    virtual void do_post_render_tile(
        const renderer::Frame*  frame,
        const size_t            tile_x,
        const size_t            tile_y);

  private:
    boost::mutex                m_mutex;
    size_t                      m_rendered_pixels;
};

class ProgressTileCallbackFactory
  : public renderer::ITileCallbackFactory
{
  public:
    explicit ProgressTileCallbackFactory(foundation::Logger& logger);

    virtual void release() APPLESEED_OVERRIDE;

    virtual renderer::ITileCallback* create() APPLESEED_OVERRIDE;

  private:
    std::auto_ptr<renderer::ITileCallback> m_callback;
};

}       // namespace cli
}       // namespace appleseed

#endif  // !APPLESEED_CLI_PROGRESSTILECALLBACK_H
