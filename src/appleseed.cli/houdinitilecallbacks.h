
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Hans Hoogenboom, Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_CLI_HOUDINITILECALLBACKS_H
#define APPLESEED_CLI_HOUDINITILECALLBACKS_H

// appleseed.renderer headers.
#include "renderer/api/rendering.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Standard headers.
#include <memory>

// Forward declarations.
namespace foundation    { class Logger; }

namespace appleseed {
namespace cli {

class MPlayTileCallbackFactory
  : public renderer::ITileCallbackFactory
{
  public:
    MPlayTileCallbackFactory(
        const char*         scene_name,
        bool                progressive_mode,
        foundation::Logger& logger);

    virtual void release() APPLESEED_OVERRIDE;

    virtual renderer::ITileCallback* create() APPLESEED_OVERRIDE;

  private:
    std::auto_ptr<renderer::ITileCallback> m_callback;
};

class HRmanPipeTileCallbackFactory
  : public renderer::ITileCallbackFactory
{
  public:
    HRmanPipeTileCallbackFactory(
        int                 socket_number,
        bool                progressive_mode,
        foundation::Logger& logger);

    virtual void release() APPLESEED_OVERRIDE;

    virtual renderer::ITileCallback* create() APPLESEED_OVERRIDE;

  private:
    std::auto_ptr<renderer::ITileCallback> m_callback;
};

}       // namespace cli
}       // namespace appleseed

#endif  // !APPLESEED_CLI_HOUDINITILECALLBACKS_H
