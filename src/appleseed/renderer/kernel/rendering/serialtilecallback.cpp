
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
#include "serialtilecallback.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/serialrenderercontroller.h"

// Standard headers.
#include <cstddef>

namespace renderer
{

namespace
{
    class SerialTileCallback
      : public ITileCallback
    {
      public:
        explicit SerialTileCallback(SerialRendererController* controller)
          : m_controller(controller)
        {
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual void pre_render(
            const size_t     x,
            const size_t     y,
            const size_t     width,
            const size_t     height) APPLESEED_OVERRIDE
        {
            m_controller->add_pre_render_tile_callback(x, y, width, height);
        }

        virtual void post_render_tile(
            const Frame*    frame,
            const size_t    tile_x,
            const size_t    tile_y) APPLESEED_OVERRIDE
        {
            m_controller->add_post_render_tile_callback(frame, tile_x, tile_y);
        }

        virtual void post_render(const Frame* frame) APPLESEED_OVERRIDE
        {
            m_controller->add_post_render_tile_callback(frame);
        }

      private:
        SerialRendererController* m_controller;
    };
}


//
// SerialTileCallbackFactory class implementation.
//

SerialTileCallbackFactory::SerialTileCallbackFactory(SerialRendererController* controller)
  : m_controller(controller)
{
}

void SerialTileCallbackFactory::release()
{
    delete this;
}

ITileCallback* SerialTileCallbackFactory::create()
{
    return new SerialTileCallback(m_controller);
}

}   // namespace renderer
