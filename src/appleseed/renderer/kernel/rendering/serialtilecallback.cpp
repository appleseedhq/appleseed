
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
#include "serialtilecallback.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/serialrenderercontroller.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

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

        void release() override
        {
            delete this;
        }

        void on_tiled_frame_begin(const Frame* frame) override
        {
            m_controller->add_on_tiled_frame_begin_callback(frame);
        }

        void on_tiled_frame_end(const Frame* frame) override
        {
            m_controller->add_on_tiled_frame_end_callback(frame);
        }

        void on_tile_begin(
            const Frame*            frame,
            const size_t            tile_x,
            const size_t            tile_y,
            const size_t            thread_index,
            const size_t            thread_count) override
        {
            m_controller->add_on_tile_begin_callback(frame, tile_x, tile_y, thread_index, thread_count);
        }

        void on_tile_end(
            const Frame*            frame,
            const size_t            tile_x,
            const size_t            tile_y) override
        {
            m_controller->add_on_tile_end_callback(frame, tile_x, tile_y);
        }

        void on_progressive_frame_update(
            const Frame&            frame,
            const double            time,
            const std::uint64_t     samples,
            const double            samples_per_pixel,
            const std::uint64_t     samples_per_second) override
        {
            m_controller->add_on_progressive_frame_update_callback(
                frame,
                time,
                samples,
                samples_per_pixel,
                samples_per_second);
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
