
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Achal Pandey, The appleseedhq Organization
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
#include "tilecallbackcollection.h"

// Standard headers.
#include <cassert>
#include <cstdint>
#include <list>

using namespace foundation;

namespace renderer
{

namespace
{
    class TileCallbackCollection
      : public ITileCallback
    {
      public:
        explicit TileCallbackCollection(std::list<ITileCallbackFactory*> factories)
        {
            for (ITileCallbackFactory* factory : factories)
                m_callbacks.push_back(factory->create());
        }

        void release() override
        {
            for (ITileCallback* callback : m_callbacks)
                callback->release();
        }

        void on_tiled_frame_begin(const Frame* frame) override
        {
            for (ITileCallback* callback : m_callbacks)
                callback->on_tiled_frame_begin(frame);
        }

        void on_tiled_frame_end(const Frame* frame) override
        {
            for (ITileCallback* callback : m_callbacks)
                callback->on_tiled_frame_end(frame);
        }

        void on_tile_begin(
            const Frame*            frame,
            const size_t            tile_x,
            const size_t            tile_y,
            const size_t            thread_index,
            const size_t            thread_count) override
        {
            for (ITileCallback* callback : m_callbacks)
                callback->on_tile_begin(frame, tile_x, tile_y, thread_index, thread_count);
        }
        
        void on_tile_end(
            const Frame*            frame,
            const size_t            tile_x,
            const size_t            tile_y) override
        {
            for (ITileCallback* callback : m_callbacks)
                callback->on_tile_end(frame, tile_x, tile_y);
        }

        void on_progressive_frame_update(
            const Frame&            frame,
            const double            time,
            const std::uint64_t     samples,
            const double            samples_per_pixel,
            const std::uint64_t     samples_per_second) override
        {
            for (ITileCallback* callback : m_callbacks)
            {
                callback->on_progressive_frame_update(
                    frame,
                    time,
                    samples,
                    samples_per_pixel,
                    samples_per_second);
            }
        }

      private:
        std::list<ITileCallback*> m_callbacks;
    };
}


//
// TileCallbackCollectionFactory implementation.
//

struct TileCallbackCollectionFactory::Impl
{
    typedef std::list<ITileCallbackFactory*> TileCallbackFactoryContainer;

    TileCallbackFactoryContainer m_factories;
};


TileCallbackCollectionFactory::TileCallbackCollectionFactory()
  : impl(new Impl())
{
}

TileCallbackCollectionFactory::~TileCallbackCollectionFactory()
{
    delete impl;
}

void TileCallbackCollectionFactory::release()
{
    delete this;
}

ITileCallback* TileCallbackCollectionFactory::create()
{
    return new TileCallbackCollection(impl->m_factories);
}

void TileCallbackCollectionFactory::insert(ITileCallbackFactory* factory)
{
    impl->m_factories.push_back(factory);
}

}   // namespace renderer
