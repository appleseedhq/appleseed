
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
#include "qttilecallback.h"

// appleseed.qtcommon headers.
#include "widgets/renderwidget.h"

// Qt headers.
#include <QObject>
#include <Qt>

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace renderer  { class Frame; }

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

namespace
{
    class QtTileCallback
      : public QObject
      , public TileCallbackBase
    {
        Q_OBJECT

      public:
        explicit QtTileCallback(RenderWidget* render_widget)
          : m_render_widget(render_widget)
        {
            connect(
                this, &QtTileCallback::signal_update,
                m_render_widget, static_cast<void (QWidget::*)(void)>(&QWidget::update),
                Qt::QueuedConnection);
        }

        void release() override
        {
            delete this;
        }

        void on_tile_begin(
            const Frame*    frame,
            const size_t    tile_x,
            const size_t    tile_y,
            const size_t    thread_index,
            const size_t    thread_count) override
        {
            assert(m_render_widget);
            m_render_widget->highlight_tile(*frame, tile_x, tile_y, thread_index, thread_count);

            emit signal_update();
        }

        void on_tile_end(
            const Frame*    frame,
            const size_t    tile_x,
            const size_t    tile_y) override
        {
            assert(m_render_widget);
            m_render_widget->blit_tile(*frame, tile_x, tile_y);

            emit signal_update();
        }

        void on_progressive_frame_update(
            const Frame&            frame,
            const double            /*time*/,
            const std::uint64_t     /*samples*/,
            const double            /*samples_per_pixel*/,
            const std::uint64_t     /*samples_per_second*/) override
        {
            assert(m_render_widget);
            m_render_widget->blit_frame(frame);

            emit signal_update();
        }

      signals:
        void signal_update();

      private:
        RenderWidget* m_render_widget;
    };
}


//
// QtTileCallbackFactory class implementation.
//

QtTileCallbackFactory::QtTileCallbackFactory(RenderWidget* render_widget)
  : m_render_widget(render_widget)
{
}

void QtTileCallbackFactory::release()
{
    delete this;
}

ITileCallback* QtTileCallbackFactory::create()
{
    return new QtTileCallback(m_render_widget);
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/rendering/moc_cpp_qttilecallback.cxx"
