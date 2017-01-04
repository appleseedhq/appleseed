
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

// Interface header.
#include "qttilecallback.h"

// appleseed.studio headers.
#include "mainwindow/rendering/renderwidget.h"

// Qt headers.
#include <QObject>
#include <Qt>

// Standard headers.
#include <cassert>
#include <cstddef>

// Forward declarations.
namespace renderer  { class Frame; }

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
                this, SIGNAL(signal_update()),
                m_render_widget, SLOT(update()),
                Qt::QueuedConnection);
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual void pre_render(
            const size_t    x,
            const size_t    y,
            const size_t    width,
            const size_t    height) APPLESEED_OVERRIDE
        {
            assert(m_render_widget);
            m_render_widget->highlight_region(x, y, width, height);
            emit signal_update();
        }

        virtual void post_render_tile(
            const Frame*    frame,
            const size_t    tile_x,
            const size_t    tile_y) APPLESEED_OVERRIDE
        {
            assert(m_render_widget);
            m_render_widget->blit_tile(*frame, tile_x, tile_y);
            emit signal_update();
        }

        virtual void post_render(
            const Frame*    frame) APPLESEED_OVERRIDE
        {
            assert(m_render_widget);
            m_render_widget->blit_frame(*frame);
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
