
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_RENDERING_RENDERWIDGET_H
#define APPLESEED_STUDIO_MAINWINDOW_RENDERING_RENDERWIDGET_H

// appleseed.studio headers.
#include "mainwindow/rendering/irenderwidget.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QImage>
#include <QMutex>
#include <QPainter>
#include <QWidget>

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace foundation    { class CanvasProperties; }
namespace foundation    { class Tile; }
namespace renderer      { class Frame; }
class QPaintEvent;

namespace appleseed {
namespace studio {

//
// A render widget based on QImage.
//

class RenderWidget
  : public QWidget
  , public IRenderWidget
{
  public:
    // Constructor.
    RenderWidget(
        const size_t                width,
        const size_t                height,
        QWidget*                    parent = 0);

    // Thread-safe.
    QImage get_image() const;

    // Thread-safe.
    virtual void resize(
        const size_t                width,
        const size_t                height) OVERRIDE;

    // Thread-safe.
    virtual void clear(const foundation::Color4f& color) OVERRIDE;

    // Thread-safe.
    virtual void multiply(const float multiplier) OVERRIDE;

    // Thread-safe.
    virtual void highlight_region(
        const size_t                x,
        const size_t                y,
        const size_t                width,
        const size_t                height) OVERRIDE;

    // Thread-safe.
    virtual void blit_tile(
        const renderer::Frame&      frame,
        const size_t                tile_x,
        const size_t                tile_y) OVERRIDE;

    // Thread-safe.
    virtual void blit_frame(
        const renderer::Frame&      frame) OVERRIDE;

  private:
    mutable QMutex  m_mutex;
    QImage          m_image;
    QPainter        m_painter;

    std::auto_ptr<foundation::Tile> m_float_tile_storage;
    std::auto_ptr<foundation::Tile> m_uint8_tile_storage;

    void allocate_working_storage(const foundation::CanvasProperties& frame_props);

    void blit_tile_no_lock(
        const renderer::Frame&      frame,
        const size_t                tile_x,
        const size_t                tile_y);

    virtual void paintEvent(QPaintEvent* event);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERING_RENDERWIDGET_H
