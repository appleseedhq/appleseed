
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

#pragma once

// appleseed.studio headers.
#include "mainwindow/rendering/renderclipboardhandler.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/vector.h"

// OpenColorIO headers.
#include <OpenColorIO/OpenColorIO.h>
namespace OCIO = OCIO_NAMESPACE;

// Qt headers.
#include <QImage>
#include <QMutex>
#include <QPainter>
#include <QWidget>

// Standard headers.
#include <cstddef>
#include <memory>
#include <string>

// Forward declarations.
namespace foundation    { class CanvasProperties; }
namespace renderer      { class Frame; }
namespace renderer      { class Project; }

class QDragEnterEvent;
class QDropEvent;
class QPaintEvent;

namespace appleseed {
namespace studio {

//
// A render widget based on QImage.
//

class RenderWidget
  : public QWidget
  , public ICapturableWidget
{
    Q_OBJECT

  public:
    // Constructor.
    RenderWidget(
        const renderer::Project&    project,
        const size_t                width,
        const size_t                height,
        OCIO::ConstConfigRcPtr      ocio_config,
        QWidget*                    parent = nullptr);

    // Thread-safe.
    QImage capture() override;

    // Thread-safe.
    void resize(
        const size_t            width,
        const size_t            height);

    // Thread-safe.
    void clear();

    // Called before rendering begins.
    void start_render();

    // Thread-safe.
    void multiply(const float multiplier);

    // Thread-safe.
    void highlight_tile(
        const renderer::Frame&  frame,
        const size_t            tile_x,
        const size_t            tile_y);

    // Thread-safe.
    void blit_tile(
        const renderer::Frame&  frame,
        const size_t            tile_x,
        const size_t            tile_y);

    // Thread-safe.
    void blit_frame(const renderer::Frame& frame);

    // Direct access to internals for high-performance drawing.
    QMutex& mutex();
    QImage& image();

  signals:
    void signal_material_dropped(
        const foundation::Vector2d& drop_pos,
        const std::string&          material_name);

  public slots:
    void slot_display_transform_changed(const QString& transform);

  private:
    const renderer::Project&            m_project;

    mutable QMutex                      m_mutex;
    QImage                              m_image;
    QPainter                            m_painter;
    std::unique_ptr<foundation::Tile>   m_float_tile_storage;
    std::unique_ptr<foundation::Tile>   m_uint8_tile_storage;
    std::unique_ptr<foundation::Image>  m_image_storage;

    OCIO::ConstConfigRcPtr              m_ocio_config;
    OCIO::ConstProcessorRcPtr           m_ocio_processor;

    void allocate_working_storage(const foundation::CanvasProperties& frame_props);

    void blit_tile_no_lock(
        const renderer::Frame&  frame,
        const size_t            tile_x,
        const size_t            tile_y);

    void update_tile_no_lock(
        const size_t            tile_x,
        const size_t            tile_y);

    void paintEvent(QPaintEvent* event) override;
    void dragEnterEvent(QDragEnterEvent* event) override;
    void dragMoveEvent(QDragMoveEvent* event) override;
    void dropEvent(QDropEvent* event) override;
};


//
// RenderWidget class implementation.
//

inline QMutex& RenderWidget::mutex()
{
    return m_mutex;
}

inline QImage& RenderWidget::image()
{
    return m_image;
}

}   // namespace studio
}   // namespace appleseed
