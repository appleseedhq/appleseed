
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

// appleseed.renderer headers.
#include "renderer/api/rendering.h"

// Qt headers.
#include <QObject>
#include <QtGlobal>

// Standard headers.
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace appleseed { namespace qtcommon { class RenderWidget; } }
namespace renderer  { class Frame; }

namespace appleseed {
namespace bench {

class QtTileCallback
  : public QObject
  , public renderer::TileCallbackBase
{
    Q_OBJECT

  public:
    explicit QtTileCallback(qtcommon::RenderWidget* render_widget);

    void release() override;

    void on_tile_begin(
        const renderer::Frame*  frame,
        const size_t            tile_x,
        const size_t            tile_y,
        const size_t            thread_index,
        const size_t            thread_count) override;

    void on_tile_end(
        const renderer::Frame*  frame,
        const size_t            tile_x,
        const size_t            tile_y) override;

    void on_progressive_frame_update(
        const renderer::Frame&  frame,
        const double            time,
        const std::uint64_t     samples,
        const double            samples_per_pixel,
        const std::uint64_t     samples_per_second) override;

  signals:
    void signal_update();
    void signal_progressive_frame_update(
        const double            time,
        const quint64           samples,
        const double            samples_per_pixel,
        const quint64           samples_per_second);

  private:
    qtcommon::RenderWidget* m_render_widget;
};

class QtTileCallbackFactory
  : public QObject
  , public renderer::ITileCallbackFactory
{
    Q_OBJECT

  public:
    // Constructor.
    explicit QtTileCallbackFactory(qtcommon::RenderWidget* render_widget);

    // Delete this instance.
    void release() override;

    // Return a new instance of the class.
    renderer::ITileCallback* create() override;

  signals:
    void signal_progressive_frame_update(
        const double            time,
        const quint64           samples,
        const double            samples_per_pixel,
        const quint64           samples_per_second);

  private:
    qtcommon::RenderWidget* m_render_widget;
};

}   // namespace bench
}   // namespace appleseed
