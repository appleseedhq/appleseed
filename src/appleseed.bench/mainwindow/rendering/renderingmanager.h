
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

// appleseed.bench headers.
#include "mainwindow/rendering/qtrenderercontroller.h"

// appleseed.renderer headers.
#include "renderer/api/rendering.h"
#include "renderer/api/types.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/searchpaths.h"

// Qt headers.
#include <QObject>
#include <QtGlobal>
#include <QThread>

// Standard headers.
#include <cstdint>
#include <memory>

// Forward declarations.
namespace appleseed     { namespace bench { class RenderingTimeDisplay; } }
namespace appleseed     { namespace qtcommon { class RenderWidget; } }
namespace renderer      { class Project; }

namespace appleseed {
namespace bench {

class RenderingManager
  : public QObject
{
    Q_OBJECT

  public:
    enum class RenderingMode
    {
        InteractiveRendering,
        FinalRendering
    };

    // Constructor.
    explicit RenderingManager(RenderingTimeDisplay& rendering_time_display);

    // Start rendering.
    void start_rendering(
        renderer::Project*              project,
        const renderer::ParamArray&     params,
        const RenderingMode             rendering_mode,
        qtcommon::RenderWidget*         render_widget);

    // Return true if currently rendering, false otherwise.
    bool is_rendering() const;

    // Wait until rendering has ended.
    void wait_until_rendering_end();

    // Send orders to the renderer via the renderer controller.
    void abort_rendering();
    void restart_rendering();
    void reinitialize_rendering();

  signals:
    void signal_rendering_success();
    void signal_rendering_abort();
    void signal_progressive_frame_update(
        const double                    time,
        const quint64                   samples,
        const double                    samples_per_pixel,
        const quint64                   samples_per_second);


  public slots:
    void slot_abort_rendering();
    void slot_restart_rendering();
    void slot_reinitialize_rendering();

  private:
    RenderingTimeDisplay&                       m_rendering_time_display;
    QtRendererController                        m_renderer_controller;

    renderer::Project*                          m_project;
    renderer::ParamArray                        m_params;
    foundation::SearchPaths                     m_resource_search_paths;
    RenderingMode                               m_rendering_mode;
    qtcommon::RenderWidget*                     m_render_widget;

    std::unique_ptr<renderer::TileCallbackCollectionFactory>
                                                m_tile_callback_factory;
    std::unique_ptr<renderer::MasterRenderer>   m_master_renderer;
    std::unique_ptr<QThread>                    m_master_renderer_thread;

  private slots:
    void slot_rendering_begin();
    void slot_rendering_end();
    void slot_rendering_failed();
    void slot_frame_begin();
    void slot_frame_end();
    void slot_master_renderer_thread_finished();
};

}   // namespace bench
}   // namespace appleseed
