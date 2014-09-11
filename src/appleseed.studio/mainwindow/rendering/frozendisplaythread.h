
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_RENDERING_FROZENDISPLAYTHREAD_H
#define APPLESEED_STUDIO_MAINWINDOW_RENDERING_FROZENDISPLAYTHREAD_H

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/rng.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/job/abortswitch.h"

// Qt headers.
#include <QMutex>
#include <QThread>

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace appleseed     { namespace studio { class RenderWidget; } }
namespace foundation    { class CanvasProperties; }
namespace foundation    { class Image; }
namespace renderer      { class Camera; }
namespace renderer      { class Frame; }

namespace appleseed {
namespace studio {

class FrozenDisplayThread
  : public QThread
{
  public:
    FrozenDisplayThread(
        const renderer::Camera&         camera,
        const renderer::Frame&          frame,
        RenderWidget&                   render_widget);

    void update_camera_transform();

    void abort();

  private:
    const renderer::Camera&             m_camera;
    const renderer::Frame&              m_frame;
    const foundation::CanvasProperties& m_frame_props;
    foundation::Image&                  m_color_image;
    foundation::Image&                  m_depth_image;
    RenderWidget&                       m_render_widget;
    foundation::AbortSwitch             m_abort_switch;
    foundation::Transformd              m_camera_transform;
    QMutex                              m_camera_mutex;

    struct RenderPoint
    {
        foundation::Vector3f    m_position;
        foundation::Color3b     m_color;
    };

    std::vector<RenderPoint>            m_points;
    size_t                              m_point_count;
    size_t                              m_point_index;

    std::vector<float>                  m_zbuffer;

    foundation::MersenneTwister         m_rng;

    virtual void run() OVERRIDE;

    void add_points(const size_t spacing);
    void render_points();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERING_FROZENDISPLAYTHREAD_H
