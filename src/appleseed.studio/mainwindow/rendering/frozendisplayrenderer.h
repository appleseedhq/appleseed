
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_RENDERING_FROZENDISPLAYRENDERER_H
#define APPLESEED_STUDIO_MAINWINDOW_RENDERING_FROZENDISPLAYRENDERER_H

// appleseed.renderer headers.
#include "renderer/api/types.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/thread.h"

// Standard headers.
#include <vector>

// Forward declarations.
namespace foundation    { class CanvasProperties; }
namespace foundation    { class Image; }
namespace renderer      { class Camera; }
namespace renderer      { class Frame; }

namespace appleseed {
namespace studio {

class FrozenDisplayRenderer
{
  public:
    FrozenDisplayRenderer(
        const renderer::SamplingContext::Mode   sampling_mode,
        const renderer::Camera&                 camera,
        const renderer::Frame&                  frame);

    // Capture the frame as a point cloud.
    void capture();

    // Set the new camera transform. Thread-safe.
    void set_camera_transform(
        const foundation::Transformd&           transform);

    // Render the point cloud to the frame.
    void render();

  private:
    const renderer::SamplingContext::Mode       m_sampling_mode;
    const renderer::Camera&                     m_camera;
    const renderer::Frame&                      m_frame;
    const foundation::CanvasProperties&         m_frame_props;

    foundation::Image&                          m_color_image;
    foundation::Image&                          m_depth_image;
    std::vector<float>                          m_temp_image;

    foundation::Transformd                      m_camera_transform;
    boost::mutex                                m_camera_transform_mutex;

    struct RenderPoint
    {
        foundation::Vector3f    m_position;
        foundation::Color3f     m_color;
    };

    std::vector<RenderPoint>                    m_points;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERING_FROZENDISPLAYRENDERER_H
