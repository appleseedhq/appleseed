
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "frozendisplayrenderer.h"

// appleseed.studio headers.
#include "mainwindow/rendering/renderwidget.h"

// appleseed.renderer headers.
#include "renderer/api/aov.h"
#include "renderer/api/camera.h"
#include "renderer/api/frame.h"
#include "renderer/api/trace.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/platform/types.h"

// Qt headers.
#include <QImage>
#include <QMutexLocker>

// Standard headers.
#include <cstddef>
#include <cstring>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

FrozenDisplayRenderer::FrozenDisplayRenderer(
    const Camera&   camera,
    const Frame&    frame,
    RenderWidget&   render_widget)
  : m_camera(camera)
  , m_frame(frame)
  , m_frame_props(frame.image().properties())
  , m_color_image(frame.image())
  , m_depth_image(frame.aov_images().get_image(frame.aov_images().get_index("depth")))
  , m_render_widget(render_widget)
{
    const QImage& dest_image = m_render_widget.image();
    m_zbuffer.resize(dest_image.width() * dest_image.height(), 0.0f);
    m_points.resize(m_frame_props.m_pixel_count);
}

namespace
{
    // todo: factorize with code in utility/interop.h and foundation/image/pixel.h.
    Color3b rgb32f_to_color3b(const float r, const float g, const float b)
    {
        return
            Color3b(
                truncate<uint8>(clamp(r * 256.0f, 0.0f, 255.0f)),
                truncate<uint8>(clamp(g * 256.0f, 0.0f, 255.0f)),
                truncate<uint8>(clamp(b * 256.0f, 0.0f, 255.0f)));
    }
}

void FrozenDisplayRenderer::capture()
{
    MersenneTwister rng;
    SamplingContext sampling_context(
        rng,
        2,      // number of dimensions
        0,      // number of samples -- unknown
        0);     // initial instance number

    size_t point_index = 0;

    for (size_t ty = 0; ty < m_frame_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < m_frame_props.m_tile_count_x; ++tx)
        {
            const Tile& color_tile = m_color_image.tile(tx, ty);
            const Tile& depth_tile = m_depth_image.tile(tx, ty);
            const size_t tile_width = color_tile.get_width();
            const size_t tile_height = color_tile.get_height();

            for (size_t py = 0; py < tile_height; ++py)
            {
                for (size_t px = 0; px < tile_width; ++px)
                {
                    // Compute film point in NDC.
                    const Vector2d point =
                        m_frame.get_sample_position(
                            tx, ty,
                            px, py,
                            0.5, 0.5);

                    // Generate a world space ray going through that film point.
                    ShadingRay ray;
                    m_camera.generate_ray(sampling_context, point, ray);

                    // Retrieve pixel color.
                    Color4f pixel;
                    color_tile.get_pixel(px, py, pixel);

                    // todo: hack.
                    pixel.rgb() = fast_linear_rgb_to_srgb(pixel.rgb());

                    // Retrieve pixel depth.
                    const float depth = depth_tile.get_component<float>(px, py, 0);

                    if (depth >= 0.0f)
                    {
                        // Compute and store world space point and color.
                        RenderPoint point;
                        point.m_position = Vector3f(ray.point_at(depth));
                        point.m_color = rgb32f_to_color3b(pixel[0], pixel[1], pixel[2]);
                        m_points[point_index++] = point;
                    }
                }
            }
        }
    }
}

void FrozenDisplayRenderer::render()
{
    // Get the camera transform.
    Transformd tmp;
    const Transformd& camera_transform = m_camera.transform_sequence().evaluate(0.0, tmp);

    // Get exclusive access to the render widget.
    QMutexLocker render_widget_locker(&m_render_widget.mutex());

    // Retrieve destination image and associated information.
    QImage& dest_image = m_render_widget.image();
    const int image_width = dest_image.width();
    const int image_height = dest_image.height();
    const size_t dest_stride = static_cast<size_t>(dest_image.bytesPerLine());

    // Retrieve pointers to the color and depth buffers.
    uint8* pixels = static_cast<uint8*>(dest_image.scanLine(0));
    float* zbuffer = &m_zbuffer[0];

    // Clear image.
    for (size_t y = 0; y < image_height; ++y)
        memset(pixels + y * dest_stride, 0, dest_stride);

    // Clear Z-buffer.
    for (size_t i = 0; i < image_width * image_height; ++i)
        zbuffer[i] = -1.0e38f;

    const size_t point_count = m_points.size();
    for (size_t i = 0; i < point_count; ++i)
    {
        const RenderPoint& point = m_points[i];

        // Transform point to camera space.
        const Vector3f point_camera = camera_transform.point_to_local(point.m_position);

        // Project point to film plane.
        Vector2d ndc;
        m_camera.project_camera_space_point(Vector3d(point_camera), ndc);

        // Compute point coordinates in pixels.
        const int ix = truncate<int>(ndc.x * image_width);
        const int iy = truncate<int>(ndc.y * image_height);

        // Reject points outside the frame.
        if (ix < 0 ||
            iy < 0 ||
            ix >= image_width ||
            iy >= image_height)
            continue;

        // Check point depth against Z-buffer.
        const size_t zbuffer_index = iy * image_width + ix;
        if (point_camera.z <= zbuffer[zbuffer_index])
            continue;

        // Write to Z-buffer.
        zbuffer[zbuffer_index] = point_camera.z;

        // Draw point on render widget.
        const size_t base = iy * dest_stride + ix * 3;
        pixels[base + 0] = point.m_color[0];
        pixels[base + 1] = point.m_color[1];
        pixels[base + 2] = point.m_color[2];
    }
}

}   // namespace studio
}   // namespace appleseed
