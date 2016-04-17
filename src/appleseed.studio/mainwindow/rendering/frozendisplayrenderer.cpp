
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

// Interface header.
#include "frozendisplayrenderer.h"

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
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

FrozenDisplayRenderer::FrozenDisplayRenderer(
    const SamplingContext::Mode sampling_mode,
    const Camera&               camera,
    const Frame&                frame)
  : m_sampling_mode(sampling_mode)
  , m_camera(camera)
  , m_frame(frame)
  , m_frame_props(frame.image().properties())
  , m_color_image(frame.image())
  , m_depth_image(frame.aov_images().get_image(frame.aov_images().get_index("depth")))
  , m_temp_image(m_frame_props.m_pixel_count * 4)
  , m_camera_transform(camera.transform_sequence().get_earliest_transform())
  , m_points(m_frame_props.m_pixel_count)
{
}

void FrozenDisplayRenderer::capture()
{
    SamplingContext::RNGType rng;
    SamplingContext sampling_context(
        rng,
        m_sampling_mode,
        2,                  // number of dimensions
        0,                  // number of samples -- unknown
        0);                 // initial instance number

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
                    const Vector2d ndc =
                        m_frame.get_sample_position(
                            tx, ty,
                            px, py,
                            0.5, 0.5);

                    // Retrieve pixel depth.
                    const float depth = depth_tile.get_component<float>(px, py, 0);

                    if (depth >= 0.0f)
                    {
                        // Generate a world space ray going through that film point.
                        ShadingRay ray;
                        m_camera.spawn_ray(sampling_context, Dual2d(ndc), ray);

                        // Retrieve pixel color.
                        Color4f pixel;
                        color_tile.get_pixel(px, py, pixel);

                        // Compute and store world space point and color.
                        RenderPoint point;
                        point.m_position = Vector3f(ray.point_at(depth));
                        point.m_color = pixel.rgb();
                        m_points[point_index++] = point;
                    }
                }
            }
        }
    }
}

void FrozenDisplayRenderer::set_camera_transform(const Transformd& transform)
{
    boost::mutex::scoped_lock lock(m_camera_transform_mutex);
    m_camera_transform = transform;
}

void FrozenDisplayRenderer::render()
{
    // Make a local copy of the camera transform to prevent update() from blocking.
    Transformd camera_transform_copy;
    {
        boost::mutex::scoped_lock lock(m_camera_transform_mutex);
        camera_transform_copy = m_camera_transform;
    }

    // Retrieve pointer to temporary image pixels.
    float* temp_pixels = &m_temp_image[0];

    // Clear temporary image.
    for (size_t i = 0; i < m_frame_props.m_pixel_count * 4; i += 4)
    {
        temp_pixels[i + 0] = 0.0f;
        temp_pixels[i + 1] = 0.0f;
        temp_pixels[i + 2] = 0.0f;
        temp_pixels[i + 3] = -numeric_limits<float>::max();
    }

    const size_t point_count = m_points.size();
    for (size_t i = 0; i < point_count; ++i)
    {
        const RenderPoint& point = m_points[i];

        // Transform point to camera space.
        const Vector3f point_camera = camera_transform_copy.point_to_local(point.m_position);

        // Project point to film plane.
        Vector2d ndc;
        if (!m_camera.project_camera_space_point(Vector3d(point_camera), ndc))
            continue;

        // Compute point coordinates in pixels.
        const int ix = truncate<int>(ndc.x * m_frame_props.m_canvas_width);
        const int iy = truncate<int>(ndc.y * m_frame_props.m_canvas_height);

        // Reject points outside the frame.
        if (ix < 0 ||
            iy < 0 ||
            ix >= m_frame_props.m_canvas_width ||
            iy >= m_frame_props.m_canvas_height)
            continue;

        const size_t pixel_index = (iy * m_frame_props.m_canvas_width + ix) * 4;

        // Check point depth against Z-buffer.
        if (point_camera.z <= temp_pixels[pixel_index + 3])
            continue;

        // Set point in temporary image.
        temp_pixels[pixel_index + 0] = point.m_color[0];
        temp_pixels[pixel_index + 1] = point.m_color[1];
        temp_pixels[pixel_index + 2] = point.m_color[2];
        temp_pixels[pixel_index + 3] = point_camera.z;
    }

    // Copy the temporary image to the frame.
    for (size_t ty = 0; ty < m_frame_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < m_frame_props.m_tile_count_x; ++tx)
        {
            Tile& color_tile = m_color_image.tile(tx, ty);
            const size_t tile_width = color_tile.get_width();
            const size_t tile_height = color_tile.get_height();

            for (size_t py = 0; py < tile_height; ++py)
            {
                for (size_t px = 0; px < tile_width; ++px)
                {
                    const size_t ix = tx * m_frame_props.m_tile_width + px;
                    const size_t iy = ty * m_frame_props.m_tile_height + py;

                    assert(ix < m_frame_props.m_canvas_width);
                    assert(iy < m_frame_props.m_canvas_height);

                    const size_t pixel_index = (iy * m_frame_props.m_canvas_width + ix) * 4;

                    // The third channel becomes an alpha channel.
                    temp_pixels[pixel_index + 3] = 1.0f;

                    color_tile.set_pixel<float>(px, py, temp_pixels + pixel_index);
                }
            }
        }
    }
}

}   // namespace studio
}   // namespace appleseed
