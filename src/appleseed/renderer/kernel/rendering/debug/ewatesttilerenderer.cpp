
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

// Interface header.
#include "ewatesttilerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/atomkraft/ewa.h"
#include "renderer/kernel/atomkraft/textureobject.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>

// Forward declarations.
namespace foundation    { class AbortSwitch; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    class EWATestTileRenderer
      : public ITileRenderer
    {
      public:
        EWATestTileRenderer(
            const Scene&            scene,
            const TraceContext&     trace_context,
            TextureStore&           texture_store,
            const ParamArray&       params)
          : m_scene(scene)
          , m_texture_cache(texture_store)
          , m_intersector(trace_context, m_texture_cache)
          , m_texture_width(params.get_optional<size_t>("texture_width", 256))
          , m_texture_height(params.get_optional<size_t>("texture_height", 256))
          , m_checkerboard_scale(params.get_optional<size_t>("checkerboard_scale", 16))
          , m_max_radius(params.get_optional<float>("max_radius", 16.0f))
        {
            Image texture(
                m_texture_width,
                m_texture_height,
                m_texture_width,
                m_texture_height,
                4,
                PixelFormatFloat);

            draw_checkerboard(texture, m_checkerboard_scale);

            m_texture_object.reset(new TextureObject(texture));
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual void render_tile(
            const Frame&            frame,
            const size_t            tile_x,
            const size_t            tile_y,
            AbortSwitch&            abort_switch) OVERRIDE
        {
            Image& image = frame.image();

            assert(tile_x < image.properties().m_tile_count_x);
            assert(tile_y < image.properties().m_tile_count_y);

            Tile& tile = image.tile(tile_x, tile_y);
            const size_t tile_width = tile.get_width();
            const size_t tile_height = tile.get_height();

            for  (size_t py = 0; py < tile_height; ++py)
            {
                for  (size_t px = 0; px < tile_width; ++px)
                {
                    const size_t ix = tile_x * image.properties().m_tile_width + px;
                    const size_t iy = tile_y * image.properties().m_tile_height + py;
                    const size_t instance = iy * image.properties().m_canvas_width + ix;

                    SamplingContext sampling_context(
                        m_rng,
                        0,              // number of dimensions
                        0,              // number of samples
                        instance);      // initial instance number

                    //
                    // We're lazy so we just cast three rays: one at the center of the pixel
                    // and two rays offset by half a pixel in the X and Y directions.
                    //

                    const Vector2d sample_center = frame.get_sample_position(tile_x, tile_y, px, py, 0.5, 0.5);
                    const Vector2d sample_dx = frame.get_sample_position(tile_x, tile_y, px, py, 1.0, 0.5);
                    const Vector2d sample_dy = frame.get_sample_position(tile_x, tile_y, px, py, 0.5, 1.0);

                    ShadingRay ray_center, ray_dx, ray_dy;
                    m_scene.get_camera()->generate_ray(sampling_context, sample_center, ray_center);
                    m_scene.get_camera()->generate_ray(sampling_context, sample_dx, ray_dx);
                    m_scene.get_camera()->generate_ray(sampling_context, sample_dy, ray_dy);

                    ShadingPoint hit_center, hit_dx, hit_dy;
                    m_intersector.trace(ray_center, hit_center);
                    m_intersector.trace(ray_dx, hit_dx);
                    m_intersector.trace(ray_dy, hit_dy);

                    if (hit_center.hit() && hit_dx.hit() && hit_dy.hit())
                    {
                        const Vector2d& uv_center = hit_center.get_uv(0);
                        const Vector2d& uv_dx = hit_dx.get_uv(0);
                        const Vector2d& uv_dy = hit_dy.get_uv(0);

                        // Multiply by 2 since dx and dy are half a pixel.
                        const double dudx = 2.0 * (uv_dx.x - uv_center.x);
                        const double dudy = 2.0 * (uv_dy.x - uv_center.x);
                        const double dvdx = 2.0 * (uv_dx.y - uv_center.y);
                        const double dvdy = 2.0 * (uv_dy.y - uv_center.y);

                        Color4f result;

                        m_filter.filter(
                            *m_texture_object.get(),
                            static_cast<float>(uv_center.x * m_texture_width),
                            static_cast<float>(uv_center.y * m_texture_height),
                            static_cast<float>(dudx * m_texture_width),
                            static_cast<float>(dudy * m_texture_height),
                            static_cast<float>(dvdx * m_texture_width),
                            static_cast<float>(dvdy * m_texture_height),
                            m_max_radius,
                            &result[0]);

                        tile.set_pixel(px, py, result);
                    }
#if 0
                    else if (hit_center.hit() || hit_dx.hit() || hit_dy.hit())
                    {
                        tile.set_pixel(px, py, Color4f(1.0f, 0.0f, 0.0f, 1.0f));
                    }
#endif
                    else
                    {
                        tile.set_pixel(px, py, Color4f(0.0f, 0.0f, 0.0f, 1.0f));
                    }
                }
            }
        }

        virtual StatisticsVector get_statistics() const OVERRIDE
        {
            return StatisticsVector();
        }

      private:
        const Scene&                        m_scene;
        TextureCache                        m_texture_cache;
        Intersector                         m_intersector;
        const size_t                        m_texture_width;
        const size_t                        m_texture_height;
        const size_t                        m_checkerboard_scale;
        const float                         m_max_radius;

        auto_ptr<TextureObject>             m_texture_object;
        ak::EWAFilter<4, TextureObject>     m_filter;
        SamplingContext::RNGType            m_rng;

        // todo: move to foundation/image/drawing.h.
        void draw_checkerboard(
            Image&          image,
            const size_t    scale = 16,
            const Color4f&  color1 = Color4f(0.0f, 0.0f, 0.0f, 1.0f),
            const Color4f&  color2 = Color4f(1.0f, 1.0f, 1.0f, 1.0f))
        {
            const size_t width = image.properties().m_canvas_width;
            const size_t height = image.properties().m_canvas_height;

            for (size_t y = 0; y < height; ++y)
            {
                for (size_t x = 0; x < width; ++x)
                {
                    const size_t b = ((x / scale) ^ (y / scale)) & 1;
                    image.set_pixel(x, y, b ? color1 : color2);
                }
            }
        }
    };
}


//
// EWATestTileRendererFactory class implementation.
//

EWATestTileRendererFactory::EWATestTileRendererFactory(
    const Scene&            scene,
    const TraceContext&     trace_context,
    TextureStore&           texture_store,
    const ParamArray&       params)
  : m_scene(scene)
  , m_trace_context(trace_context)
  , m_texture_store(texture_store)
  , m_params(params)
{
}

void EWATestTileRendererFactory::release()
{
    delete this;
}

ITileRenderer* EWATestTileRendererFactory::create(const bool primary)
{
    return
        new EWATestTileRenderer(
            m_scene,
            m_trace_context,
            m_texture_store,
            m_params);
}

}   // namespace renderer
