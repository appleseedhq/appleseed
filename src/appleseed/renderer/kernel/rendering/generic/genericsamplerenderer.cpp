
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

// Interface header.
#include "genericsamplerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingengine.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/kernel/texturing/oiiotexturesystem.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/vector.h"
#include "foundation/memory/arena.h"
#include "foundation/string/string.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>

// Forward declarations.
namespace renderer  { class PixelContext; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Generic sample renderer.
    //

    // If defined, the texture cache returns solid tiles whose color depends on whether
    // the requested tile could be found in the cache or not.
    #undef DEBUG_DISPLAY_TEXTURE_CACHE_PERFORMANCE

    class GenericSampleRenderer
      : public ISampleRenderer
    {
      public:
        GenericSampleRenderer(
            const Scene&            scene,
            const Frame&            frame,
            const TraceContext&     trace_context,
            TextureStore&           texture_store,
            ILightingEngineFactory* lighting_engine_factory,
            ShadingEngine&          shading_engine,
            OIIOTextureSystem&      oiio_texture_system,
            OSLShadingSystem&       shading_system,
            const size_t            thread_index,
            const ParamArray&       params)
          : m_params(params)
          , m_scene(scene)
          , m_opacity_threshold(1.0f - m_params.m_transparency_threshold)
          , m_texture_cache(texture_store)
          , m_lighting_engine(lighting_engine_factory->create())
          , m_shading_engine(shading_engine)
          , m_oiio_texture_system(oiio_texture_system)
          , m_thread_index(thread_index)
          , m_shadergroup_exec(shading_system, m_arena)
          , m_intersector(
                trace_context,
                m_texture_cache,
                m_params.m_report_self_intersections)
          , m_tracer(
                m_scene,
                m_intersector,
                m_shadergroup_exec,
                m_params.m_transparency_threshold,
                m_params.m_max_iterations,
                thread_index == 0)
          , m_shading_context(
                m_intersector,
                m_tracer,
                m_texture_cache,
                m_oiio_texture_system,
                m_shadergroup_exec,
                m_arena,
                m_thread_index,
                m_lighting_engine,
                m_params.m_transparency_threshold,
                m_params.m_max_iterations)
        {
            // 1/4 of a pixel, like in RenderMan RIS.
            const CanvasProperties& c = frame.image().properties();
            m_image_point_dx = Vector2d(1.0 / (2.0 * c.m_canvas_width), 0.0);
            m_image_point_dy = Vector2d(0.0, -1.0 / (2.0 * c.m_canvas_height));
        }

        ~GenericSampleRenderer() override
        {
            m_lighting_engine->release();
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
            RENDERER_LOG_INFO(
                "generic sample renderer settings:\n"
                "  transparency threshold        %f\n"
                "  max iterations                %s\n"
                "  report self intersections     %s",
                m_params.m_transparency_threshold,
                pretty_uint(m_params.m_max_iterations).c_str(),
                m_params.m_report_self_intersections ? "on" : "off");

            m_lighting_engine->print_settings();
        }

        void render_sample(
            SamplingContext&            sampling_context,
            const PixelContext&         pixel_context,
            const Vector2d&             image_point,
            AOVAccumulatorContainer&    aov_accumulators,
            ShadingResult&              shading_result) override
        {
#ifdef DEBUG_DISPLAY_TEXTURE_CACHE_PERFORMANCE

            const std::uint64_t last_texture_cache_hit_count = m_texture_cache.get_hit_count();
            const std::uint64_t last_texture_cache_miss_count = m_texture_cache.get_miss_count();

#endif

            // Construct a primary ray.
            ShadingRay primary_ray;
            m_scene.get_render_data().m_active_camera->spawn_ray(
                sampling_context,
                Dual2d(image_point, m_image_point_dx, m_image_point_dy),
                primary_ray);

            ShadingPoint shading_points[2];
            size_t shading_point_index = 0;
            const ShadingPoint* shading_point_ptr = nullptr;
            size_t iterations = 0;

            // Inform the AOV accumulators that we are about to render a sample.
            aov_accumulators.on_sample_begin(pixel_context);

            while (true)
            {
                // Put a hard limit on the number of iterations.
                if (++iterations >= m_params.m_max_iterations)
                {
                    RENDERER_LOG_WARNING(
                        "reached hard iteration limit (%s), breaking primary ray trace loop.",
                        pretty_int(m_params.m_max_iterations).c_str());
                    break;
                }

                m_arena.clear();

                // Trace the ray.
                shading_points[shading_point_index].clear();
                m_intersector.trace(
                    primary_ray,
                    shading_points[shading_point_index],
                    shading_point_ptr);

                // Update the pointers to the shading points.
                shading_point_ptr = &shading_points[shading_point_index];
                shading_point_index = 1 - shading_point_index;

                if (iterations == 1)
                {
                    // Shade the first intersection point along the ray.
                    const bool terminate_path =
                        m_shading_engine.shade(
                            sampling_context,
                            pixel_context,
                            m_shading_context,
                            *shading_point_ptr,
                            aov_accumulators,
                            shading_result);

                    if (terminate_path)
                        break;
                }
                else
                {
                    // Shade the next intersection point along the ray.
                    ShadingResult local_result(shading_result.m_aov_count);
                    const bool terminate_path =
                        m_shading_engine.shade(
                            sampling_context,
                            pixel_context,
                            m_shading_context,
                            *shading_point_ptr,
                            aov_accumulators,
                            local_result);

                    // Composite `shading_result` over `local_result`.
                    shading_result.composite_over(local_result);

                    if (terminate_path)
                        break;
                }

                // Stop once we hit the environment.
                if (!shading_point_ptr->hit_surface())
                    break;

                // Stop once we hit full opacity.
                if (shading_result.m_main.a > m_opacity_threshold)
                    break;

                // Move the ray origin to the intersection point.
                primary_ray.m_org = shading_point_ptr->get_point();
                if (primary_ray.m_has_differentials)
                {
                    const double t = shading_point_ptr->get_distance();
                    primary_ray.m_rx_org = primary_ray.m_rx_org + t * primary_ray.m_rx_dir;
                    primary_ray.m_ry_org = primary_ray.m_ry_org + t * primary_ray.m_ry_dir;
                }
            }

            // Inform the AOV accumulators that we are done rendering a sample.
            aov_accumulators.on_sample_end(pixel_context);

#ifdef DEBUG_DISPLAY_TEXTURE_CACHE_PERFORMANCE

            const std::uint64_t delta_hit_count = m_texture_cache.get_hit_count() - last_texture_cache_hit_count;
            const std::uint64_t delta_miss_count = m_texture_cache.get_miss_count() - last_texture_cache_miss_count;

            if (delta_hit_count + delta_miss_count == 0)
            {
                // In black: no access to the texture cache.
                shading_result.m_main = Color4f(0.0f, 0.0f, 0.0f, 1.0f);
            }
            else if (delta_hit_count > delta_miss_count)
            {
                // In green: a majority of cache hits.
                shading_result.m_main = Color4f(0.0f, 1.0f, 0.0f, 1.0f);
            }
            else
            {
                // In red: a majority of cache misses.
                shading_result.m_main = Color4f(1.0f, 0.0f, 0.0f, 1.0f);
            }

#endif
        }

        StatisticsVector get_statistics() const override
        {
            StatisticsVector stats;
            stats.merge(m_texture_cache.get_statistics());
            stats.merge(m_intersector.get_statistics());
            stats.merge(m_lighting_engine->get_statistics());
            return stats;
        }

      private:
        struct Parameters
        {
            const float     m_transparency_threshold;
            const size_t    m_max_iterations;
            const bool      m_report_self_intersections;

            explicit Parameters(const ParamArray& params)
              : m_transparency_threshold(params.get_optional<float>("transparency_threshold", 0.001f))
              , m_max_iterations(params.get_optional<size_t>("max_iterations", 100))
              , m_report_self_intersections(params.get_optional<bool>("report_self_intersections", false))
            {
            }
        };

        const Parameters            m_params;
        const Scene&                m_scene;
        const float                 m_opacity_threshold;
        TextureCache                m_texture_cache;
        ILightingEngine*            m_lighting_engine;
        ShadingEngine&              m_shading_engine;
        OIIOTextureSystem&          m_oiio_texture_system;
        const size_t                m_thread_index;

        Arena                       m_arena;
        OSLShaderGroupExec          m_shadergroup_exec;
        const Intersector           m_intersector;
        Tracer                      m_tracer;
        const ShadingContext        m_shading_context;

        Vector2d                    m_image_point_dx;
        Vector2d                    m_image_point_dy;
    };
}


//
// GenericSampleRendererFactory class implementation.
//

GenericSampleRendererFactory::GenericSampleRendererFactory(
    const Scene&            scene,
    const Frame&            frame,
    const TraceContext&     trace_context,
    TextureStore&           texture_store,
    ILightingEngineFactory* lighting_engine_factory,
    ShadingEngine&          shading_engine,
    OIIOTextureSystem&      oiio_texture_system,
    OSLShadingSystem&       shading_system,
    const ParamArray&       params)
  : m_scene(scene)
  , m_frame(frame)
  , m_trace_context(trace_context)
  , m_texture_store(texture_store)
  , m_lighting_engine_factory(lighting_engine_factory)
  , m_shading_engine(shading_engine)
  , m_oiio_texture_system(oiio_texture_system)
  , m_shading_system(shading_system)
  , m_params(params)
{
}

void GenericSampleRendererFactory::release()
{
    delete this;
}

ISampleRenderer* GenericSampleRendererFactory::create(const size_t thread_index)
{
    return
        new GenericSampleRenderer(
            m_scene,
            m_frame,
            m_trace_context,
            m_texture_store,
            m_lighting_engine_factory,
            m_shading_engine,
            m_oiio_texture_system,
            m_shading_system,
            thread_index,
            m_params);
}

}   // namespace renderer
