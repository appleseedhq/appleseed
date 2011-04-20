
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingengine.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/scene/scene.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Generic sample renderer.
    //

    class GenericSampleRenderer
      : public ISampleRenderer
    {
      public:
        GenericSampleRenderer(
            const Scene&            scene,
            const Frame&            frame,
            const TraceContext&     trace_context,
            ILightingEngineFactory* lighting_engine_factory,
            ShadingEngine&          shading_engine,
            const ParamArray&       params)
          : m_params(params)
          , m_scene(scene)
          , m_lighting_conditions(frame.get_lighting_conditions())
          , m_intersector(trace_context, true, m_params.m_report_self_intersections)
          , m_texture_cache(scene, m_params.m_texture_cache_size)
          , m_lighting_engine(lighting_engine_factory->create())
          , m_shading_engine(shading_engine)
        {
        }

        ~GenericSampleRenderer()
        {
            m_lighting_engine->release();
        }

        virtual void release()
        {
            delete this;
        }

        virtual void render_sample(
            SamplingContext&        sampling_context,
            const Vector2d&         image_point,        // point in image plane, in NDC
            ShadingResult&          shading_result)
        {
            // Construct a shading context.
            ShadingContext shading_context(
                m_intersector,
                m_texture_cache,
                m_lighting_engine);

            // Construct a primary ray.
            ShadingRay primary_ray;
            m_scene.get_camera()->generate_ray(
                sampling_context,
                image_point,
                0.0f,               // ray time
                primary_ray);

            // Initialize the result to linear RGB transparent black.
            shading_result.clear();

            ShadingPoint shading_points[2];
            size_t shading_point_index = 0;
            const ShadingPoint* shading_point_ptr = 0;

            while (true)
            {
                // Trace the ray.
                shading_points[shading_point_index].clear();
                m_intersector.trace(
                    primary_ray,
                    shading_points[shading_point_index],
                    shading_point_ptr);

                // Update the pointers to the shading points.
                shading_point_ptr = &shading_points[shading_point_index];
                shading_point_index = 1 - shading_point_index;

                // Shade the intersection point.
                ShadingResult local_result;
                m_shading_engine.shade(
                    sampling_context,
                    shading_context,
                    *shading_point_ptr,
                    local_result);

                // Transform the result to the linear RGB color space.
                local_result.transform_to_linear_rgb(m_lighting_conditions);

                // "Over" alpha compositing.
                const Alpha contrib = Alpha(1.0) - shading_result.m_alpha;
                const Alpha color_contrib = contrib * local_result.m_alpha;
                shading_result.m_color[0] += color_contrib[0] * local_result.m_color[0];
                shading_result.m_color[1] += color_contrib[0] * local_result.m_color[1];
                shading_result.m_color[2] += color_contrib[0] * local_result.m_color[2];
                shading_result.m_alpha += contrib * local_result.m_alpha;

                // Stop once we hit the environment.
                if (!shading_point_ptr->hit())
                    break;

                // Stop once we hit full opacity.
                const float Threshold = 1.0e-5f;
                if (max_value(shading_result.m_alpha) > 1.0f - Threshold)
                    break;

                // Move the ray origin to the intersection point.
                primary_ray.m_org = shading_point_ptr->get_point();
                primary_ray.m_tmax = numeric_limits<double>::max();
            }
        }

      private:
        struct Parameters
        {
            const size_t    m_texture_cache_size;           // size in bytes of the texture cache
            const bool      m_report_self_intersections;

            explicit Parameters(const ParamArray& params)
              : m_texture_cache_size(params.get_optional<size_t>("texture_cache_size", 16 * 1024 * 1024))
              , m_report_self_intersections(params.get_optional<bool>("report_self_intersections", false))
            {
            }
        };

        const Parameters            m_params;
        const Scene&                m_scene;
        const LightingConditions&   m_lighting_conditions;
        Intersector                 m_intersector;
        TextureCache                m_texture_cache;
        ILightingEngine*            m_lighting_engine;
        ShadingEngine&              m_shading_engine;
    };
}


//
// GenericSampleRendererFactory class implementation.
//

GenericSampleRendererFactory::GenericSampleRendererFactory(
    const Scene&            scene,
    const Frame&            frame,
    const TraceContext&     trace_context,
    ILightingEngineFactory* lighting_engine_factory,
    ShadingEngine&          shading_engine,
    const ParamArray&       params)
  : m_scene(scene)
  , m_frame(frame)
  , m_trace_context(trace_context)
  , m_lighting_engine_factory(lighting_engine_factory)
  , m_shading_engine(shading_engine)
  , m_params(params)
{
}

void GenericSampleRendererFactory::release()
{
    delete this;
}

ISampleRenderer* GenericSampleRendererFactory::create()
{
    return
        new GenericSampleRenderer(
            m_scene,
            m_frame,
            m_trace_context,
            m_lighting_engine_factory,
            m_shading_engine,
            m_params);
}

ISampleRenderer* GenericSampleRendererFactory::create(
    const Scene&            scene,
    const Frame&            frame,
    const TraceContext&     trace_context,
    ILightingEngineFactory* lighting_engine_factory,
    ShadingEngine&          shading_engine,
    const ParamArray&       params)
{
    return
        new GenericSampleRenderer(
            scene,
            frame,
            trace_context,
            lighting_engine_factory,
            shading_engine,
            params);
}

}   // namespace renderer
