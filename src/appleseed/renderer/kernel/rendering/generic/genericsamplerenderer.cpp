
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"

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
        // Constructor.
        GenericSampleRenderer(
            const Scene&            scene,
            const TraceContext&     trace_context,
            ILightingEngineFactory* lighting_engine_factory,
            ShadingEngine&          shading_engine,
            const ParamArray&       params)
          : m_params(params)
          , m_trace_context(trace_context)
          , m_intersector(trace_context, true, m_params.m_report_self_intersections)
          , m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
          , m_texture_cache(scene, m_params.m_texture_cache_size)
          , m_lighting_engine(lighting_engine_factory->create())
          , m_shading_engine(shading_engine)
        {
        }

        // Destructor.
        ~GenericSampleRenderer()
        {
            m_lighting_engine->release();
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Render a sample at a given point on the image plane.
        virtual void render_sample(
            SamplingContext&        sampling_context,
            const Vector2d&         image_point,        // point in image plane, in NDC
            ShadingResult&          shading_result)
        {
            // Retrieve the camera.
            const Scene& scene = m_trace_context.get_scene();
            const Camera* camera = scene.get_camera();
            assert(camera);

            // Construct a primary ray.
            ShadingRay primary_ray;
            camera->generate_ray(
                sampling_context,
                image_point,
                0.0f,               // ray time
                primary_ray);

            // Trace the primary ray.
            ShadingPoint shading_point;
            m_intersector.trace(primary_ray, shading_point);

            // Construct a shading context.
            ShadingContext shading_context(
                m_intersector,
                m_lighting_conditions,
                m_texture_cache,
                *m_lighting_engine);

            // Shade the intersection point.
            m_shading_engine.shade(
                sampling_context,
                shading_context,
                shading_point,
                shading_result);
        }

      private:
        // Parameters.
        struct Parameters
        {
            const size_t    m_texture_cache_size;           // size in bytes of the texture cache
            const bool      m_report_self_intersections;

            // Constructor, extract parameters.
            explicit Parameters(const ParamArray& params)
              : m_texture_cache_size(params.get_optional<size_t>("texture_cache_size", 16 * 1024 * 1024))
              , m_report_self_intersections(params.get_optional<bool>("report_self_intersections", false))
            {
            }
        };

        const Parameters            m_params;
        const TraceContext&         m_trace_context;
        Intersector                 m_intersector;
        LightingConditions          m_lighting_conditions;
        TextureCache                m_texture_cache;
        ILightingEngine*            m_lighting_engine;
        ShadingEngine&              m_shading_engine;
    };
}


//
// GenericSampleRendererFactory class implementation.
//

// Constructor.
GenericSampleRendererFactory::GenericSampleRendererFactory(
    const Scene&            scene,
    const TraceContext&     trace_context,
    ILightingEngineFactory* lighting_engine_factory,
    ShadingEngine&          shading_engine,
    const ParamArray&       params)
  : m_scene(scene)
  , m_trace_context(trace_context)
  , m_lighting_engine_factory(lighting_engine_factory)
  , m_shading_engine(shading_engine)
  , m_params(params)
{
}

// Delete this instance.
void GenericSampleRendererFactory::release()
{
    delete this;
}

// Return a new generic sample renderer instance.
ISampleRenderer* GenericSampleRendererFactory::create()
{
    return
        new GenericSampleRenderer(
            m_scene,
            m_trace_context,
            m_lighting_engine_factory,
            m_shading_engine,
            m_params);
}

// Return a new generic sample renderer instance.
ISampleRenderer* GenericSampleRendererFactory::create(
    const Scene&            scene,
    const TraceContext&     trace_context,
    ILightingEngineFactory* lighting_engine_factory,
    ShadingEngine&          shading_engine,
    const ParamArray&       params)
{
    return
        new GenericSampleRenderer(
            scene,
            trace_context,
            lighting_engine_factory,
            shading_engine,
            params);
}

}   // namespace renderer
