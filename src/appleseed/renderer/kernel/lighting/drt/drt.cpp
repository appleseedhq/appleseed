
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
#include "drt.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/directlighting.h"
#include "renderer/kernel/lighting/imagebasedlighting.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/mis.h"
#include "foundation/math/population.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/string.h"

// Forward declarations.
namespace renderer  { class EnvironmentEDF; }
namespace renderer  { class InputParams; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Distribution Ray Tracing (DRT) lighting engine.
    //

    class DRTLightingEngine
      : public ILightingEngine
    {
      public:
        DRTLightingEngine(
            const LightSampler&     light_sampler,
            const ParamArray&       params)
          : m_params(params)
          , m_light_sampler(light_sampler)
        {
        }

        ~DRTLightingEngine()
        {
            RENDERER_LOG_DEBUG(
                "distribution ray tracing statistics:\n"
                "  paths            %s\n"
                "  ray tree depth   avg %.1f  min %s  max %s  dev %.1f\n",
                pretty_uint(m_stats.m_path_count).c_str(),
                m_stats.m_ray_tree_depth.get_avg(),
                pretty_uint(m_stats.m_ray_tree_depth.get_min()).c_str(),
                pretty_uint(m_stats.m_ray_tree_depth.get_max()).c_str(),
                m_stats.m_ray_tree_depth.get_dev());
        }

        virtual void release()
        {
            delete this;
        }

        virtual void compute_lighting(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Spectrum&               radiance)   // output radiance, in W.sr^-1.m^-2
        {
            typedef PathTracer<
                PathVisitor,
                BSDF::Glossy | BSDF::Specular,
                false                           // not adjoint
            > PathTracer;

            PathVisitor path_visitor(
                m_params,
                m_light_sampler,
                m_light_samples,
                shading_context,
                shading_point.get_scene(),
                radiance);

            PathTracer path_tracer(
                path_visitor,
                m_params.m_minimum_path_length);

            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    shading_context.get_intersector(),
                    shading_context.get_texture_cache(),
                    shading_point);

            // Update statistics.
            ++m_stats.m_path_count;
            m_stats.m_ray_tree_depth.insert(path_length);
        }

      private:
        struct Parameters
        {
            const size_t        m_minimum_path_length;      // minimum path length before Russian Roulette is used
            const size_t        m_dl_sample_count;          // number of samples used to estimate direct illumination
            const size_t        m_ibl_bsdf_sample_count;    // number of samples (in BSDF sampling) used to estimate IBL
            const size_t        m_ibl_env_sample_count;     // number of samples (in environment sampling) used to estimate IBL

            // Constructor, extract parameters.
            explicit Parameters(const ParamArray& params)
              : m_minimum_path_length   ( params.get_optional<size_t>("minimum_path_length", 3) )
              , m_dl_sample_count       ( params.get_optional<size_t>("dl_samples", 1) )
              , m_ibl_bsdf_sample_count ( params.get_optional<size_t>("ibl_bsdf_samples", 2) )
              , m_ibl_env_sample_count  ( params.get_optional<size_t>("ibl_env_samples", 2) )
            {
            }
        };

        struct Statistics
        {
            uint64              m_path_count;               // number of paths
            Population<size_t>  m_ray_tree_depth;           // ray tree depth

            Statistics()
              : m_path_count(0)
            {
            }
        };

        class PathVisitor
        {
          public:
            PathVisitor(
                const Parameters&       params,
                const LightSampler&     light_sampler,
                LightSampleVector&      light_samples,
                const ShadingContext&   shading_context,
                const Scene&            scene,
                Spectrum&               path_radiance)
              : m_params(params)
              , m_light_sampler(light_sampler)
              , m_light_samples(light_samples)
              , m_shading_context(shading_context)
              , m_texture_cache(shading_context.get_texture_cache())
              , m_path_radiance(path_radiance)
            {
                const Environment* environment = scene.get_environment();
                m_env_edf = environment ? environment->get_environment_edf() : 0;

                m_path_radiance.set(0.0f);
            }

            bool visit_vertex(
                SamplingContext&        sampling_context,
                const ShadingPoint&     shading_point,
                const Vector3d&         outgoing,
                const BSDF*             bsdf,
                const void*             bsdf_data,
                const BSDF::Mode        bsdf_mode,
                const double            bsdf_prob,
                const Spectrum&         throughput)
            {
                const Vector3d& point = shading_point.get_point();
                const Vector3d& geometric_normal = shading_point.get_geometric_normal();
                const Vector3d& shading_normal = shading_point.get_shading_normal();
                const Basis3d& shading_basis = shading_point.get_shading_basis();
                const Material* material = shading_point.get_material();
                const InputParams& input_params = shading_point.get_input_params();

                // Generate light samples.
                clear_keep_memory(m_light_samples);
                m_light_sampler.sample(
                    sampling_context,
                    m_params.m_dl_sample_count,
                    m_light_samples);

                // Compute direct lighting.
                Spectrum vertex_radiance;
                compute_direct_lighting(
                    sampling_context,
                    m_shading_context,
                    point,
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    *bsdf,
                    bsdf_data,
                    m_light_samples,
                    vertex_radiance,
                    &shading_point);

                if (m_env_edf)
                {
                    // Compute image-based lighting.
                    Spectrum ibl_radiance;
                    compute_image_based_lighting(
                        sampling_context,
                        m_shading_context,
                        *m_env_edf,
                        point,
                        geometric_normal,
                        shading_basis,
                        outgoing,
                        *bsdf,
                        bsdf_data,
                        m_params.m_ibl_bsdf_sample_count,
                        m_params.m_ibl_env_sample_count,
                        ibl_radiance,
                        &shading_point);
                    vertex_radiance += ibl_radiance;
                }

                // Retrieve the EDF.
                const EDF* edf = material->get_edf();

                if (edf)
                {
                    // Handle light sources visible either directly or through
                    // specular/glossy reflections and refractions.

                    // Evaluate the input values of the EDF (if any).
                    InputEvaluator edf_input_evaluator(m_texture_cache);
                    const void* edf_data =
                        edf_input_evaluator.evaluate(
                            edf->get_inputs(),
                            input_params);

                    // Compute the emitted radiance.
                    Spectrum emitted_radiance;
                    edf->evaluate(
                        edf_data,
                        geometric_normal,
                        shading_basis,
                        outgoing,
                        emitted_radiance);

                    const double distance = shading_point.get_distance();

                    if (bsdf_mode != BSDF::Specular && distance > 0.0)
                    {
                        // Compute the probability density with respect to surface area
                        // of choosing this point through sampling of the light sources.
                        const double sample_probability =
                            m_light_sampler.evaluate_pdf(shading_point);

                        // Compute the probability density with respect to surface area
                        // of the direction obtained through sampling of the BSDF.
                        double px = bsdf_prob;
                        px *= max(dot(outgoing, shading_normal), 0.0);
                        px /= square(distance);

                        // Multiply the emitted radiance by the MIS weight.
                        const double mis_weight = mis_power2(px, sample_probability);
                        emitted_radiance *= static_cast<float>(mis_weight);
                    }

                    vertex_radiance += emitted_radiance;
                }

                // Update the path radiance.
                vertex_radiance *= throughput;
                m_path_radiance += vertex_radiance;

                // Proceed with this path.
                return true;
            }

            void visit_environment(
                const ShadingPoint&     shading_point,
                const Vector3d&         outgoing,
                const Spectrum&         throughput)
            {
            }

          private:
            const Parameters&       m_params;
            const LightSampler&     m_light_sampler;
            LightSampleVector&      m_light_samples;
            const ShadingContext&   m_shading_context;
            TextureCache&           m_texture_cache;
            const EnvironmentEDF*   m_env_edf;
            Spectrum&               m_path_radiance;
        };

        const Parameters        m_params;
        Statistics              m_stats;
        const LightSampler&     m_light_sampler;
        LightSampleVector       m_light_samples;
    };
}


//
// DRTLightingEngineFactory class implementation.
//

DRTLightingEngineFactory::DRTLightingEngineFactory(
    const LightSampler& light_sampler,
    const ParamArray&   params)
  : m_light_sampler(light_sampler)
  , m_params(params)
{
}

void DRTLightingEngineFactory::release()
{
    delete this;
}

ILightingEngine* DRTLightingEngineFactory::create()
{
    return new DRTLightingEngine(m_light_sampler, m_params);
}

ILightingEngine* DRTLightingEngineFactory::create(
    const LightSampler& light_sampler,
    const ParamArray&   params)
{
    return new DRTLightingEngine(light_sampler, params);
}

}   // namespace renderer
