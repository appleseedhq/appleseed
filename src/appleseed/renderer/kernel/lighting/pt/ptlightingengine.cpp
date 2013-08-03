
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
#include "ptlightingengine.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/lighting/directlightingintegrator.h"
#include "renderer/kernel/lighting/imagebasedlighting.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/mis.h"
#include "foundation/math/population.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Forward declarations.
namespace renderer  { class EnvironmentEDF; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Path Tracing lighting engine.
    //
    // Implementation of spectral, Monte Carlo backward path tracing,
    // optionally with next event estimation.
    //
    // Reference:
    //
    //   http://citeseer.ist.psu.edu/344088.html
    //

    class PTLightingEngine
      : public ILightingEngine
    {
      public:
        struct Parameters
        {
            const bool      m_enable_dl;                    // direct lighting enabled?
            const bool      m_enable_ibl;                   // image-based lighting enabled?
            const bool      m_enable_caustics;              // caustics enabled?

            const size_t    m_max_path_length;              // maximum path length, 0 for unlimited
            const size_t    m_rr_min_path_length;           // minimum path length before Russian Roulette is used, 0 for unlimited
            const bool      m_next_event_estimation;        // use next event estimation?

            const size_t    m_dl_light_sample_count;        // number of light samples used to estimate direct illumination
            const size_t    m_ibl_env_sample_count;         // number of environment samples used to estimate IBL

            const bool      m_has_max_ray_intensity;
            const float     m_max_ray_intensity;

            explicit Parameters(const ParamArray& params)
              : m_enable_dl(params.get_optional<bool>("enable_dl", true))
              , m_enable_ibl(params.get_optional<bool>("enable_ibl", true))
              , m_enable_caustics(params.get_optional<bool>("enable_caustics", true))
              , m_max_path_length(params.get_optional<size_t>("max_path_length", 0))
              , m_rr_min_path_length(params.get_optional<size_t>("rr_min_path_length", 3))
              , m_next_event_estimation(params.get_optional<bool>("next_event_estimation", true))
              , m_dl_light_sample_count(params.get_optional<size_t>("dl_light_samples", 1))
              , m_ibl_env_sample_count(params.get_optional<size_t>("ibl_env_samples", 1))
              , m_has_max_ray_intensity(params.strings().exist("max_ray_intensity"))
              , m_max_ray_intensity(params.get_optional<float>("max_ray_intensity", 0.0f))
            {
            }

            void print() const
            {
                RENDERER_LOG_INFO(
                    "path tracing settings:\n"
                    "  direct lighting  %s\n"
                    "  ibl              %s\n"
                    "  caustics         %s\n"
                    "  max path length  %s\n"
                    "  rr min path len. %s\n"
                    "  next event est.  %s\n"
                    "  dl light samples %s\n"
                    "  ibl env samples  %s\n"
                    "  max ray intens.  %s",
                    m_enable_dl ? "on" : "off",
                    m_enable_ibl ? "on" : "off",
                    m_enable_caustics ? "on" : "off",
                    m_max_path_length == 0 ? "infinite" : pretty_uint(m_max_path_length).c_str(),
                    m_rr_min_path_length == 0 ? "infinite" : pretty_uint(m_rr_min_path_length).c_str(),
                    m_next_event_estimation ? "on" : "off",
                    pretty_uint(m_dl_light_sample_count).c_str(),
                    pretty_uint(m_ibl_env_sample_count).c_str(),
                    m_has_max_ray_intensity ? pretty_scalar(m_max_ray_intensity).c_str() : "infinite");
            }
        };

        PTLightingEngine(
            const LightSampler&     light_sampler,
            const ParamArray&       params)
          : m_params(params)
          , m_light_sampler(light_sampler)
          , m_path_count(0)
        {
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual void compute_lighting(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Spectrum&               radiance,               // output radiance, in W.sr^-1.m^-2
            SpectrumStack&          aovs) OVERRIDE
        {
            PathVisitor path_visitor(
                m_params,
                m_light_sampler,
                shading_context,
                shading_point.get_scene(),
                radiance,
                aovs);

            PathTracer<PathVisitor, false> path_tracer(     // false = not adjoint
                path_visitor,
                m_params.m_rr_min_path_length,
                m_params.m_max_path_length,
                shading_context.get_max_iterations());

            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    shading_context.get_intersector(),
                    shading_context.get_texture_cache(),
                    shading_point);

            // Update statistics.
            ++m_path_count;
            m_path_length.insert(path_length);
        }

        virtual StatisticsVector get_statistics() const OVERRIDE
        {
            Statistics stats;
            stats.insert("path count", m_path_count);
            stats.insert("path length", m_path_length);

            return StatisticsVector::make("path tracing statistics", stats);
        }

      private:
        class PathVisitor
        {
          public:
            PathVisitor(
                const Parameters&       params,
                const LightSampler&     light_sampler,
                const ShadingContext&   shading_context,
                const Scene&            scene,
                Spectrum&               path_radiance,
                SpectrumStack&          path_aovs)
              : m_params(params)
              , m_light_sampler(light_sampler)
              , m_shading_context(shading_context)
              , m_texture_cache(shading_context.get_texture_cache())
              , m_env_edf(scene.get_environment()->get_environment_edf())
              , m_path_radiance(path_radiance)
              , m_path_aovs(path_aovs)
            {
            }

            bool accept_scattering_mode(
                const BSDF::Mode        prev_bsdf_mode,
                const BSDF::Mode        bsdf_mode) const
            {
                assert(bsdf_mode != BSDF::Absorption);

                if (!m_params.m_enable_caustics)
                {
                    // No caustics.
                    if (BSDF::has_diffuse(prev_bsdf_mode) && BSDF::has_glossy_or_specular(bsdf_mode))
                        return false;
                }

                return true;
            }

            bool visit_vertex(
                SamplingContext&        sampling_context,
                const ShadingPoint&     shading_point,
                const Vector3d&         outgoing,
                const BSDF*             bsdf,
                const void*             bsdf_data,
                const size_t            path_length,
                const BSDF::Mode        prev_bsdf_mode,
                const double            prev_bsdf_prob,
                const Spectrum&         throughput)
            {
                const Vector3d& point = shading_point.get_point();
                const Vector3d& geometric_normal = shading_point.get_geometric_normal();
                const Vector3d& shading_normal = shading_point.get_shading_normal();
                const Basis3d& shading_basis = shading_point.get_shading_basis();
                const Material* material = shading_point.get_material();

                const EDF* edf = material->get_edf();
                const double cos_on = dot(outgoing, shading_normal);

                // Evaluate the input values of the EDF (if any).
                InputEvaluator edf_input_evaluator(m_texture_cache);
                const void* edf_data = edf
                    ? edf_input_evaluator.evaluate(
                        edf->get_inputs(),
                        shading_point.get_uv(0))
                    : 0;

                if (m_params.m_next_event_estimation)
                {
                    const int scattering_modes =
                        prev_bsdf_mode == BSDF::Diffuse && !m_params.m_enable_caustics
                            ? BSDF::Diffuse
                            : BSDF::AllScatteringModes;

                    Spectrum vertex_radiance;
                    SpectrumStack vertex_aovs(m_path_aovs.size());

                    if (bsdf && (m_params.m_enable_dl || path_length > 1))
                    {
                        // Compute direct lighting.
                        // We compute direct lighting by sampling the lights only. Direct
                        // lighting via BSDF sampling will be done by extending the current
                        // path with a single ray.
                        DirectLightingIntegrator integrator(
                            m_shading_context,
                            m_light_sampler,
                            shading_point,
                            outgoing,
                            *bsdf,
                            bsdf_data,
                            scattering_modes,
                            scattering_modes,
                            1,                                  // a single BSDF sample since the path will be extended with a single ray
                            m_params.m_dl_light_sample_count);  // the number of light samples is user-adjustable
                        integrator.sample_lights_low_variance(
                            prev_bsdf_mode == BSDF::Diffuse,    // indirect lighting situation?
                            sampling_context,
                            DirectLightingIntegrator::mis_power2,
                            vertex_radiance,
                            vertex_aovs);
                    }
                    else
                    {
                        vertex_radiance.set(0.0f);
                        vertex_aovs.set(0.0f);
                    }

                    if (bsdf && m_env_edf && m_params.m_enable_ibl)
                    {
                        // Compute image-based lighting.
                        // We compute image-based lighting by sampling the environment only.
                        // Image-based lighting via BSDF sampling will be done by extending
                        // the current path with a single ray.
                        Spectrum ibl_radiance;
                        compute_ibl_environment_sampling(
                            sampling_context,
                            m_shading_context,
                            *m_env_edf,
                            shading_point,
                            outgoing,
                            *bsdf,
                            bsdf_data,
                            scattering_modes,
                            1,                                  // a single BSDF sample since the path will be extended with a single ray
                            m_params.m_ibl_env_sample_count,    // the number of environment samples is user-adjustable
                            ibl_radiance);
                        vertex_radiance += ibl_radiance;
                        vertex_aovs.add(m_env_edf->get_render_layer_index(), ibl_radiance);
                    }

                    if (edf &&
                        cos_on > 0.0 &&
                        (path_length > 2 || m_params.m_enable_dl) &&
                        (path_length < 2 || (edf->get_flags() & EDF::CastIndirectLight)))
                    {
                        // Compute the emitted radiance.
                        Spectrum emitted_radiance;
                        edf->evaluate(
                            edf_data,
                            geometric_normal,
                            shading_basis,
                            outgoing,
                            emitted_radiance);

                        // Multiple importance sampling.
                        const double square_distance = square(shading_point.get_distance());
                        if (prev_bsdf_mode != BSDF::Specular && square_distance > 0.0)
                        {
                            // Transform prev_bsdf_prob to surface area measure (Veach: 8.2.2.2 eq. 8.10).
                            const double bsdf_point_prob = prev_bsdf_prob * cos_on / square_distance;

                            // Compute the probability density wrt. surface area of choosing this point
                            // by sampling the light sources.
                            const double light_point_prob = m_light_sampler.evaluate_pdf(shading_point);

                            // Apply MIS.
                            const double mis_weight =
                                mis_power2(
                                    1 * bsdf_point_prob,
                                    m_params.m_dl_light_sample_count * light_point_prob);
                            emitted_radiance *= static_cast<float>(mis_weight);
                        }

                        vertex_radiance += emitted_radiance;
                        vertex_aovs.add(edf->get_render_layer_index(), emitted_radiance);
                    }

                    // Optionally clamp secondary rays contribution.
                    if (m_params.m_has_max_ray_intensity && path_length > 1)
                    {
                        clamp_contribution(vertex_radiance);

                        for (size_t i = 0; i < vertex_aovs.size(); ++i)
                            clamp_contribution(vertex_aovs[i]);
                    }

                    // Update the path radiance.
                    vertex_radiance *= throughput;
                    m_path_radiance += vertex_radiance;
                    vertex_aovs *= throughput;
                    m_path_aovs += vertex_aovs;
                }
                else
                {
                    if (edf &&
                        cos_on > 0.0 &&
                        (path_length > 2 || m_params.m_enable_dl) &&
                        (path_length < 2 || (edf->get_flags() & EDF::CastIndirectLight)))
                    {
                        // Compute the emitted radiance.
                        Spectrum emitted_radiance;
                        edf->evaluate(
                            edf_data,
                            geometric_normal,
                            shading_basis,
                            outgoing,
                            emitted_radiance);

                        // Update the path radiance.
                        emitted_radiance *= throughput;
                        m_path_radiance += emitted_radiance;
                        m_path_aovs.add(edf->get_render_layer_index(), emitted_radiance);
                    }
                }

                // Proceed with this path.
                return true;
            }

            void visit_environment(
                const ShadingPoint&     shading_point,
                const Vector3d&         outgoing,
                const BSDF::Mode        prev_bsdf_mode,
                const double            prev_bsdf_prob,
                const Spectrum&         throughput)
            {
                assert(prev_bsdf_mode != BSDF::Absorption);

                // Can't look up the environment if there's no environment EDF.
                if (m_env_edf == 0)
                    return;

                // When IBL is disabled, only specular reflections should contribute here.
                if (!m_params.m_enable_ibl && prev_bsdf_mode != BSDF::Specular)
                    return;

                // Evaluate the environment EDF.
                InputEvaluator input_evaluator(m_texture_cache);
                Spectrum env_radiance;
                double env_prob;
                m_env_edf->evaluate(
                    input_evaluator,
                    -outgoing,
                    env_radiance,
                    env_prob);

                // If next event estimation is enabled, apply multiple importance sampling.
                if (m_params.m_next_event_estimation && prev_bsdf_mode != BSDF::Specular)
                {
                    assert(prev_bsdf_prob > 0.0);
                    const double mis_weight =
                        mis_power2(
                            1 * prev_bsdf_prob,
                            m_params.m_ibl_env_sample_count * env_prob);
                    env_radiance *= static_cast<float>(mis_weight);
                }

                // Update the path radiance.
                env_radiance *= throughput;
                m_path_radiance += env_radiance;
                m_path_aovs.add(m_env_edf->get_render_layer_index(), env_radiance);
            }

          private:
            const Parameters&       m_params;
            const LightSampler&     m_light_sampler;
            const ShadingContext&   m_shading_context;
            TextureCache&           m_texture_cache;
            const EnvironmentEDF*   m_env_edf;
            Spectrum&               m_path_radiance;
            SpectrumStack&          m_path_aovs;

            void clamp_contribution(Spectrum& radiance)
            {
                const float avg = average_value(radiance);

                if (avg > m_params.m_max_ray_intensity)
                    radiance *= m_params.m_max_ray_intensity / avg;
            }
        };

        const Parameters        m_params;
        const LightSampler&     m_light_sampler;

        uint64                  m_path_count;
        Population<uint64>      m_path_length;
    };
}


//
// PTLightingEngineFactory class implementation.
//

PTLightingEngineFactory::PTLightingEngineFactory(
    const LightSampler& light_sampler,
    const ParamArray&   params)
  : m_light_sampler(light_sampler)
  , m_params(params)
{
    PTLightingEngine::Parameters(params).print();
}

void PTLightingEngineFactory::release()
{
    delete this;
}

ILightingEngine* PTLightingEngineFactory::create()
{
    return new PTLightingEngine(m_light_sampler, m_params);
}

}   // namespace renderer
