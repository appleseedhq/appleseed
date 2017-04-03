
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/directlightingintegrator.h"
#include "renderer/kernel/lighting/imagebasedlighting.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/stochasticcast.h"

// appleseed.foundation headers.
#include "foundation/math/mis.h"
#include "foundation/math/population.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <string>

// Forward declarations.
namespace renderer  { class LightSampler; }
namespace renderer  { class PixelContext; }
namespace renderer  { class TextureCache; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Path Tracing lighting engine.
    //
    // Implementation of Monte Carlo backward path tracing with and without next event estimation.
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
            const bool      m_enable_dl;                    // is direct lighting enabled?
            const bool      m_enable_ibl;                   // is image-based lighting enabled?
            const bool      m_enable_caustics;              // are caustics enabled?

            const size_t    m_max_path_length;              // maximum path length, ~0 for unlimited
            const size_t    m_max_specular_bounces;         // maximum number of specular bounces, ~0 for unlimited
            const size_t    m_max_glossy_bounces;           // maximum number of glossy bounces, ~0 for unlimited
            const size_t    m_max_diffuse_bounces;          // maximum number of diffuse bounces, ~0 for unlimited
            const size_t    m_rr_min_path_length;           // minimum path length before Russian Roulette kicks in, ~0 for unlimited
            const bool      m_next_event_estimation;        // use next event estimation?

            const float     m_dl_light_sample_count;        // number of light samples used to estimate direct illumination
            const float     m_dl_low_light_threshold;       // light contribution threshold to disable shadow rays
            const float     m_ibl_env_sample_count;         // number of environment samples used to estimate IBL

            const bool      m_has_max_ray_intensity;
            const float     m_max_ray_intensity;

            float           m_rcp_dl_light_sample_count;
            float           m_rcp_ibl_env_sample_count;

            explicit Parameters(const ParamArray& params)
              : m_enable_dl(params.get_optional<bool>("enable_dl", true))
              , m_enable_ibl(params.get_optional<bool>("enable_ibl", true))
              , m_enable_caustics(params.get_optional<bool>("enable_caustics", false))
              , m_max_path_length(fixup_path_length(params.get_optional<size_t>("max_path_length", 0)))
              , m_max_specular_bounces(fixup_bounces(params.get_optional<int>("max_specular_bounces", -1)))
              , m_max_glossy_bounces(fixup_bounces(params.get_optional<int>("max_glossy_bounces", -1)))
              , m_max_diffuse_bounces(fixup_bounces(params.get_optional<int>("max_diffuse_bounces", -1)))
              , m_rr_min_path_length(fixup_path_length(params.get_optional<size_t>("rr_min_path_length", 6)))
              , m_next_event_estimation(params.get_optional<bool>("next_event_estimation", true))
              , m_dl_light_sample_count(params.get_optional<float>("dl_light_samples", 1.0f))
              , m_dl_low_light_threshold(params.get_optional<float>("dl_low_light_threshold", 0.0f))
              , m_ibl_env_sample_count(params.get_optional<float>("ibl_env_samples", 1.0f))
              , m_has_max_ray_intensity(params.strings().exist("max_ray_intensity"))
              , m_max_ray_intensity(params.get_optional<float>("max_ray_intensity", 0.0f))
            {
                // Precompute the reciprocal of the number of light samples.
                m_rcp_dl_light_sample_count =
                    m_dl_light_sample_count > 0.0f && m_dl_light_sample_count < 1.0f
                        ? 1.0f / m_dl_light_sample_count
                        : 0.0f;

                // Precompute the reciprocal of the number of environment samples.
                m_rcp_ibl_env_sample_count =
                    m_ibl_env_sample_count > 0.0f && m_ibl_env_sample_count < 1.0f
                        ? 1.0f / m_ibl_env_sample_count
                        : 0.0f;
            }

            static size_t fixup_path_length(const size_t x)
            {
                return x == 0 ? ~0 : x;
            }

            static size_t fixup_bounces(const int x)
            {
                return x == -1 ? ~0 : x;
            }

            void print() const
            {
                RENDERER_LOG_INFO(
                    "path tracing settings:\n"
                    "  direct lighting               %s\n"
                    "  ibl                           %s\n"
                    "  caustics                      %s\n"
                    "  max path length               %s\n"
                    "  max specular bounces          %s\n"
                    "  max glossy bounces            %s\n"
                    "  max diffuse bounces           %s\n"
                    "  rr min path length            %s\n"
                    "  next event estimation         %s\n"
                    "  dl light samples              %s\n"
                    "  dl light threshold            %s\n"
                    "  ibl env samples               %s\n"
                    "  max ray intensity             %s",
                    m_enable_dl ? "on" : "off",
                    m_enable_ibl ? "on" : "off",
                    m_enable_caustics ? "on" : "off",
                    m_max_path_length == size_t(~0) ? "infinite" : pretty_uint(m_max_path_length).c_str(),
                    m_max_specular_bounces == size_t(~0) ? "infinite" : pretty_uint(m_max_specular_bounces).c_str(),
                    m_max_glossy_bounces == size_t(~0) ? "infinite" : pretty_uint(m_max_glossy_bounces).c_str(),
                    m_max_diffuse_bounces == size_t(~0) ? "infinite" : pretty_uint(m_max_diffuse_bounces).c_str(),
                    m_rr_min_path_length == size_t(~0) ? "infinite" : pretty_uint(m_rr_min_path_length).c_str(),
                    m_next_event_estimation ? "on" : "off",
                    pretty_scalar(m_dl_light_sample_count).c_str(),
                    pretty_scalar(m_dl_low_light_threshold, 3).c_str(),
                    pretty_scalar(m_ibl_env_sample_count).c_str(),
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

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual void compute_lighting(
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Spectrum&               radiance) APPLESEED_OVERRIDE    // output radiance, in W.sr^-1.m^-2
        {
            if (m_params.m_next_event_estimation)
            {
                do_compute_lighting<PathVisitorNextEventEstimation>(
                    sampling_context,
                    shading_context,
                    shading_point,
                    radiance);
            }
            else
            {
                do_compute_lighting<PathVisitorSimple>(
                    sampling_context,
                    shading_context,
                    shading_point,
                    radiance);
            }
        }

        template <typename PathVisitor>
        void do_compute_lighting(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Spectrum&               radiance)               // output radiance, in W.sr^-1.m^-2

        {
            PathVisitor path_visitor(
                m_params,
                m_light_sampler,
                sampling_context,
                shading_context,
                shading_point.get_scene(),
                radiance);

            PathTracer<PathVisitor, false> path_tracer(     // false = not adjoint
                path_visitor,
                m_params.m_rr_min_path_length,
                m_params.m_max_path_length,
                shading_context.get_max_iterations());

            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    shading_context,
                    shading_point);

            // Update statistics.
            ++m_path_count;
            m_path_length.insert(path_length);
        }

        virtual StatisticsVector get_statistics() const APPLESEED_OVERRIDE
        {
            Statistics stats;
            stats.insert("path count", m_path_count);
            stats.insert("path length", m_path_length);

            return StatisticsVector::make("path tracing statistics", stats);
        }

      private:
        const Parameters                m_params;
        const LightSampler&             m_light_sampler;

        uint64                          m_path_count;
        Population<uint64>              m_path_length;

        //
        // Base path visitor.
        //

        struct PathVisitorBase
        {
            const Parameters&           m_params;
            const LightSampler&         m_light_sampler;
            SamplingContext&            m_sampling_context;
            const ShadingContext&       m_shading_context;
            const EnvironmentEDF*       m_env_edf;
            Spectrum&                   m_path_radiance;
            bool                        m_omit_emitted_light;   // todo: get rid of this
            uint64                      m_specular_bounces;
            uint64                      m_glossy_bounces;
            uint64                      m_diffuse_bounces;

            PathVisitorBase(
                const Parameters&       params,
                const LightSampler&     light_sampler,
                SamplingContext&        sampling_context,
                const ShadingContext&   shading_context,
                const Scene&            scene,
                Spectrum&               path_radiance)
              : m_params(params)
              , m_light_sampler(light_sampler)
              , m_sampling_context(sampling_context)
              , m_shading_context(shading_context)
              , m_env_edf(scene.get_environment()->get_environment_edf())
              , m_path_radiance(path_radiance)
              , m_omit_emitted_light(false)
              , m_specular_bounces(0)
              , m_glossy_bounces(0)
              , m_diffuse_bounces(0)
            {
            }

            bool accept_scattering(
                const ScatteringMode::Mode  prev_mode,
                const ScatteringMode::Mode  next_mode)
            {
                assert(next_mode != ScatteringMode::Absorption);

                // Count the number of diffuse bounces.
                if (ScatteringMode::has_diffuse(next_mode))
                    m_diffuse_bounces++;

                // Don't exceed the maximum limit of diffuse bounces.
                if (m_diffuse_bounces > m_params.m_max_diffuse_bounces)
                    return false;

                // Count the number of glossy bounces.
                if (ScatteringMode::has_glossy(next_mode))
                    m_glossy_bounces++;

                // Don't exceed the maximum limit of glossy bounces.
                if (m_glossy_bounces > m_params.m_max_glossy_bounces)
                    return false;

                // Count the number of specular bounces.
                if (ScatteringMode::has_specular(next_mode))
                    m_specular_bounces++;

                // Don't exceed the maximum limit of specular bounces.
                if (m_specular_bounces > m_params.m_max_specular_bounces)
                    return false;

                if (!m_params.m_enable_caustics)
                {
                    // Don't follow paths leading to caustics.
                    if (ScatteringMode::has_diffuse(prev_mode) &&
                        ScatteringMode::has_glossy_or_specular(next_mode))
                        return false;

                    // Ignore light emission after glossy-to-specular bounces to prevent another class of fireflies.
                    if (ScatteringMode::has_glossy(prev_mode) &&
                        ScatteringMode::has_specular(next_mode))
                        m_omit_emitted_light = true;
                }

                return true;
            }
        };

        //
        // Path visitor without next event estimation.
        //

        struct PathVisitorSimple
          : public PathVisitorBase
        {
            PathVisitorSimple(
                const Parameters&       params,
                const LightSampler&     light_sampler,
                SamplingContext&        sampling_context,
                const ShadingContext&   shading_context,
                const Scene&            scene,
                Spectrum&               path_radiance)
              : PathVisitorBase(
                    params,
                    light_sampler,
                    sampling_context,
                    shading_context,
                    scene,
                    path_radiance)
            {
            }

            void visit_vertex(const PathVertex& vertex)
            {
                if ((!m_omit_emitted_light || m_params.m_enable_caustics) &&
                    vertex.m_edf &&
                    vertex.m_cos_on > 0.0 &&
                    (vertex.m_path_length > 2 || m_params.m_enable_dl) &&
                    (vertex.m_path_length < 2 || (vertex.m_edf->get_flags() & EDF::CastIndirectLight)))
                {
                    // Compute the emitted radiance.
                    Spectrum emitted_radiance(Spectrum::Illuminance);
                    vertex.compute_emitted_radiance(m_shading_context, emitted_radiance);

                    // Update the path radiance.
                    emitted_radiance *= vertex.m_throughput;
                    m_path_radiance += emitted_radiance;
                }
            }

            void visit_environment(const PathVertex& vertex)
            {
                assert(vertex.m_prev_mode != ScatteringMode::Absorption);

                // Can't look up the environment if there's no environment EDF.
                if (m_env_edf == 0)
                    return;

                // When IBL is disabled, only specular reflections should contribute here.
                if (!m_params.m_enable_ibl && vertex.m_prev_mode != ScatteringMode::Specular)
                    return;

                // Evaluate the environment EDF.
                Spectrum env_radiance(Spectrum::Illuminance);
                float env_prob;
                m_env_edf->evaluate(
                    m_shading_context,
                    -Vector3f(vertex.m_outgoing.get_value()),
                    env_radiance,
                    env_prob);

                // Update path radiance.
                env_radiance *= vertex.m_throughput;
                m_path_radiance += env_radiance;
            }
        };

        //
        // Path visitor with next event estimation.
        //

        struct PathVisitorNextEventEstimation
          : public PathVisitorBase
        {
            bool m_is_indirect_lighting;

            PathVisitorNextEventEstimation(
                const Parameters&       params,
                const LightSampler&     light_sampler,
                SamplingContext&        sampling_context,
                const ShadingContext&   shading_context,
                const Scene&            scene,
                Spectrum&               path_radiance)
              : PathVisitorBase(
                    params,
                    light_sampler,
                    sampling_context,
                    shading_context,
                    scene,
                    path_radiance)
              , m_is_indirect_lighting(false)
            {
            }

            void visit_vertex(const PathVertex& vertex)
            {
                // Any light contribution after a diffuse or glossy bounce is considered indirect.
                if (ScatteringMode::has_diffuse_or_glossy(vertex.m_prev_mode))
                    m_is_indirect_lighting = true;

                const int scattering_modes =
                    !m_params.m_enable_caustics && vertex.m_prev_mode == ScatteringMode::Diffuse
                        ? ScatteringMode::Diffuse
                        : ScatteringMode::All;

                const bool last_vertex = vertex.m_path_length == m_params.m_max_path_length;

                Spectrum vertex_radiance(0.0f, Spectrum::Illuminance);

                // Emitted light.
                if ((!m_omit_emitted_light || m_params.m_enable_caustics) &&
                    vertex.m_edf &&
                    vertex.m_cos_on > 0.0 &&
                    (vertex.m_path_length > 2 || m_params.m_enable_dl) &&
                    (vertex.m_path_length < 2 || (vertex.m_edf->get_flags() & EDF::CastIndirectLight)))
                {
                    add_emitted_light_contribution(
                        vertex,
                        vertex_radiance);
                }

                if (vertex.m_bssrdf == 0)
                {
                    // If we have an OSL shader and this is not the last vertex of the path,
                    // we need to choose one of the closures and set its shading basis into the shading point
                    // for the DirectLightingIntegrator to use it.
                    if (!last_vertex && (m_params.m_enable_dl || m_params.m_enable_ibl))
                    {
                        const Material::RenderData& material_data =
                            vertex.m_shading_point->get_material()->get_render_data();

                        if (material_data.m_shader_group)
                        {
                            m_sampling_context.split_in_place(2, 1);
                            m_shading_context.choose_bsdf_closure_shading_basis(
                                *vertex.m_shading_point,
                                m_sampling_context.next2<Vector2f>());
                        }
                    }
                }

                // Direct lighting.
                if (m_params.m_enable_dl || vertex.m_path_length > 1)
                {
                    if (vertex.m_bsdf)
                    {
                        add_direct_lighting_contribution_bsdf(
                            *vertex.m_shading_point,
                            vertex.m_outgoing,
                            *vertex.m_bsdf,
                            vertex.m_bsdf_data,
                            last_vertex,
                            scattering_modes,
                            vertex_radiance);
                    }
                }

                // Image-based lighting.
                if (m_params.m_enable_ibl && m_env_edf)
                {
                    if (vertex.m_bsdf)
                    {
                        add_image_based_lighting_contribution_bsdf(
                            *vertex.m_shading_point,
                            vertex.m_outgoing,
                            *vertex.m_bsdf,
                            vertex.m_bsdf_data,
                            last_vertex,
                            scattering_modes,
                            vertex_radiance);
                    }
                }

                // Apply path throughput.
                vertex_radiance *= vertex.m_throughput;

                // Optionally clamp secondary rays contribution.
                if (m_params.m_has_max_ray_intensity && vertex.m_path_length > 1)
                    clamp_contribution(vertex_radiance);

                // Update path radiance.
                m_path_radiance += vertex_radiance;
            }

            void add_direct_lighting_contribution_bsdf(
                const ShadingPoint&     shading_point,
                const Dual3d&           outgoing,
                const BSDF&             bsdf,
                const void*             bsdf_data,
                const bool              last_vertex,
                const int               scattering_modes,
                Spectrum&               vertex_radiance)
            {
                Spectrum dl_radiance(Spectrum::Illuminance);

                const size_t light_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_dl_light_sample_count);

                const size_t bsdf_sample_count = last_vertex ? light_sample_count : 1;

                const DirectLightingIntegrator integrator(
                    m_shading_context,
                    m_light_sampler,
                    shading_point,
                    bsdf,
                    bsdf_data,
                    scattering_modes,
                    scattering_modes,
                    bsdf_sample_count,
                    light_sample_count,
                    m_params.m_dl_low_light_threshold,
                    m_is_indirect_lighting);

                if (last_vertex)
                {
                    // This path won't be extended: sample both the lights and the BSDF.
                    integrator.compute_outgoing_radiance_combined_sampling_low_variance(
                        m_sampling_context,
                        outgoing,
                        dl_radiance);
                }
                else
                {
                    // This path will be extended via BSDF sampling: sample the lights only.
                    integrator.compute_outgoing_radiance_light_sampling_low_variance(
                        m_sampling_context,
                        MISPower2,
                        outgoing,
                        dl_radiance);
                }

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_dl_light_sample_count > 0.0f)
                    dl_radiance *= m_params.m_rcp_dl_light_sample_count;

                // Add direct lighting contribution.
                vertex_radiance += dl_radiance;
            }

            void add_emitted_light_contribution(
                const PathVertex&       vertex,
                Spectrum&               vertex_radiance)
            {
                // Compute the emitted radiance.
                Spectrum emitted_radiance(Spectrum::Illuminance);
                vertex.compute_emitted_radiance(m_shading_context, emitted_radiance);

                // Multiple importance sampling.
                if (vertex.m_prev_mode != ScatteringMode::Specular)
                {
                    const float light_sample_count = max(m_params.m_dl_light_sample_count, 1.0f);
                    const float mis_weight =
                        mis_power2(
                            1.0f * vertex.get_bsdf_prob_area(),
                            light_sample_count * vertex.get_light_prob_area(m_light_sampler));
                    emitted_radiance *= mis_weight;
                }

                // Add emitted light contribution.
                vertex_radiance += emitted_radiance;
            }

            void add_image_based_lighting_contribution_bsdf(
                const ShadingPoint&     shading_point,
                const Dual3d&           outgoing,
                const BSDF&             bsdf,
                const void*             bsdf_data,
                const bool              last_vertex,
                const int               scattering_modes,
                Spectrum&               vertex_radiance)
            {
                Spectrum ibl_radiance(Spectrum::Illuminance);

                const size_t env_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_ibl_env_sample_count);

                const size_t bsdf_sample_count = last_vertex ? env_sample_count : 1;

                if (last_vertex)
                {
                    // This path won't be extended: sample both the environment and the BSDF.
                    compute_ibl_combined_sampling(
                        m_sampling_context,
                        m_shading_context,
                        *m_env_edf,
                        shading_point,
                        outgoing,
                        bsdf,
                        bsdf_data,
                        scattering_modes,
                        scattering_modes,
                        bsdf_sample_count,
                        env_sample_count,
                        ibl_radiance);
                }
                else
                {
                    // This path will be extended via BSDF sampling: sample the environment only.
                    compute_ibl_environment_sampling(
                        m_sampling_context,
                        m_shading_context,
                        *m_env_edf,
                        shading_point,
                        outgoing,
                        bsdf,
                        bsdf_data,
                        scattering_modes,
                        bsdf_sample_count,
                        env_sample_count,
                        ibl_radiance);
                }

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_ibl_env_sample_count > 0.0f)
                    ibl_radiance *= m_params.m_rcp_ibl_env_sample_count;

                // Add image-based lighting contribution.
                vertex_radiance += ibl_radiance;
            }

            void visit_environment(const PathVertex& vertex)
            {
                assert(vertex.m_prev_mode != ScatteringMode::Absorption);

                // Can't look up the environment if there's no environment EDF.
                if (m_env_edf == 0)
                    return;

                // When IBL is disabled, only specular reflections should contribute here.
                if (!m_params.m_enable_ibl && vertex.m_prev_mode != ScatteringMode::Specular)
                    return;

                // Evaluate the environment EDF.
                Spectrum env_radiance(Spectrum::Illuminance);
                float env_prob;
                m_env_edf->evaluate(
                    m_shading_context,
                    -Vector3f(vertex.m_outgoing.get_value()),
                    env_radiance,
                    env_prob);

                // This may happen for points of the environment map with infinite components,
                // which are then excluded from importance sampling and thus have zero weight.
                if (env_prob == 0.0)
                    return;

                // Multiple importance sampling.
                if (vertex.m_prev_mode != ScatteringMode::Specular)
                {
                    assert(vertex.m_prev_prob > 0.0f);
                    const float env_sample_count = max(m_params.m_ibl_env_sample_count, 1.0f);
                    const float mis_weight =
                        mis_power2(
                            1.0f * vertex.m_prev_prob,
                            env_sample_count * env_prob);
                    env_radiance *= mis_weight;
                }

                // Apply path throughput.
                env_radiance *= vertex.m_throughput;

                // Optionally clamp secondary rays contribution.
                if (m_params.m_has_max_ray_intensity && vertex.m_path_length > 1)
                    clamp_contribution(env_radiance);

                // Update the path radiance.
                m_path_radiance += env_radiance;
            }

            void clamp_contribution(Spectrum& radiance) const
            {
                const float avg = average_value(radiance);

                if (avg > m_params.m_max_ray_intensity)
                    radiance *= m_params.m_max_ray_intensity / avg;
            }
        };
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

Dictionary PTLightingEngineFactory::get_params_metadata()
{
    Dictionary metadata;
    add_common_params_metadata(metadata, true);

    metadata.dictionaries().insert(
        "enable_dl",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "true")
            .insert("label", "Enable Direct Lighting")
            .insert("help", "Enable direct lighting"));

    metadata.dictionaries().insert(
        "enable_caustics",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "false")
            .insert("label", "Enable Caustics")
            .insert("help", "Enable caustics"));

    metadata.dictionaries().insert(
        "max_path_length",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "1")
            .insert("label", "Max Path Length")
            .insert("help", "Maximum number of path bounces"));

    metadata.dictionaries().insert(
        "max_specular_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "0")
            .insert("label", "Max Specular Bounces")
            .insert("help", "Maximum number of specular bounces"));

    metadata.dictionaries().insert(
        "max_glossy_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "0")
            .insert("label", "Max Glossy Bounces")
            .insert("help", "Maximum number of glossy bounces"));

    metadata.dictionaries().insert(
        "max_diffuse_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "0")
            .insert("label", "Max Diffuse Bounces")
            .insert("help", "Maximum number of diffuse bounces"));

    metadata.dictionaries().insert(
        "rr_min_path_length",
        Dictionary()
            .insert("type", "int")
            .insert("default", "6")
            .insert("min", "1")
            .insert("label", "Russian Roulette Start Bounce")
            .insert("help", "Consider pruning low contribution paths starting with this bounce"));

    metadata.dictionaries().insert(
        "next_event_estimation",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "true")
            .insert("label", "Next Event Estimation")
            .insert("help", "Explicitly connect path vertices to light sources to improve efficiency"));

    metadata.dictionaries().insert(
        "max_ray_intensity",
        Dictionary()
            .insert("type", "float")
            .insert("default", "1.0")
            .insert("unlimited", "true")
            .insert("min", "0.0")
            .insert("label", "Max Ray Intensity")
            .insert("help", "Clamp intensity of rays (after the first bounce) to this value to reduce fireflies"));

    return metadata;
}

}   // namespace renderer
