
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
#include "renderer/kernel/shading/shadingcomponents.h"
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

            const size_t    m_max_bounces;                  // maximum number of bounces, ~0 for unlimited
            const size_t    m_max_diffuse_bounces;          // maximum number of diffuse bounces, ~0 for unlimited
            const size_t    m_max_glossy_bounces;           // maximum number of glossy bounces, ~0 for unlimited
            const size_t    m_max_specular_bounces;         // maximum number of specular bounces, ~0 for unlimited

            const size_t    m_rr_min_path_length;           // minimum path length before Russian Roulette kicks in, ~0 for unlimited
            const bool      m_next_event_estimation;        // use next event estimation?

            const float     m_dl_light_sample_count;        // number of light samples used to estimate direct illumination
            const float     m_dl_low_light_threshold;       // light contribution threshold to disable shadow rays
            const float     m_ibl_env_sample_count;         // number of environment samples used to estimate IBL

            const bool      m_has_max_ray_intensity;
            const float     m_max_ray_intensity;

            const size_t    m_distance_sample_count;        // number of distance samples until the ray is completely extincted

            float           m_rcp_dl_light_sample_count;
            float           m_rcp_ibl_env_sample_count;

            explicit Parameters(const ParamArray& params)
              : m_enable_dl(params.get_optional<bool>("enable_dl", true))
              , m_enable_ibl(params.get_optional<bool>("enable_ibl", true))
              , m_enable_caustics(params.get_optional<bool>("enable_caustics", false))
              , m_max_bounces(fixup_bounces(params.get_optional<int>("max_bounces", -1)))
              , m_max_diffuse_bounces(fixup_bounces(params.get_optional<int>("max_diffuse_bounces", -1)))
              , m_max_glossy_bounces(fixup_bounces(params.get_optional<int>("max_glossy_bounces", -1)))
              , m_max_specular_bounces(fixup_bounces(params.get_optional<int>("max_specular_bounces", -1)))
              , m_rr_min_path_length(fixup_path_length(params.get_optional<size_t>("rr_min_path_length", 6)))
              , m_next_event_estimation(params.get_optional<bool>("next_event_estimation", true))
              , m_dl_light_sample_count(params.get_optional<float>("dl_light_samples", 1.0f))
              , m_dl_low_light_threshold(params.get_optional<float>("dl_low_light_threshold", 0.0f))
              , m_ibl_env_sample_count(params.get_optional<float>("ibl_env_samples", 1.0f))
              , m_has_max_ray_intensity(params.strings().exist("max_ray_intensity"))
              , m_distance_sample_count(params.get_optional<size_t>("volume_distance_samples", 4))
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

            static size_t fixup_bounces(const int x)
            {
                return x == -1 ? ~0 : x;
            }

            static size_t fixup_path_length(const size_t x)
            {
                return x == 0 ? ~0 : x;
            }

            void print() const
            {
                RENDERER_LOG_INFO(
                    "path tracing settings:\n"
                    "  direct lighting               %s\n"
                    "  ibl                           %s\n"
                    "  caustics                      %s\n"
                    "  max bounces                   %s\n"
                    "  max diffuse bounces           %s\n"
                    "  max glossy bounces            %s\n"
                    "  max specular bounces          %s\n"
                    "  rr min path length            %s\n"
                    "  next event estimation         %s\n"
                    "  dl light samples              %s\n"
                    "  dl light threshold            %s\n"
                    "  ibl env samples               %s\n"
                    "  max ray intensity             %s\n"
                    "  volume distance samples       %s",
                    m_enable_dl ? "on" : "off",
                    m_enable_ibl ? "on" : "off",
                    m_enable_caustics ? "on" : "off",
                    m_max_bounces == ~0 ? "infinite" : pretty_uint(m_max_bounces).c_str(),
                    m_max_diffuse_bounces == ~0 ? "infinite" : pretty_uint(m_max_diffuse_bounces).c_str(),
                    m_max_glossy_bounces == ~0 ? "infinite" : pretty_uint(m_max_glossy_bounces).c_str(),
                    m_max_specular_bounces == ~0 ? "infinite" : pretty_uint(m_max_specular_bounces).c_str(),
                    m_rr_min_path_length == ~0 ? "infinite" : pretty_uint(m_rr_min_path_length).c_str(),
                    m_next_event_estimation ? "on" : "off",
                    pretty_scalar(m_dl_light_sample_count).c_str(),
                    pretty_scalar(m_dl_low_light_threshold, 3).c_str(),
                    pretty_scalar(m_ibl_env_sample_count).c_str(),
                    m_has_max_ray_intensity ? pretty_scalar(m_max_ray_intensity).c_str() : "infinite",
                    pretty_int(m_distance_sample_count).c_str());
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

        virtual void release() override
        {
            delete this;
        }

        virtual void compute_lighting(
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingComponents&      radiance) override      // output radiance, in W.sr^-1.m^-2
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
            ShadingComponents&      radiance)               // output radiance, in W.sr^-1.m^-2
        {
            PathVisitor path_visitor(
                m_params,
                m_light_sampler,
                sampling_context,
                shading_context,
                shading_point.get_scene(),
                radiance);

            VolumeVisitorDistanceSampling volume_visitor(
                m_params,
                m_light_sampler,
                sampling_context,
                shading_context,
                shading_point.get_scene(),
                radiance);

            PathTracer<PathVisitor, VolumeVisitorDistanceSampling, false> path_tracer(     // false = not adjoint
                path_visitor,
                volume_visitor,
                m_params.m_rr_min_path_length,
                m_params.m_max_bounces == ~0 ? ~0 : m_params.m_max_bounces + 1,
                m_params.m_max_diffuse_bounces == ~0 ? ~0 : m_params.m_max_diffuse_bounces + 1,
                m_params.m_max_glossy_bounces,
                m_params.m_max_specular_bounces,
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

        virtual StatisticsVector get_statistics() const override
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
            ShadingComponents&          m_path_radiance;
            bool                        m_omit_emitted_light;

            PathVisitorBase(
                const Parameters&       params,
                const LightSampler&     light_sampler,
                SamplingContext&        sampling_context,
                const ShadingContext&   shading_context,
                const Scene&            scene,
                ShadingComponents&      path_radiance)
              : m_params(params)
              , m_light_sampler(light_sampler)
              , m_sampling_context(sampling_context)
              , m_shading_context(shading_context)
              , m_env_edf(scene.get_environment()->get_environment_edf())
              , m_path_radiance(path_radiance)
              , m_omit_emitted_light(false)
            {
            }

            bool accept_scattering(
                const ScatteringMode::Mode  prev_mode,
                const ScatteringMode::Mode  next_mode)
            {
                assert(next_mode != ScatteringMode::None);

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
                ShadingComponents&      path_radiance)
              : PathVisitorBase(
                    params,
                    light_sampler,
                    sampling_context,
                    shading_context,
                    scene,
                    path_radiance)
            {
            }

            void on_miss(const PathVertex& vertex)
            {
                assert(vertex.m_prev_mode != ScatteringMode::None);

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
                m_path_radiance.add_emission(
                    vertex.m_path_length,
                    vertex.m_aov_mode,
                    env_radiance);
            }

            void on_hit(const PathVertex& vertex)
            {
                // Emitted light contribution.
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
                    m_path_radiance.add_emission(
                        vertex.m_path_length,
                        vertex.m_aov_mode,
                        emitted_radiance);
                }
            }

            void on_scatter(PathVertex& vertex)
            {
                // When caustics are disabled, disable glossy and specular components after a diffuse bounce.
                // Note that accept_scattering() is later going to return false in this case.
                if (!m_params.m_enable_caustics && vertex.m_prev_mode == ScatteringMode::Diffuse)
                    vertex.m_scattering_modes &= ~(ScatteringMode::Glossy | ScatteringMode::Specular);

                // Terminate the path if all scattering modes are disabled.
                if (vertex.m_scattering_modes == ScatteringMode::None)
                    return;
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
                ShadingComponents&      path_radiance)
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

            void on_miss(const PathVertex& vertex)
            {
                assert(vertex.m_prev_mode != ScatteringMode::None);

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
                m_path_radiance.add_emission(
                    vertex.m_path_length,
                    vertex.m_aov_mode,
                    env_radiance);
            }

            void on_hit(const PathVertex& vertex)
            {
                // Emitted light contribution.
                if ((!m_omit_emitted_light || m_params.m_enable_caustics) &&
                    vertex.m_edf &&
                    vertex.m_cos_on > 0.0 &&
                    (vertex.m_path_length > 2 || m_params.m_enable_dl) &&
                    (vertex.m_path_length < 2 || (vertex.m_edf->get_flags() & EDF::CastIndirectLight)))
                {
                    // Compute the emitted radiance.
                    Spectrum emitted_radiance(0.0f, Spectrum::Illuminance);
                    add_emitted_light_contribution(vertex, emitted_radiance);

                    // Update the path radiance.
                    emitted_radiance *= vertex.m_throughput;
                    m_path_radiance.add_emission(
                        vertex.m_path_length,
                        vertex.m_aov_mode,
                        emitted_radiance);
                }
            }

            void on_scatter(PathVertex& vertex)
            {
                assert(vertex.m_scattering_modes != ScatteringMode::None);

                // Any light contribution after a diffuse or glossy bounce is considered indirect.
                if (ScatteringMode::has_diffuse_or_glossy(vertex.m_prev_mode))
                    m_is_indirect_lighting = true;

                // When caustics are disabled, disable glossy and specular components after a diffuse bounce.
                if (!m_params.m_enable_caustics && vertex.m_prev_mode == ScatteringMode::Diffuse)
                    vertex.m_scattering_modes &= ~(ScatteringMode::Glossy | ScatteringMode::Specular);

                // Terminate the path if all scattering modes are disabled.
                if (vertex.m_scattering_modes == ScatteringMode::None)
                    return;

                ShadingComponents vertex_radiance(Spectrum::Illuminance);

                if (vertex.m_bssrdf == 0)
                {
                    // If we have an OSL shader, we need to choose one of the closures and set
                    // its shading basis into the shading point for the DirectLightingIntegrator
                    // to use it.
                    if (m_params.m_enable_dl || m_params.m_enable_ibl)
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

                // Direct lighting contribution.
                if (m_params.m_enable_dl || vertex.m_path_length > 1)
                {
                    if (vertex.m_bsdf)
                    {
                        add_direct_lighting_contribution_bsdf(
                            *vertex.m_shading_point,
                            vertex.m_outgoing,
                            *vertex.m_bsdf,
                            vertex.m_bsdf_data,
                            vertex.m_scattering_modes,
                            vertex_radiance);
                    }
                }

                // Image-based lighting contribution.
                if (m_params.m_enable_ibl && m_env_edf)
                {
                    if (vertex.m_bsdf)
                    {
                        add_image_based_lighting_contribution_bsdf(
                            *vertex.m_shading_point,
                            vertex.m_outgoing,
                            *vertex.m_bsdf,
                            vertex.m_bsdf_data,
                            vertex.m_scattering_modes,
                            vertex_radiance);
                    }
                }

                // Apply path throughput.
                vertex_radiance *= vertex.m_throughput;

                // Optionally clamp secondary rays contribution.
                if (m_params.m_has_max_ray_intensity && vertex.m_path_length > 1)
                    clamp_contribution(vertex_radiance);

                // Update path radiance.
                if (vertex.m_path_length == 1)
                    m_path_radiance += vertex_radiance;
                else
                    m_path_radiance.add_to_component(vertex.m_aov_mode, vertex_radiance);
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

            void add_direct_lighting_contribution_bsdf(
                const ShadingPoint&     shading_point,
                const Dual3d&           outgoing,
                const BSDF&             bsdf,
                const void*             bsdf_data,
                const int               scattering_modes,
                ShadingComponents&      vertex_radiance)
            {
                ShadingComponents dl_radiance(Spectrum::Illuminance);

                const size_t light_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_dl_light_sample_count);

                // No light samples has to be made.
                if (light_sample_count == 0) return;

                const BSDFSampler bsdf_sampler(
                    bsdf,
                    bsdf_data,
                    scattering_modes,   // bsdf_sampling_modes (unused)
                    shading_point);

                // This path will be extended via BSDF sampling: sample the lights only.
                const DirectLightingIntegrator integrator(
                    m_shading_context,
                    m_light_sampler,
                    shading_point,
                    bsdf_sampler,
                    shading_point.get_time(),
                    scattering_modes,   // light_sampling_modes
                    1,                  // material_sample_count
                    light_sample_count,
                    m_params.m_dl_low_light_threshold,
                    m_is_indirect_lighting);
                integrator.compute_outgoing_radiance_light_sampling_low_variance(
                    m_sampling_context,
                    MISPower2,
                    outgoing,
                    dl_radiance);

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_dl_light_sample_count > 0.0f)
                    dl_radiance *= m_params.m_rcp_dl_light_sample_count;

                // Add direct lighting contribution.
                vertex_radiance += dl_radiance;
            }

            void add_image_based_lighting_contribution_bsdf(
                const ShadingPoint&     shading_point,
                const Dual3d&           outgoing,
                const BSDF&             bsdf,
                const void*             bsdf_data,
                const int               scattering_modes,
                ShadingComponents&      vertex_radiance)
            {
                ShadingComponents ibl_radiance(Spectrum::Illuminance);

                const size_t env_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_ibl_env_sample_count);

                const BSDFSampler bsdf_sampler(
                    bsdf,
                    bsdf_data,
                    scattering_modes,   // bsdf_sampling_modes (unused)
                    shading_point);

                // This path will be extended via BSDF sampling: sample the environment only.
                compute_ibl_environment_sampling(
                    m_sampling_context,
                    m_shading_context,
                    *m_env_edf,
                    outgoing,
                    bsdf_sampler,
                    scattering_modes,
                    1,                  // bsdf_sample_count
                    env_sample_count,
                    ibl_radiance);

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_ibl_env_sample_count > 0.0f)
                    ibl_radiance *= m_params.m_rcp_ibl_env_sample_count;

                // Add image-based lighting contribution.
                vertex_radiance += ibl_radiance;
            }

            void clamp_contribution(Spectrum& radiance) const
            {
                const float avg = average_value(radiance);

                if (avg > m_params.m_max_ray_intensity)
                    radiance *= m_params.m_max_ray_intensity / avg;
            }

            void clamp_contribution(ShadingComponents& radiance) const
            {
                // Clamp all components.
                clamp_contribution(radiance.m_diffuse);
                clamp_contribution(radiance.m_glossy);
                clamp_contribution(radiance.m_volume);
                clamp_contribution(radiance.m_emission);

                // Rebuild the beauty component.
                radiance.m_beauty  = radiance.m_diffuse;
                radiance.m_beauty += radiance.m_glossy;
                radiance.m_beauty += radiance.m_volume;
                radiance.m_beauty += radiance.m_emission;
            }
        };

        //
        // Volume visitor that performs distance sampling and light sampling in a decoupled fashion.
        //

        struct VolumeVisitorDistanceSampling
        {
            const Parameters&           m_params;
            const LightSampler&         m_light_sampler;
            SamplingContext&            m_sampling_context;
            const ShadingContext&       m_shading_context;
            ShadingComponents&          m_path_radiance;
            const EnvironmentEDF*       m_env_edf;
            bool                        m_is_indirect_lighting;

            VolumeVisitorDistanceSampling(
                const Parameters&       params,
                const LightSampler&     light_sampler,
                SamplingContext&        sampling_context,
                const ShadingContext&   shading_context,
                const Scene&            scene,
                ShadingComponents&      path_radiance)
              : m_params(params)
              , m_light_sampler(light_sampler)
              , m_sampling_context(sampling_context)
              , m_shading_context(shading_context)
              , m_path_radiance(path_radiance)
              , m_env_edf(scene.get_environment()->get_environment_edf())
              , m_is_indirect_lighting(false)
            {
            }

            void add_direct_lighting_contribution(
                const ShadingPoint&     shading_point,
                const ShadingRay&       volume_ray,
                const PhaseFunction&    phase_function,
                const void*             phase_function_data,
                const float             distance_sample,
                const int               scattering_modes,
                ShadingComponents&      radiance)
            {
                ShadingComponents dl_radiance(Spectrum::Illuminance);

                const size_t light_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_dl_light_sample_count);

                // No light samples has to be made.
                if (light_sample_count == 0) return;

                const PhaseFunctionSampler phase_function_sampler(
                    volume_ray,
                    phase_function,
                    phase_function_data,
                    distance_sample);

                const DirectLightingIntegrator integrator(
                    m_shading_context,
                    m_light_sampler,
                    shading_point,
                    phase_function_sampler,
                    volume_ray.m_time,
                    scattering_modes,   // light sampling modes
                    1,                  // phase function sample count
                    light_sample_count,
                    m_params.m_dl_low_light_threshold,
                    m_is_indirect_lighting);
                integrator.compute_outgoing_radiance_combined_sampling_low_variance(
                    m_sampling_context,
                    Dual3d(volume_ray.m_dir),
                    dl_radiance);

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_dl_light_sample_count > 0.0f)
                    dl_radiance *= m_params.m_rcp_dl_light_sample_count;

                // Add direct lighting contribution.
                radiance += dl_radiance;
            }

            void add_image_based_lighting_contribution(
                const ShadingRay&       volume_ray,
                const PhaseFunction&    phase_function,
                const void*             phase_function_data,
                const float             distance_sample,
                ShadingComponents&      radiance)
            {
                const PhaseFunctionSampler phase_function_sampler(
                    volume_ray,
                    phase_function,
                    phase_function_data,
                    distance_sample);

                ShadingComponents ibl_radiance(Spectrum::Illuminance);

                const size_t env_sample_count =
                    stochastic_cast<size_t>(
                    m_sampling_context,
                    m_params.m_ibl_env_sample_count);

                compute_ibl_environment_sampling(
                    m_sampling_context,
                    m_shading_context,
                    *m_env_edf,
                    Dual3d(volume_ray.m_dir),
                    phase_function_sampler,
                    ScatteringMode::All,
                    1,                  // bsdf_sample_count
                    env_sample_count,
                    ibl_radiance);

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_ibl_env_sample_count > 0.0f)
                    ibl_radiance *= m_params.m_rcp_ibl_env_sample_count;

                // Add image-based lighting contribution.
                radiance += ibl_radiance;
            }

            void visit(PathVertex& vertex, const ShadingRay& volume_ray)
            {
                const ShadingRay::Medium* medium = volume_ray.get_current_medium();
                assert(medium != nullptr);
                const PhaseFunction* phase_function = medium->get_phase_function();
                assert(phase_function != nullptr);

                // Any light contribution after a diffuse or glossy bounce is considered indirect.
                if (ScatteringMode::has_diffuse_or_glossy(vertex.m_scattering_modes))
                    m_is_indirect_lighting = true;

                Spectrum ray_transmission;
                phase_function->evaluate_transmission(
                    volume_ray, vertex.m_phase_function_data, ray_transmission);

                const float density = 1.0f - min_value(ray_transmission);
                const float distance_sample_count_float = density * m_params.m_distance_sample_count;
                const size_t distance_sample_count = stochastic_cast<size_t>(
                    m_sampling_context, distance_sample_count_float);
                if (distance_sample_count == 0) return;

                for (size_t i = 0; i < distance_sample_count; ++i)
                {
                    ShadingComponents radiance(Spectrum::Illuminance);

                    // Sample distance.
                    float distance_sample;
                    const float distance_prob = phase_function->sample_distance(
                        m_sampling_context,
                        volume_ray,
                        vertex.m_phase_function_data,
                        distance_sample);

                    Spectrum transmission;
                    phase_function->evaluate_transmission(
                        volume_ray, vertex.m_phase_function_data, distance_sample, transmission);
                    const float extinction = phase_function->extinction_multiplier(
                        volume_ray, vertex.m_phase_function_data, distance_sample);
                    if (extinction == 0.0f) continue;

                    if (m_params.m_enable_dl || vertex.m_path_length > 1)
                    {
                        add_direct_lighting_contribution(
                            *vertex.m_shading_point,
                            volume_ray,
                            *phase_function,
                            vertex.m_phase_function_data,
                            distance_sample,
                            vertex.m_scattering_modes,
                            radiance);
                    }
                    if (m_params.m_enable_ibl && m_env_edf)
                    {
                        add_image_based_lighting_contribution(
                            volume_ray,
                            *phase_function,
                            vertex.m_phase_function_data,
                            distance_sample,
                            radiance);
                    }

                    radiance *= vertex.m_throughput;
                    radiance *= transmission;
                    madd(m_path_radiance, radiance, distance_sample_count_float / distance_prob);
                }
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
        "max_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "0")
            .insert("label", "Max Bounces")
            .insert("help", "Maximum number of bounces"));

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
        "max_glossy_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "0")
            .insert("label", "Max Glossy Bounces")
            .insert("help", "Maximum number of glossy bounces"));

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

    metadata.dictionaries().insert(
        "volume_distance_samples",
        Dictionary()
            .insert("type", "int")
            .insert("default", "4")
            .insert("unlimited", "true")
            .insert("min", "1")
            .insert("label", "Distance Samples")
            .insert("help", "Number of distance samples per ray for volume rendering"));

    return metadata;
}

}   // namespace renderer
