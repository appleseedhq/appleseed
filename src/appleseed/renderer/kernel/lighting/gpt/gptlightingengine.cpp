
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Stephen Agyemang, The appleseedhq Organization
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
#include "gptlightingengine.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovcomponents.h"
#include "renderer/kernel/lighting/directlightingintegrator.h"
#include "renderer/kernel/lighting/imagebasedlighting.h"
#include "renderer/kernel/lighting/lightpathrecorder.h"
#include "renderer/kernel/lighting/lightpathstream.h"
#include "renderer/kernel/lighting/guidedpathtracer.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/lighting/volumelightingintegrator.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/spectrumclamp.h"
#include "renderer/utility/stochasticcast.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/mis.h"
#include "foundation/math/population.h"
#include "foundation/math/vector.h"
#include "foundation/string/string.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <string>

// Forward declarations.
namespace renderer  { class BackwardLightSampler; }
namespace renderer  { class PixelContext; }
namespace renderer  { class TextureCache; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Guided Path Tracing lighting engine.
    //
    // Implementation of "Practical Path Guiding for Efficient Light-Transport Simulation" [Müller et al. 2017].
    //

    class GPTLightingEngine
      : public ILightingEngine
    {
      public:
        GPTLightingEngine(
            STree*                          sd_tree,
            const BackwardLightSampler&     light_sampler,
            LightPathRecorder&              light_path_recorder,
            const GPTParameters&            params)
          : m_sd_tree(sd_tree)
          , m_params(params)
          , m_light_sampler(light_sampler)
          , m_light_path_stream(
              m_params.m_record_light_paths
                  ? light_path_recorder.create_stream()
                  : nullptr)
          , m_path_count(0)
          , m_inf_volume_ray_warnings(0)
        {
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
            m_params.print();
        }

        void compute_lighting(
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingComponents&      radiance,               // output radiance, in W.sr^-1.m^-2
            AOVComponents&          aov_components) override
        {
            if (m_light_path_stream)
            {
                m_light_path_stream->begin_path(
                    pixel_context,
                    shading_point.get_scene().get_render_data().m_active_camera,
                    shading_point.get_ray().m_org);
            }

            if (m_params.m_next_event_estimation)
            {
                do_compute_lighting<PathVisitorNextEventEstimation, VolumeVisitorDistanceSampling>(
                    sampling_context,
                    shading_context,
                    shading_point,
                    radiance,
                    aov_components);
            }
            else
            {
                do_compute_lighting<PathVisitorSimple, VolumeVisitorSimple>(
                    sampling_context,
                    shading_context,
                    shading_point,
                    radiance,
                    aov_components);
            }

            if (m_light_path_stream)
                m_light_path_stream->end_path();
        }

        template <typename PathVisitor, typename VolumeVisitor>
        void do_compute_lighting(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingComponents&      radiance,               // output radiance, in W.sr^-1.m^-2
            AOVComponents&          aov_components)
        {
            PathVisitor path_visitor(
                m_sd_tree,
                m_params,
                m_light_sampler,
                sampling_context,
                shading_context,
                shading_point.get_scene(),
                radiance,
                aov_components,
                m_light_path_stream);

            VolumeVisitor volume_visitor(
                m_params,
                m_light_sampler,
                sampling_context,
                shading_context,
                shading_point.get_scene(),
                radiance,
                m_inf_volume_ray_warnings);

            GuidedPathTracer<PathVisitor, VolumeVisitor, false> path_tracer(     // false = not adjoint
                m_sd_tree,
                path_visitor,
                volume_visitor,
                m_params.m_bsdf_sampling_fraction_mode,
                m_params.m_guided_bounce_mode,
                m_params.m_rr_min_path_length,
                m_params.m_max_bounces == ~size_t(0) ? ~size_t(0) : m_params.m_max_bounces + 1,
                m_params.m_max_guided_bounces,
                m_params.m_max_diffuse_bounces == ~size_t(0) ? ~size_t(0) : m_params.m_max_diffuse_bounces + 1,
                m_params.m_max_glossy_bounces,
                m_params.m_max_specular_bounces,
                m_params.m_max_volume_bounces,
                m_params.m_clamp_roughness,
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

        StatisticsVector get_statistics() const override
        {
            Statistics stats;
            stats.insert("path count", m_path_count);
            stats.insert("path length", m_path_length);

            return StatisticsVector::make("path tracing statistics", stats);
        }

      private:
        const GPTParameters             m_params;
        STree*                          m_sd_tree;
        const BackwardLightSampler&     m_light_sampler;
        LightPathStream*                m_light_path_stream;

        std::uint64_t                   m_path_count;
        Population<std::uint64_t>       m_path_length;

        size_t                          m_inf_volume_ray_warnings;
        static const size_t             MaxInfVolumeRayWarnings = 5;

        //
        // Base path visitor.
        //

        class PathVisitorBase
        {
          public:
            void on_first_diffuse_bounce(
                const PathVertex&           vertex,
                const Spectrum&             albedo)
            {
                m_aov_components.m_albedo = albedo;
            }

            bool accept_scattering(
                const ScatteringMode::Mode  prev_mode,
                const ScatteringMode::Mode  next_mode)
            {
                assert(next_mode != ScatteringMode::None);

                if (!m_params.m_enable_caustics)
                {
                    // Don't follow paths leading to caustics.
                    if (ScatteringMode::has_diffuse_or_volume(prev_mode) &&
                        ScatteringMode::has_glossy_or_specular(next_mode))
                        return false;

                    // Ignore light emission after glossy-to-specular bounces to prevent another class of fireflies.
                    if (ScatteringMode::has_glossy(prev_mode) &&
                        ScatteringMode::has_specular(next_mode))
                        m_omit_emitted_light = true;
                }

                return true;
            }

          protected:
            const GPTParameters&                m_params;
            const BackwardLightSampler&         m_light_sampler;
            SamplingContext&                    m_sampling_context;
            const ShadingContext&               m_shading_context;
            const EnvironmentEDF*               m_env_edf;
            ShadingComponents&                  m_path_radiance;
            AOVComponents&                      m_aov_components;
            LightPathStream*                    m_light_path_stream;
            bool                                m_omit_emitted_light;
            STree*                              m_sd_tree;

            PathVisitorBase(
                STree*                          sd_tree,
                const GPTParameters&            params,
                const BackwardLightSampler&     light_sampler,
                SamplingContext&                sampling_context,
                const ShadingContext&           shading_context,
                const Scene&                    scene,
                ShadingComponents&              path_radiance,
                AOVComponents&                  aov_components,
                LightPathStream*                light_path_stream)
              : m_params(params)
              , m_light_sampler(light_sampler)
              , m_sampling_context(sampling_context)
              , m_shading_context(shading_context)
              , m_env_edf(scene.get_environment()->get_environment_edf())
              , m_path_radiance(path_radiance)
              , m_aov_components(aov_components)
              , m_light_path_stream(light_path_stream)
              , m_omit_emitted_light(false)
              , m_sd_tree(sd_tree)
            {
            }
        };

        //
        // Path visitor without next event estimation.
        //

        class PathVisitorSimple
          : public PathVisitorBase
        {
          public:
            PathVisitorSimple(
                STree*                          sd_tree,
                const GPTParameters&            params,
                const BackwardLightSampler&     light_sampler,
                SamplingContext&                sampling_context,
                const ShadingContext&           shading_context,
                const Scene&                    scene,
                ShadingComponents&              path_radiance,
                AOVComponents&                  aov_components,
                LightPathStream*                light_path_stream)
              : PathVisitorBase(
                    sd_tree,
                    params,
                    light_sampler,
                    sampling_context,
                    shading_context,
                    scene,
                    path_radiance,
                    aov_components,
                    light_path_stream)
            {
            }

            void on_miss(const PathVertex &vertex, GPTVertexPath& guided_path)
            {
                assert(vertex.m_prev_mode != ScatteringMode::None);

                // Can't look up the environment if there's no environment EDF.
                if (m_env_edf == nullptr)
                    return;

                // When IBL is disabled, the environment should still be reflected by glossy and specular surfaces.
                if (!m_params.m_enable_ibl && vertex.m_prev_mode == ScatteringMode::Diffuse)
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
                
                guided_path.add_radiance(env_radiance);
            }

            void on_hit(const PathVertex &vertex, GPTVertexPath& guided_path)
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

                    // Record light path event.
                    if (m_light_path_stream)
                        m_light_path_stream->hit_emitter(vertex, emitted_radiance);

                    // Apply path throughput.
                    emitted_radiance *= vertex.m_throughput;

                    // Update path radiance.
                    m_path_radiance.add_emission(
                        vertex.m_path_length,
                        vertex.m_aov_mode,
                        emitted_radiance);
                    
                    guided_path.add_radiance(emitted_radiance);
                }
                else
                {
                    // Record light path event.
                    if (m_light_path_stream)
                        m_light_path_stream->hit_reflector(vertex);
                }
            }

            void on_scatter(
                PathVertex&                 vertex,
                GPTVertexPath&              guided_path,
                const float                 bsdf_sampling_fraction,
                const bool                  enable_path_guiding)
            {
                // When caustics are disabled, disable glossy and specular components after a diffuse or volume bounce.
                // Note that accept_scattering() is later going to return false in this case.
                if (!m_params.m_enable_caustics)
                {
                    if (vertex.m_prev_mode == ScatteringMode::Diffuse ||
                        vertex.m_prev_mode == ScatteringMode::Volume)
                        vertex.m_scattering_modes &= ~(ScatteringMode::Glossy | ScatteringMode::Specular);
                }
            }
        };

        //
        // Path visitor with next event estimation.
        //

        class PathVisitorNextEventEstimation
          : public PathVisitorBase
        {
          public:
            PathVisitorNextEventEstimation(
                STree*                          sd_tree,
                const GPTParameters&            params,
                const BackwardLightSampler&     light_sampler,
                SamplingContext&                sampling_context,
                const ShadingContext&           shading_context,
                const Scene&                    scene,
                ShadingComponents&              path_radiance,
                AOVComponents&                  aov_components,
                LightPathStream*                light_path_stream)
              : PathVisitorBase(
                    sd_tree,
                    params,
                    light_sampler,
                    sampling_context,
                    shading_context,
                    scene,
                    path_radiance,
                    aov_components,
                    light_path_stream)
              , m_is_indirect_lighting(false)
            {
            }

            void on_miss(const PathVertex &vertex, GPTVertexPath& guided_path)
            {
                assert(vertex.m_prev_mode != ScatteringMode::None);

                // Can't look up the environment if there's no environment EDF.
                if (m_env_edf == nullptr)
                    return;

                // When IBL is disabled, the environment should still be reflected by glossy and specular surfaces.
                if (!m_params.m_enable_ibl && vertex.m_prev_mode == ScatteringMode::Diffuse)
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
                if (env_prob == 0.0f)
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
                if (m_params.m_has_max_ray_intensity && vertex.m_path_length > 1 && vertex.m_prev_mode != ScatteringMode::Specular)
                    clamp_contribution(env_radiance, m_params.m_max_ray_intensity);

                // Update path radiance.
                m_path_radiance.add_emission(
                    vertex.m_path_length,
                    vertex.m_aov_mode,
                    env_radiance);

                if(!m_params.m_enable_ibl)
                    guided_path.add_radiance(env_radiance);
                else
                    guided_path.add_indirect_radiance(env_radiance);

            }

            void on_hit(const PathVertex& vertex, GPTVertexPath& guided_path)
            {
                // Emitted light contribution.
                if ((!m_omit_emitted_light || m_params.m_enable_caustics) &&
                    vertex.m_edf &&
                    vertex.m_cos_on > 0.0 &&
                    (vertex.m_path_length > 2 || m_params.m_enable_dl) &&
                    (vertex.m_path_length < 2 || (vertex.m_edf->get_flags() & EDF::CastIndirectLight)))
                {
                    // Compute the emitted radiance.
                    Spectrum emitted_radiance(0.0f);
                    add_emitted_light_contribution(vertex, emitted_radiance);

                    // Record light path event.
                    if (m_light_path_stream)
                        m_light_path_stream->hit_emitter(vertex, emitted_radiance);

                    // Apply path throughput.
                    emitted_radiance *= vertex.m_throughput;

                    // Optionally clamp secondary rays contribution.
                    if (m_params.m_has_max_ray_intensity && vertex.m_path_length > 1 && vertex.m_prev_mode != ScatteringMode::Specular)
                        clamp_contribution(emitted_radiance, m_params.m_max_ray_intensity);

                    // Update path radiance.
                    m_path_radiance.add_emission(
                        vertex.m_path_length,
                        vertex.m_aov_mode,
                        emitted_radiance);
                    
                    guided_path.add_indirect_radiance(emitted_radiance);
                }
                else
                {
                    // Record light path event.
                    if (m_light_path_stream)
                        m_light_path_stream->hit_reflector(vertex);
                }
            }

            void on_scatter(
                PathVertex&                 vertex,
                GPTVertexPath&              guided_path,
                const float                 bsdf_sampling_fraction,
                const bool                  enable_path_guiding)
            {
                assert(vertex.m_scattering_modes != ScatteringMode::None);

                // Any light contribution after a diffuse or glossy bounce is considered indirect.
                if (ScatteringMode::has_diffuse_or_glossy_or_volume(vertex.m_prev_mode))
                    m_is_indirect_lighting = true;

                // When caustics are disabled, disable glossy and specular components after a diffuse or volume bounce.
                if (!m_params.m_enable_caustics)
                {
                    if (vertex.m_prev_mode == ScatteringMode::Diffuse ||
                        vertex.m_prev_mode == ScatteringMode::Volume)
                        vertex.m_scattering_modes &= ~(ScatteringMode::Glossy | ScatteringMode::Specular);
                }

                // Terminate the path if all scattering modes are disabled.
                if (vertex.m_scattering_modes == ScatteringMode::None)
                    return;

                DirectShadingComponents vertex_radiance;

                if (vertex.m_bssrdf == nullptr)
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
                            // todo: don't split if there's only one closure.
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
                            vertex_radiance,
                            m_light_path_stream,
                            bsdf_sampling_fraction,
                            enable_path_guiding);
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
                            vertex_radiance,
                            m_light_path_stream,
                            bsdf_sampling_fraction,
                            enable_path_guiding);
                    }
                }

                // Apply path throughput.
                vertex_radiance *= vertex.m_throughput;

                // Optionally clamp secondary rays contribution.
                if (m_params.m_has_max_ray_intensity && vertex.m_path_length > 1 && vertex.m_prev_mode != ScatteringMode::Specular)
                    clamp_contribution(vertex_radiance, m_params.m_max_ray_intensity);

                // Update path radiance.
                m_path_radiance.add(
                    vertex.m_path_length,
                    vertex.m_aov_mode,
                    vertex_radiance);

                guided_path.add_radiance(vertex_radiance.m_beauty);
            }

          private:
            bool   m_is_indirect_lighting;

            void add_emitted_light_contribution(
                const PathVertex&           vertex,
                Spectrum&                   vertex_radiance)
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
                const ShadingPoint&         shading_point,
                const Dual3d&               outgoing,
                const BSDF&                 bsdf,
                const void*                 bsdf_data,
                const int                   scattering_modes,
                DirectShadingComponents&    vertex_radiance,
                LightPathStream*            light_path_stream,
                const float                 bsdf_sampling_fraction,
                const bool                  enable_path_guiding)
            {
                DirectShadingComponents dl_radiance;

                const size_t light_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_dl_light_sample_count);

                if (light_sample_count == 0)
                    return;

                const PathGuidedSampler path_guided_sampler(
                    enable_path_guiding,
                    m_params.m_guided_bounce_mode,
                    m_sd_tree->get_d_tree(foundation::Vector3f(shading_point.get_point())),
                    bsdf_sampling_fraction,
                    bsdf,
                    bsdf_data,
                    scattering_modes,       // bsdf_sampling_modes (unused)
                    shading_point,
                    m_sd_tree->is_built());

                // This path will be extended via BSDF sampling: sample the lights only.
                const DirectLightingIntegrator integrator(
                    m_shading_context,
                    m_light_sampler,
                    path_guided_sampler,
                    shading_point.get_time(),
                    scattering_modes,       // light_sampling_modes
                    1,                      // material_sample_count
                    light_sample_count,
                    m_params.m_dl_low_light_threshold,
                    m_is_indirect_lighting);
                integrator.compute_outgoing_radiance_light_sampling_low_variance(
                    m_sampling_context,
                    MISPower2,
                    outgoing,
                    dl_radiance,
                    light_path_stream);

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_dl_light_sample_count > 0.0f)
                    dl_radiance *= m_params.m_rcp_dl_light_sample_count;

                // Add direct lighting contribution.
                vertex_radiance += dl_radiance;
            }

            void add_image_based_lighting_contribution_bsdf(
                const ShadingPoint&         shading_point,
                const Dual3d&               outgoing,
                const BSDF&                 bsdf,
                const void*                 bsdf_data,
                const int                   scattering_modes,
                DirectShadingComponents&    vertex_radiance,
                LightPathStream*            light_path_stream,
                const float                 bsdf_sampling_fraction,
                const bool                  enable_path_guiding)
            {
                DirectShadingComponents ibl_radiance;

                const size_t env_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_ibl_env_sample_count);

                const PathGuidedSampler path_guided_sampler(
                    enable_path_guiding,
                    m_params.m_guided_bounce_mode,
                    m_sd_tree->get_d_tree(foundation::Vector3f(shading_point.get_point())),
                    bsdf_sampling_fraction,
                    bsdf,
                    bsdf_data,
                    scattering_modes, // bsdf_sampling_modes (unused)
                    shading_point,
                    m_sd_tree->is_built());

                // This path will be extended via BSDF sampling: sample the environment only.
                compute_ibl_environment_sampling(
                    m_sampling_context,
                    m_shading_context,
                    *m_env_edf,
                    outgoing,
                    path_guided_sampler,
                    scattering_modes,
                    1,                      // bsdf_sample_count
                    env_sample_count,
                    ibl_radiance,
                    light_path_stream);

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_ibl_env_sample_count > 0.0f)
                    ibl_radiance *= m_params.m_rcp_ibl_env_sample_count;

                // Add image-based lighting contribution.
                vertex_radiance += ibl_radiance;
            }
        };

        //
        // Base volume visitor.
        //

        class VolumeVisitorBase
        {
          public:
            bool accept_scattering(const ScatteringMode::Mode prev_mode)
            {
                return true;
            }

            void on_scatter(PathVertex& vertex)
            {
                // When caustics are disabled, disable glossy and specular components after a diffuse or volume bounce.
                // Note that accept_scattering() is later going to return false in this case.
                if (!m_params.m_enable_caustics)
                {
                    if (vertex.m_prev_mode == ScatteringMode::Diffuse ||
                        vertex.m_prev_mode == ScatteringMode::Volume)
                        vertex.m_scattering_modes &= ~(ScatteringMode::Glossy | ScatteringMode::Specular);
                }
            }

          protected:
            const GPTParameters&                m_params;
            const BackwardLightSampler&         m_light_sampler;
            SamplingContext&                    m_sampling_context;
            const ShadingContext&               m_shading_context;
            ShadingComponents&                  m_path_radiance;
            const EnvironmentEDF*               m_env_edf;
            bool                                m_is_indirect_lighting;
            size_t&                             m_inf_volume_ray_warnings;

            VolumeVisitorBase(
                const GPTParameters&            params,
                const BackwardLightSampler&     light_sampler,
                SamplingContext&                sampling_context,
                const ShadingContext&           shading_context,
                const Scene&                    scene,
                ShadingComponents&              path_radiance,
                size_t&                         inf_volume_ray_warnings)
              : m_params(params)
              , m_light_sampler(light_sampler)
              , m_sampling_context(sampling_context)
              , m_shading_context(shading_context)
              , m_path_radiance(path_radiance)
              , m_env_edf(scene.get_environment()->get_environment_edf())
              , m_is_indirect_lighting(false)
              , m_inf_volume_ray_warnings(inf_volume_ray_warnings)
            {
            }
        };

        //
        // Volume visitor without next event estimation.
        //

        class VolumeVisitorSimple
          : public VolumeVisitorBase
        {
          public:
            VolumeVisitorSimple(
                const GPTParameters&            params,
                const BackwardLightSampler&     light_sampler,
                SamplingContext&                sampling_context,
                const ShadingContext&           shading_context,
                const Scene&                    scene,
                ShadingComponents&              path_radiance,
                size_t&                         inf_volume_ray_warnings)
              : VolumeVisitorBase(
                  params,
                  light_sampler,
                  sampling_context,
                  shading_context,
                  scene,
                  path_radiance,
                  inf_volume_ray_warnings)
            {
            }

            void visit_ray(PathVertex& vertex, const ShadingRay& volume_ray, GPTVertexPath& guided_path)
            {
                // Any light contribution after a diffuse, glossy or volume bounce is considered indirect.
                if (ScatteringMode::has_diffuse_or_glossy_or_volume(vertex.m_scattering_modes))
                    m_is_indirect_lighting = true;
            }
        };

        //
        // Volume visitor with next event estimation.
        //

        class VolumeVisitorDistanceSampling
          : public VolumeVisitorBase
        {
          public:
            VolumeVisitorDistanceSampling(
                const GPTParameters&            params,
                const BackwardLightSampler&     light_sampler,
                SamplingContext&                sampling_context,
                const ShadingContext&           shading_context,
                const Scene&                    scene,
                ShadingComponents&              path_radiance,
                size_t&                         inf_volume_ray_warnings)
              : VolumeVisitorBase(
                  params,
                  light_sampler,
                  sampling_context,
                  shading_context,
                  scene,
                  path_radiance,
                  inf_volume_ray_warnings)
            {
            }

            float sample_distance(
                const ShadingRay&           volume_ray,
                const float                 extinction,
                float&                      distance)
            {
                m_sampling_context.split_in_place(1, 1);

                if (!volume_ray.is_finite())
                {
                    // Sample distance.
                    distance = sample_exponential_distribution(
                        m_sampling_context.next2<float>(), extinction);

                    // Calculate PDF of this distance sample.
                    return exponential_distribution_pdf(distance, extinction);
                }
                else
                {
                    // Sample distance.
                    const float ray_length = static_cast<float>(volume_ray.get_length());
                    distance = sample_exponential_distribution_on_segment(
                        m_sampling_context.next2<float>(), extinction, 0.0f, ray_length);

                    // Calculate PDF of this distance sample.
                    return exponential_distribution_on_segment_pdf(
                        distance, extinction, 0.0f, ray_length);
                }
            }

            void visit_ray(PathVertex &vertex, const ShadingRay &volume_ray, GPTVertexPath &guided_path)
            {
                // Any light contribution after a diffuse, glossy or volume bounce is considered indirect.
                if (ScatteringMode::has_diffuse_or_glossy_or_volume(vertex.m_scattering_modes))
                    m_is_indirect_lighting = true;

                if (!volume_ray.is_finite())
                {
                    if (m_inf_volume_ray_warnings < MaxInfVolumeRayWarnings)
                        RENDERER_LOG_WARNING("volume ray of infinite length encountered.");
                    else if (m_inf_volume_ray_warnings == MaxInfVolumeRayWarnings)
                        RENDERER_LOG_WARNING("more volume rays of infinite length found, "
                                             "omitting warning messages for brevity.");
                    ++m_inf_volume_ray_warnings;
                }

                const ShadingRay::Medium* medium = volume_ray.get_current_medium();
                assert(medium != nullptr);
                const Volume* volume = medium->get_volume();
                assert(volume != nullptr);

                const size_t light_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_dl_light_sample_count);

                VolumeLightingIntegrator integrator(
                    m_shading_context,
                    m_light_sampler,
                    *volume,
                    volume_ray,
                    vertex.m_volume_data,
                    *vertex.m_shading_point,
                    vertex.m_scattering_modes,
                    m_params.m_distance_sample_count,
                    light_sample_count,
                    m_params.m_dl_low_light_threshold,
                    m_is_indirect_lighting);

                DirectShadingComponents radiance;
                if (m_params.m_enable_equiangular_sampling)
                {
                    integrator.compute_radiance_combined_sampling(
                        m_sampling_context,
                        MISPower2,
                        radiance);
                }
                else
                {
                    integrator.compute_radiance_exponential_sampling(
                        m_sampling_context,
                        MISPower2,
                        radiance);
                }

                radiance *= vertex.m_throughput;
                m_path_radiance.add(vertex.m_path_length, vertex.m_aov_mode, radiance);

                guided_path.add_radiance(radiance.m_beauty);
            }
        };
    };
}


//
// PTLightingEngineFactory class implementation.
//

Dictionary GPTLightingEngineFactory::get_params_metadata()
{
    Dictionary metadata;

    metadata.dictionaries().insert(
        "spatial_filter",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "stochastic|box|nearest")
            .insert("default", "stochastic")
            .insert("label", "Spatial Filter")
            .insert("help", "Spatial filtering mode for SD-tree recordings")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "stochastic",
                        Dictionary()
                            .insert("label", "Stochastic")
                            .insert("help", "Randomly offset position of recording"))
                    .insert(
                        "box",
                        Dictionary()
                            .insert("label", "Box")
                            .insert("help", "Record radiance in nearby S-tree leaf nodes"))
                    .insert(
                        "nearest",
                        Dictionary()
                            .insert("label", "Nearest")
                            .insert("help", "Record radiance to the nearest S-tree leaf node only"))));

    metadata.dictionaries().insert(
        "directional_filter",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "box|nearest")
            .insert("default", "box")
            .insert("label", "Directional Filter")
            .insert("help", "Directional mode for SD-tree recordings")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "box",
                        Dictionary()
                            .insert("label", "Box")
                            .insert("help", "Record radiance in nearby D-tree leaf nodes"))
                    .insert(
                        "nearest",
                        Dictionary()
                            .insert("label", "Nearest")
                            .insert("help", "Record radiance to the nearest D-tree leaf node only"))));

    metadata.dictionaries().insert(
        "bsdf_sampling_fraction",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "learn|fixed")
            .insert("default", "learn")
            .insert("label", "BSDF Sampling Fraction")
            .insert("help", "BSDF Sampling Fraction Mode")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "learn",
                        Dictionary()
                            .insert("label", "Learn BSDF Sampling Fraction")
                            .insert("help", "Automatically learn the optimal sampling fraction at each spatial leaf node"))
                    .insert(
                        "fixed",
                        Dictionary()
                            .insert("label", "Fixed BSDF Sampling Fraction")
                            .insert("help", "Use a fixed sampling fraction"))));

    metadata.dictionaries().insert(
        "iteration_progression",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "combine|automatic")
            .insert("default", "combine")
            .insert("label", "Learning Iteration Progression")
            .insert("help", "Behavior of the iterative radiance distribution learning")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "combine",
                        Dictionary()
                            .insert("label", "Combine Iterations")
                            .insert("help", "Combine iterations based on their estimated variances"))
                    .insert(
                        "automatic",
                        Dictionary()
                            .insert("label", "Automatic Final Iteration")
                            .insert("help", "Initiate a final iteration when projected variance estimate increases"))));

    metadata.dictionaries().insert(
        "guided_bounce_mode",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "learn|strictly_diffuse|strictly_glossy|prefer_diffuse|prefer_glossy")
            .insert("default", "learn")
            .insert("label", "Scattering Mode for Path Guided Bounces")
            .insert("help", "How path guided bounces should be treated internally")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "learn",
                        Dictionary()
                            .insert("label", "Learned Distribution")
                            .insert("help", "Guided bounce modes are based on the learned radiance distribution"))
                    .insert(
                        "strictly_diffuse",
                        Dictionary()
                            .insert("label", "Strictly Diffuse")
                            .insert("help", "Guided bounces are always treated as Diffuse"))
                    .insert(
                        "strictly_glossy",
                        Dictionary()
                            .insert("label", "Strictly Glossy")
                            .insert("help", "Guided bounces are always treated as Glossy"))
                    .insert(
                        "prefer_diffuse",
                        Dictionary()
                            .insert("label", "Prefer Diffuse")
                            .insert("help", "Guided bounces are treated as Diffuse if remaining modes allow, Glossy otherwise"))
                    .insert(
                        "prefer_glossy",
                        Dictionary()
                            .insert("label", "Prefer Glossy")
                            .insert("help", "Guided bounces are treated as Glossy if remaining modes allow, Diffuse otherwise"))));

    metadata.dictionaries().insert(
        "save_tree_iterations",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "none|all|final")
            .insert("default", "none")
            .insert("label", "Save option for SD tree structure")
            .insert("help", "Set which SD tree iterations to save to disk")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "none",
                        Dictionary()
                            .insert("label", "None")
                            .insert("help", "Do not save the SD tree"))
                    .insert(
                        "all",
                        Dictionary()
                            .insert("label", "All")
                            .insert("help", "Save all SD tree iterations to disk"))
                    .insert(
                        "final",
                        Dictionary()
                            .insert("label", "Final")
                            .insert("help", "Save final SD tree iteration to disk"))));

    metadata.dictionaries().insert(
        "file_path",
        Dictionary()
            .insert("type", "text")
            .insert("default", "")
            .insert("label", "File Path")
            .insert("help", "Path to disk location where files will be saved"));

    metadata.dictionaries().insert(
        "guided_bounce_mode",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "learn|strictly_diffuse|strictly_glossy|prefer_diffuse|prefer_glossy")
            .insert("default", "learn")
            .insert("label", "Scattering Mode for Path Guided Bounces")
            .insert("help", "How path guided bounces should be treated internally")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "learn",
                        Dictionary()
                            .insert("label", "Learned Distribution")
                            .insert("help", "Guided bounce modes are based on the learned radiance distribution"))
                    .insert(
                        "strictly_diffuse",
                        Dictionary()
                            .insert("label", "Strictly Diffuse")
                            .insert("help", "Guided bounces are always treated as Diffuse"))
                    .insert(
                        "strictly_glossy",
                        Dictionary()
                            .insert("label", "Strictly Glossy")
                            .insert("help", "Guided bounces are always treated as Glossy"))
                    .insert(
                        "prefer_diffuse",
                        Dictionary()
                            .insert("label", "Prefer Diffuse")
                            .insert("help", "Guided bounces are treated as Diffuse if remaining modes allow, Glossy otherwise"))
                    .insert(
                        "prefer_glossy",
                        Dictionary()
                            .insert("label", "Prefer Glossy")
                            .insert("help", "Guided bounces are treated as Glossy if remaining modes allow, Diffuse otherwise"))));

    metadata.dictionaries().insert(
        "samples_per_pass",
        Dictionary()
            .insert("type", "int")
            .insert("default", "4")
            .insert("unlimited", "false")
            .insert("min", "1")
            .insert("max", "32")
            .insert("label", "Samples Per Pass")
            .insert("help", "Number of samples for one path guiding pass"));

    metadata.dictionaries().insert(
        "fixed_bsdf_sampling_fraction_value",
        Dictionary()
            .insert("type", "float")
            .insert("default", "0.5")
            .insert("unlimited", "false")
            .insert("min", "0.0")
            .insert("max", "1.0")
            .insert("label", "Fixed BSDF Sampling Fraction")
            .insert("help", "Ratio between BSDF sampling and SD-tree sampling"));

    metadata.dictionaries().insert(
        "learning_rate",
        Dictionary()
            .insert("type", "float")
            .insert("default", "0.01")
            .insert("unlimited", "false")
            .insert("min", "0.001")
            .insert("max", "0.5")
            .insert("label", "BSDF Sampling Fraction Learning Rate")
            .insert("help", "BSDF Sampling Fraction Learning Rate"));

    metadata.dictionaries().insert(
        "dl_light_samples",
        Dictionary()
            .insert("type", "float")
            .insert("default", "1.0")
            .insert("label", "Light Samples")
            .insert("help", "Number of samples used to estimate direct lighting"));

    metadata.dictionaries().insert(
        "enable_dl",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "true")
            .insert("label", "Enable Direct Lighting")
            .insert("help", "Enable direct lighting"));

    metadata.dictionaries().insert(
        "enable_ibl",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "on")
            .insert("label", "Enable IBL")
            .insert("help", "Enable image-based lighting"));

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
        "max_guided_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "0")
            .insert("label", "Max Guided Bounces")
            .insert("help", "Maximum number of path guided bounces"));

    metadata.dictionaries().insert(
        "max_diffuse_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "3")
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
        "max_volume_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "0")
            .insert("unlimited", "false")
            .insert("min", "0")
            .insert("label", "Max Volume Bounces")
            .insert("help", "Maximum number of volume scattering events (0 = single scattering)"));

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
        "dl_light_samples",
        Dictionary()
            .insert("type", "float")
            .insert("default", "1.0")
            .insert("label", "Light Samples")
            .insert("help", "Number of samples used to estimate direct lighting"));

    metadata.dictionaries().insert(
        "dl_low_light_threshold",
        Dictionary()
            .insert("type", "float")
            .insert("default", "0.0")
            .insert("label", "Low Light Threshold")
            .insert("help", "Light contribution threshold to disable shadow rays"));

    metadata.dictionaries().insert(
        "ibl_env_samples",
        Dictionary()
            .insert("type", "float")
            .insert("default", "1.0")
            .insert("label", "IBL Samples")
            .insert("help", "Number of samples used to estimate environment lighting"));

    metadata.dictionaries().insert(
        "clamp_roughness",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "false")
            .insert("label", "Clamp BSDF roughness")
            .insert("help", "Clamp BSDF roughness parameter to a maximum level to reduce fireflies in glossy reflections"));

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
            .insert("default", "2")
            .insert("unlimited", "true")
            .insert("min", "1")
            .insert("label", "Volume Distance Samples")
            .insert("help", "Number of distance samples for volume rendering"));

    metadata.dictionaries().insert(
        "optimize_for_lights_outside_volumes",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "false")
            .insert("label", "Optimize for Lights Outside Volumes")
            .insert("help", "Optimize distance sampling for lights that are located outside volumes"));

    metadata.dictionaries().insert(
        "record_light_paths",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "false")
            .insert("label", "Record Light Paths")
            .insert("help", "Record light paths in memory to later allow visualizing them or saving them to disk"));

    return metadata;
}

GPTLightingEngineFactory::GPTLightingEngineFactory(
    STree*                          sd_tree,
    const BackwardLightSampler&     light_sampler,
    LightPathRecorder&              light_path_recorder,
    const GPTParameters&            params)
  : m_sd_tree(sd_tree)
  , m_light_sampler(light_sampler)
  , m_light_path_recorder(light_path_recorder)
  , m_params(params)
{
}

void GPTLightingEngineFactory::release()
{
    delete this;
}

ILightingEngine* GPTLightingEngineFactory::create()
{
    return
        new GPTLightingEngine(
            m_sd_tree,
            m_light_sampler,
            m_light_path_recorder,
            m_params);
}

}   // namespace renderer
