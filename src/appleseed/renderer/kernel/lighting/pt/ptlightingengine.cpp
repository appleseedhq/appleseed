
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/lighting/directlightingintegrator.h"
#include "renderer/kernel/lighting/imagebasedlighting.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/lighting/subsurfacesampler.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputevaluator.h"
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
            const size_t    m_rr_min_path_length;           // minimum path length before Russian Roulette kicks in, ~0 for unlimited
            const bool      m_next_event_estimation;        // use next event estimation?

            const double    m_dl_light_sample_count;        // number of light samples used to estimate direct illumination
            const double    m_ibl_env_sample_count;         // number of environment samples used to estimate IBL

            const bool      m_has_max_ray_intensity;
            const float     m_max_ray_intensity;

            float           m_rcp_dl_light_sample_count;
            float           m_rcp_ibl_env_sample_count;

            explicit Parameters(const ParamArray& params)
              : m_enable_dl(params.get_optional<bool>("enable_dl", true))
              , m_enable_ibl(params.get_optional<bool>("enable_ibl", true))
              , m_enable_caustics(params.get_optional<bool>("enable_caustics", false))
              , m_max_path_length(nz(params.get_optional<size_t>("max_path_length", 0)))
              , m_rr_min_path_length(nz(params.get_optional<size_t>("rr_min_path_length", 3)))
              , m_next_event_estimation(params.get_optional<bool>("next_event_estimation", true))
              , m_dl_light_sample_count(params.get_optional<double>("dl_light_samples", 1.0))
              , m_ibl_env_sample_count(params.get_optional<double>("ibl_env_samples", 1.0))
              , m_has_max_ray_intensity(params.strings().exist("max_ray_intensity"))
              , m_max_ray_intensity(params.get_optional<float>("max_ray_intensity", 0.0f))
            {
                // Precompute the reciprocal of the number of light samples.
                m_rcp_dl_light_sample_count =
                    m_dl_light_sample_count > 0.0 && m_dl_light_sample_count < 1.0
                        ? static_cast<float>(1.0 / m_dl_light_sample_count)
                        : 0.0f;

                // Precompute the reciprocal of the number of environment samples.
                m_rcp_ibl_env_sample_count =
                    m_ibl_env_sample_count > 0.0 && m_ibl_env_sample_count < 1.0
                        ? static_cast<float>(1.0 / m_ibl_env_sample_count)
                        : 0.0f;
            }

            static size_t nz(const size_t x)
            {
                return x == 0 ? ~0 : x;
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
                    m_max_path_length == ~0 ? "infinite" : pretty_uint(m_max_path_length).c_str(),
                    m_rr_min_path_length == ~0 ? "infinite" : pretty_uint(m_rr_min_path_length).c_str(),
                    m_next_event_estimation ? "on" : "off",
                    pretty_scalar(m_dl_light_sample_count).c_str(),
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
            Spectrum&               radiance,               // output radiance, in W.sr^-1.m^-2
            SpectrumStack&          aovs) APPLESEED_OVERRIDE
        {
            if (m_params.m_next_event_estimation)
            {
                do_compute_lighting<PathVisitorNextEventEstimation>(
                    sampling_context,
                    shading_context,
                    shading_point,
                    radiance,
                    aovs);
            }
            else
            {
                do_compute_lighting<PathVisitorSimple>(
                    sampling_context,
                    shading_context,
                    shading_point,
                    radiance,
                    aovs);
            }
        }

        template <typename PathVisitor>
        void do_compute_lighting(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Spectrum&               radiance,               // output radiance, in W.sr^-1.m^-2
            SpectrumStack&          aovs)
        {
            PathVisitor path_visitor(
                m_params,
                m_light_sampler,
                sampling_context,
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
        // Subsurface sample visitor.
        //
        // Accumulates the contribution of individual subsurface samples.
        //

        class SubsurfaceSampleVisitor
        {
          public:
            SubsurfaceSampleVisitor(
                const LightSampler&     light_sampler,
                SamplingContext&        sampling_context,
                const ShadingContext&   shading_context,
                const EnvironmentEDF*   env_edf,
                const size_t            env_sample_count,
                const PathVertex&       vertex,
                Spectrum&               vertex_radiance,
                SpectrumStack&          vertex_aovs)
              : m_light_sampler(light_sampler)
              , m_sampling_context(sampling_context)
              , m_shading_context(shading_context)
              , m_env_edf(env_edf)
              , m_env_sample_count(env_sample_count)
              , m_vertex(vertex)
              , m_vertex_radiance(vertex_radiance)
              , m_vertex_aovs(vertex_aovs)
            {
            }

            bool visit(
                const BSSRDFSample&     bssrdf_sample,
                const ShadingPoint&     incoming_point,
                const double            probability) const
            {
                // Since the direction in the outside medium--be it the outgoing or the incoming
                // direction--is always fixed, we always need to figure out a refracted direction
                // in the inside medium, and thus we compute both Fresnel coefficients at the
                // incoming and outgoing points using eta (defined as outside IOR / inside IOR).

                const double eta = bssrdf_sample.get_eta();

                // Compute Fresnel coefficient at outgoing point.
                double outgoing_fresnel;
                fresnel_transmittance_dielectric(outgoing_fresnel, eta, m_vertex.m_cos_on);
                if (outgoing_fresnel <= 0.0)
                    return true;

                // Add direct lighting contribution.
                add_dl_contribution(
                    incoming_point,
                    eta,
                    outgoing_fresnel,
                    probability);

                // Add image-based lighting contribution.
                add_ibl_contribution(
                    incoming_point,
                    eta,
                    outgoing_fresnel,
                    probability);

                // Continue visiting samples.
                return true;
            }

          private:
            const LightSampler&         m_light_sampler;
            SamplingContext&            m_sampling_context;
            const ShadingContext&       m_shading_context;
            const EnvironmentEDF*       m_env_edf;
            const size_t                m_env_sample_count;
            const PathVertex&           m_vertex;
            Spectrum&                   m_vertex_radiance;
            SpectrumStack&              m_vertex_aovs;

            void add_dl_contribution(
                const ShadingPoint&     incoming_point,
                const double            eta,
                const double            outgoing_fresnel,
                const double            probability) const
            {
                // Compute irradiance at incoming point.
                Vector3d incoming;
                double transmission;
                double cos_in;
                Spectrum irradiance;
                if (!compute_irradiance(
                        incoming_point,
                        incoming,
                        transmission,
                        cos_in,
                        irradiance))
                    return;

                // Compute Fresnel coefficient at incoming point.
                double incoming_fresnel;
                fresnel_transmittance_dielectric(incoming_fresnel, eta, cos_in);
                if (incoming_fresnel <= 0.0)
                    return;

                // Evaluate the diffusion profile.
                Spectrum rd;
                m_vertex.m_bssrdf->evaluate(
                    m_vertex.m_bssrdf_data,
                    *m_vertex.m_shading_point,
                    m_vertex.m_outgoing.get_value(),
                    incoming_point,
                    incoming,
                    rd);

                // Add the contribution of this sample to the illumination.
                const double weight =
                      RcpPi
                    * transmission
                    * incoming_fresnel
                    * outgoing_fresnel
                    / probability;
                irradiance *= rd;
                irradiance *= static_cast<float>(weight);
                m_vertex_radiance += irradiance;
            }

            void add_ibl_contribution(
                const ShadingPoint&     incoming_point,
                const double            eta,
                const double            outgoing_fresnel,
                const double            probability) const
            {
                if (m_env_edf)
                {
                    Spectrum irradiance;
                    compute_ibl(
                        m_sampling_context,
                        m_shading_context,
                        *m_env_edf,
                        *m_vertex.m_bssrdf,
                        m_vertex.m_bssrdf_data,
                        incoming_point,
                        *m_vertex.m_shading_point,
                        m_vertex.m_outgoing,
                        eta,
                        m_env_sample_count,
                        irradiance);

                    irradiance *= static_cast<float>(outgoing_fresnel / probability);
                    m_vertex_radiance += irradiance;
                }
            }

            // Compute irradiance at a given point of the scene. Works on back faces too.
            bool compute_irradiance(
                const ShadingPoint&     shading_point,
                Vector3d&               incoming,
                double&                 transmission,
                double&                 cos_in,
                Spectrum&               irradiance) const
            {
                if (!m_light_sampler.has_lights_or_emitting_triangles())
                    return false;

                // Sample the lights.
                m_sampling_context.split_in_place(3, 1);
                const Vector3d s = m_sampling_context.next_vector2<3>();
                LightSample light_sample;
                m_light_sampler.sample(shading_point.get_ray().m_time, s, light_sample);

                double attenuation;

                if (light_sample.m_triangle)
                {
                    const Material* material = light_sample.m_triangle->m_material;
                    const EDF* edf = material->get_edf();

                    // todo: check EDF flags.

                    // Compute the incoming direction in world space.
                    incoming = light_sample.m_point - shading_point.get_point();

                    // Cull samples on lights emitting in the wrong direction.
                    double cos_on_light = dot(-incoming, light_sample.m_shading_normal);
                    if (cos_on_light <= 0.0)
                        return false;

                    // Compute the transmission factor between the light sample and the shading point.
                    transmission =
                        m_shading_context.get_tracer().trace_between(
                            shading_point,
                            light_sample.m_point,
                            VisibilityFlags::ShadowRay);

                    // Discard occluded samples.
                    if (transmission == 0.0)
                        return false;

                    // Compute the square distance between the light sample and the shading point.
                    const double square_distance = square_norm(incoming);
                    const double rcp_sample_square_distance = 1.0 / square_distance;

                    // todo: handle light near start.

                    // Normalize the incoming direction.
                    const double rcp_sample_distance = sqrt(rcp_sample_square_distance);
                    incoming *= rcp_sample_distance;
                    cos_on_light *= rcp_sample_distance;

                    ShadingPoint light_shading_point;
                    light_sample.make_shading_point(
                        light_shading_point,
                        -incoming,
                        m_shading_context.get_intersector());

                    // Execute the OSL emission shader if needed.
#ifdef APPLESEED_WITH_OSL
                    if (const ShaderGroup* sg = material->get_osl_surface())
                        m_shading_context.execute_osl_emission(*sg, light_shading_point);
#endif

                    // Evaluate the EDF inputs.
                    InputEvaluator edf_input_evaluator(m_shading_context.get_texture_cache());
                    edf->evaluate_inputs(edf_input_evaluator, light_shading_point);

                    // Evaluate the EDF.
                    edf->evaluate(
                        edf_input_evaluator.data(),
                        light_sample.m_geometric_normal,
                        Basis3d(light_sample.m_shading_normal),
                        -incoming,
                        irradiance);

                    attenuation = rcp_sample_square_distance * cos_on_light;
                }
                else
                {
                    const Light* light = light_sample.m_light;

                    // todo: check light flags.

                    // Evaluate the light.
                    InputEvaluator light_input_evaluator(m_shading_context.get_texture_cache());
                    Vector3d emission_position, emission_direction;
                    Spectrum light_value;
                    light->evaluate(
                        light_input_evaluator,
                        light_sample.m_light_transform,
                        shading_point.get_point(),
                        emission_position,
                        emission_direction,
                        light_value);

                    // Compute the transmission factor between the light sample and the shading point.
                    transmission =
                        m_shading_context.get_tracer().trace_between(
                            shading_point,
                            light_sample.m_point,
                            VisibilityFlags::ShadowRay);

                    // Discard occluded samples.
                    if (transmission == 0.0)
                        return false;

                    // Compute the incoming direction in world space.
                    incoming = -emission_direction;

                    attenuation = light->compute_distance_attenuation(shading_point.get_point(), emission_position);
                }

                // Compute cosine factor at shading point.
                // abs() because we might be on the backside of the surface.
                cos_in = abs(dot(incoming, shading_point.get_shading_normal()));

                // Compute irradiance at shading point.
                irradiance *= static_cast<float>(cos_in / light_sample.m_probability);
                irradiance *= static_cast<float>(attenuation);

                return true;
            }
        };

        //
        // Base path visitor.
        //

        struct PathVisitorBase
        {
            const Parameters&           m_params;
            const LightSampler&         m_light_sampler;
            SamplingContext&            m_sampling_context;
            const ShadingContext&       m_shading_context;
            TextureCache&               m_texture_cache;
            const EnvironmentEDF*       m_env_edf;
            Spectrum&                   m_path_radiance;
            SpectrumStack&              m_path_aovs;
            bool                        m_omit_emitted_light;

            PathVisitorBase(
                const Parameters&       params,
                const LightSampler&     light_sampler,
                SamplingContext&        sampling_context,
                const ShadingContext&   shading_context,
                const Scene&            scene,
                Spectrum&               path_radiance,
                SpectrumStack&          path_aovs)
              : m_params(params)
              , m_light_sampler(light_sampler)
              , m_sampling_context(sampling_context)
              , m_shading_context(shading_context)
              , m_texture_cache(shading_context.get_texture_cache())
              , m_env_edf(scene.get_environment()->get_environment_edf())
              , m_path_radiance(path_radiance)
              , m_path_aovs(path_aovs)
              , m_omit_emitted_light(false)
            {
            }

            bool accept_scattering(
                const ScatteringMode::Mode  prev_mode,
                const ScatteringMode::Mode  next_mode)
            {
                assert(next_mode != ScatteringMode::Absorption);

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
                Spectrum&               path_radiance,
                SpectrumStack&          path_aovs)
              : PathVisitorBase(
                    params,
                    light_sampler,
                    sampling_context,
                    shading_context,
                    scene,
                    path_radiance,
                    path_aovs)
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
                    Spectrum emitted_radiance;
                    vertex.compute_emitted_radiance(
                        m_shading_context,
                        m_texture_cache,
                        emitted_radiance);

                    // Update the path radiance.
                    emitted_radiance *= vertex.m_throughput;
                    m_path_radiance += emitted_radiance;
                    m_path_aovs.add(vertex.m_edf->get_render_layer_index(), emitted_radiance);
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
                InputEvaluator input_evaluator(m_texture_cache);
                Spectrum env_radiance;
                double env_prob;
                m_env_edf->evaluate(
                    m_shading_context,
                    input_evaluator,
                    -vertex.m_outgoing.get_value(),
                    env_radiance,
                    env_prob);

                // Update the path radiance.
                env_radiance *= vertex.m_throughput;
                m_path_radiance += env_radiance;
                m_path_aovs.add(m_env_edf->get_render_layer_index(), env_radiance);
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
                Spectrum&               path_radiance,
                SpectrumStack&          path_aovs)
              : PathVisitorBase(
                    params,
                    light_sampler,
                    sampling_context,
                    shading_context,
                    scene,
                    path_radiance,
                    path_aovs)
              , m_is_indirect_lighting(false)
            {
            }

            void visit_vertex(const PathVertex& vertex)
            {
                // Any light contribution after a diffuse or glossy bounce is considered indirect.
                if (ScatteringMode::has_diffuse_or_glossy(vertex.m_prev_mode))
                    m_is_indirect_lighting = true;

                Spectrum vertex_radiance(0.0f);
                SpectrumStack vertex_aovs(m_path_aovs.size(), 0.0f);

                if (vertex.m_bsdf)
                {
                    const int scattering_modes =
                        !m_params.m_enable_caustics && vertex.m_prev_mode == ScatteringMode::Diffuse
                            ? ScatteringMode::Diffuse
                            : ScatteringMode::All;

                    // Direct lighting.
                    if (m_params.m_enable_dl || vertex.m_path_length > 1)
                    {
                        add_direct_lighting_contribution(
                            vertex,
                            scattering_modes,
                            vertex_radiance,
                            vertex_aovs);
                    }

                    // Image-based lighting.
                    if (m_params.m_enable_ibl && m_env_edf)
                    {
                        add_image_based_lighting_contribution(
                            vertex,
                            scattering_modes,
                            vertex_radiance,
                            vertex_aovs);
                    }
                }

                // Subsurface scattering.
                if (vertex.m_bssrdf && vertex.m_cos_on > 0.0)
                {
                    add_sss_contribution(
                        vertex,
                        vertex_radiance,
                        vertex_aovs);
                }

                // Emitted light.
                if ((!m_omit_emitted_light || m_params.m_enable_caustics) &&
                    vertex.m_edf &&
                    vertex.m_cos_on > 0.0 &&
                    (vertex.m_path_length > 2 || m_params.m_enable_dl) &&
                    (vertex.m_path_length < 2 || (vertex.m_edf->get_flags() & EDF::CastIndirectLight)))
                {
                    add_emitted_light_contribution(
                        vertex,
                        vertex_radiance,
                        vertex_aovs);
                }

                // Optionally clamp secondary rays contribution.
                if (m_params.m_has_max_ray_intensity && vertex.m_path_length > 1)
                {
                    clamp_contribution(vertex_radiance);
                    clamp_contribution(vertex_aovs);
                }

                // Update the path radiance.
                vertex_radiance *= vertex.m_throughput;
                m_path_radiance += vertex_radiance;
                vertex_aovs *= vertex.m_throughput;
                m_path_aovs += vertex_aovs;
            }

            void add_direct_lighting_contribution(
                const PathVertex&       vertex,
                const int               scattering_modes,
                Spectrum&               vertex_radiance,
                SpectrumStack&          vertex_aovs)
            {
                Spectrum dl_radiance;
                SpectrumStack dl_aovs(vertex_aovs.size());

                const bool last_vertex = vertex.m_path_length == m_params.m_max_path_length;

                const size_t light_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_dl_light_sample_count);

                const size_t bsdf_sample_count = last_vertex ? light_sample_count : 1;

                DirectLightingIntegrator integrator(
                    m_shading_context,
                    m_light_sampler,
                    vertex,
                    scattering_modes,
                    scattering_modes,
                    bsdf_sample_count,
                    light_sample_count,
                    m_is_indirect_lighting);

                if (last_vertex)
                {
                    // This path won't be extended: sample both the lights and the BSDF.
                    integrator.sample_bsdf_and_lights_low_variance(
                        vertex.m_sampling_context,
                        dl_radiance,
                        dl_aovs);
                }
                else
                {
                    // This path will be extended via BSDF sampling: sample the lights only.
                    integrator.sample_lights_low_variance(
                        vertex.m_sampling_context,
                        DirectLightingIntegrator::mis_power2,
                        dl_radiance,
                        dl_aovs);
                }

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_dl_light_sample_count > 0.0f)
                {
                    dl_radiance *= m_params.m_rcp_dl_light_sample_count;
                    dl_aovs *= m_params.m_rcp_dl_light_sample_count;
                }

                // Add the direct lighting contributions.
                vertex_radiance += dl_radiance;
                vertex_aovs += dl_aovs;
            }

            void add_image_based_lighting_contribution(
                const PathVertex&       vertex,
                const int               scattering_modes,
                Spectrum&               vertex_radiance,
                SpectrumStack&          vertex_aovs)
            {
                Spectrum ibl_radiance;

                const bool last_vertex = vertex.m_path_length == m_params.m_max_path_length;

                const size_t env_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_ibl_env_sample_count);

                const size_t bsdf_sample_count = last_vertex ? env_sample_count : 1;

                if (last_vertex)
                {
                    // This path won't be extended: sample both the environment and the BSDF.
                    compute_ibl(
                        m_shading_context,
                        *m_env_edf,
                        vertex,
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
                        m_shading_context,
                        *m_env_edf,
                        vertex,
                        scattering_modes,
                        bsdf_sample_count,
                        env_sample_count,
                        ibl_radiance);
                }

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_ibl_env_sample_count > 0.0f)
                    ibl_radiance *= m_params.m_rcp_ibl_env_sample_count;

                // Add the image-based lighting contributions.
                vertex_radiance += ibl_radiance;
                vertex_aovs.add(m_env_edf->get_render_layer_index(), ibl_radiance);
            }

            void add_sss_contribution(
                const PathVertex&       vertex,
                Spectrum&               vertex_radiance,
                SpectrumStack&          vertex_aovs)
            {
                const size_t env_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_ibl_env_sample_count);

                const SubsurfaceSampleVisitor visitor(
                    m_light_sampler,
                    m_sampling_context,
                    m_shading_context,
                    m_params.m_enable_ibl ? m_env_edf : 0,
                    env_sample_count,
                    vertex,
                    vertex_radiance,
                    vertex_aovs);
                const SubsurfaceSampler sampler(m_shading_context);
                sampler.sample(
                    m_sampling_context,
                    *vertex.m_shading_point,
                    *vertex.m_bssrdf,
                    vertex.m_bssrdf_data,
                    visitor);
            }

            void add_emitted_light_contribution(
                const PathVertex&       vertex,
                Spectrum&               vertex_radiance,
                SpectrumStack&          vertex_aovs)
            {
                // Compute the emitted radiance.
                Spectrum emitted_radiance;
                vertex.compute_emitted_radiance(
                    m_shading_context,
                    m_texture_cache,
                    emitted_radiance);

                // Multiple importance sampling.
                if (vertex.m_prev_mode != ScatteringMode::Specular)
                {
                    const double light_sample_count = max(m_params.m_dl_light_sample_count, 1.0);
                    const double mis_weight =
                        mis_power2(
                            1.0 * vertex.get_bsdf_point_prob(),
                            light_sample_count * vertex.get_light_point_prob(m_light_sampler));
                    emitted_radiance *= static_cast<float>(mis_weight);
                }

                // Add the emitted light contributions.
                vertex_radiance += emitted_radiance;
                vertex_aovs.add(vertex.m_edf->get_render_layer_index(), emitted_radiance);
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
                InputEvaluator input_evaluator(m_texture_cache);
                Spectrum env_radiance;
                double env_prob;
                m_env_edf->evaluate(
                    m_shading_context,
                    input_evaluator,
                    -vertex.m_outgoing.get_value(),
                    env_radiance,
                    env_prob);

                // Multiple importance sampling.
                if (vertex.m_prev_mode != ScatteringMode::Specular)
                {
                    assert(vertex.m_prev_prob > 0.0);
                    const double env_sample_count = max(m_params.m_ibl_env_sample_count, 1.0);
                    const double mis_weight =
                        mis_power2(
                            1.0 * vertex.m_prev_prob,
                            env_sample_count * env_prob);
                    env_radiance *= static_cast<float>(mis_weight);
                }

                // Optionally clamp secondary rays contribution.
                if (m_params.m_has_max_ray_intensity && vertex.m_path_length > 1)
                    clamp_contribution(env_radiance);

                // Update the path radiance.
                env_radiance *= vertex.m_throughput;
                m_path_radiance += env_radiance;
                m_path_aovs.add(m_env_edf->get_render_layer_index(), env_radiance);
            }

            void clamp_contribution(Spectrum& radiance)
            {
                const float avg = average_value(radiance);

                if (avg > m_params.m_max_ray_intensity)
                    radiance *= m_params.m_max_ray_intensity / avg;
            }

            void clamp_contribution(SpectrumStack& aovs)
            {
                const size_t size = aovs.size();

                for (size_t i = 0; i < size; ++i)
                    clamp_contribution(aovs[i]);
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
            .insert("help", "Enable direct lighting"));

    metadata.dictionaries().insert(
        "enable_caustics",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "false")
            .insert("help", "Enable caustics"));

    metadata.dictionaries().insert(
        "max_path_length",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "1")
            .insert("help", "Maximum number of path bounces"));

    metadata.dictionaries().insert(
        "rr_min_path_length",
        Dictionary()
            .insert("type", "int")
            .insert("default", "3")
            .insert("min", "1")
            .insert("help", "Consider pruning low contribution paths starting with this bounce"));

    metadata.dictionaries().insert(
        "max_ray_intensity",
        Dictionary()
            .insert("type", "float")
            .insert("default", "1.0")
            .insert("unlimited", "true")
            .insert("min", "0.0")
            .insert("help", "Clamp intensity of rays (after the first bounce) to this value to reduce fireflies"));

    return metadata;
}

}   // namespace renderer
