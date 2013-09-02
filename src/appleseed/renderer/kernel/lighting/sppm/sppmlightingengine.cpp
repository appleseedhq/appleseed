
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
#include "sppmlightingengine.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/lighting/sppm/sppmpasscallback.h"
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
#include "renderer/utility/stochasticcast.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/knn.h"
#include "foundation/math/mis.h"
#include "foundation/math/population.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>

// Forward declarations.
namespace renderer  { class EnvironmentEDF; }
namespace renderer  { class PixelContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Stochastic Progressive Photon Mapping (SPPM) lighting engine.
    //
    // References:
    //
    //   Progressive Photon Mapping: A Probabilistic Approach
    //   Claude Knaus, Matthias Zwicker
    //   http://www.cgg.unibe.ch/publications/2011/progressive-photon-mapping-a-probabilistic-approach
    //
    //   Progressive Photon Mapping
    //   Toshiya Hachisuka, Shinji Ogaki, Henrik Wann Jensen
    //   http://cs.au.dk/~toshiya/ppm.pdf
    //
    //   Stochastic Progressive Photon Mapping
    //   Toshiya Hachisuka, Henrik Wann Jensen
    //   http://cs.au.dk/~toshiya/sppm.pdf
    //

    class SPPMLightingEngine
      : public ILightingEngine
    {
      public:
        struct Parameters
        {
            const bool      m_enable_ibl;                   // image-based lighting enabled?

            const size_t    m_max_path_length;              // maximum path length, ~0 for unlimited
            const size_t    m_rr_min_path_length;           // minimum path length before Russian Roulette kicks in, ~0 for unlimited

            const double    m_ibl_env_sample_count;         // number of environment samples used to estimate IBL
            float           m_rcp_ibl_env_sample_count;

            const size_t    m_max_photons_per_estimate;     // maximum number of photons per density estimation

            const bool      m_view_photons;                 // debug mode to visualize the photons
            const float     m_view_photons_radius;          // lookup radius when visualizing photons

            explicit Parameters(const ParamArray& params)
              : m_enable_ibl(params.get_optional<bool>("enable_ibl", true))
              , m_max_path_length(nz(params.get_optional<size_t>("max_path_length", 0)))
              , m_rr_min_path_length(nz(params.get_optional<size_t>("rr_min_path_length", 3)))
              , m_ibl_env_sample_count(params.get_optional<double>("ibl_env_samples", 1.0))
              , m_max_photons_per_estimate(params.get_optional<size_t>("max_photons_per_estimate", 100))
              , m_view_photons(params.get_optional<bool>("view_photons", false))
              , m_view_photons_radius(params.get_optional<float>("view_photons_radius", 1.0e-3f))
            {
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
                    "sppm settings:\n"
                    "  ibl              %s\n"
                    "  max path length  %s\n"
                    "  rr min path len. %s\n"
                    "  ibl env samples  %s\n"
                    "  max photons/est  %s",
                    m_enable_ibl ? "on" : "off",
                    m_max_path_length == ~0 ? "infinite" : pretty_uint(m_max_path_length).c_str(),
                    m_rr_min_path_length == ~0 ? "infinite" : pretty_uint(m_rr_min_path_length).c_str(),
                    pretty_scalar(m_ibl_env_sample_count).c_str(),
                    pretty_uint(m_max_photons_per_estimate).c_str());
            }
        };

        SPPMLightingEngine(
            const SPPMPassCallback& pass_callback,
            const LightSampler&     light_sampler,
            const ParamArray&       params)
          : m_params(params)
          , m_pass_callback(pass_callback)
          , m_light_sampler(light_sampler)
          , m_path_count(0)
          , m_answer(m_params.m_max_photons_per_estimate)
        {
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual void compute_lighting(
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Spectrum&               radiance,               // output radiance, in W.sr^-1.m^-2
            SpectrumStack&          aovs) OVERRIDE
        {
            if (m_params.m_view_photons)
            {
                view_photons(shading_point, radiance);
                return;
            }

            PathVisitor path_visitor(
                m_params,
                m_pass_callback,
                m_light_sampler,
                sampling_context,
                shading_context,
                shading_point.get_scene(),
                m_answer,
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

            return StatisticsVector::make("sppm statistics", stats);
        }

      private:
        const Parameters                m_params;
        const SPPMPassCallback&         m_pass_callback;
        const LightSampler&             m_light_sampler;
        uint64                          m_path_count;
        Population<uint64>              m_path_length;
        knn::Answer<float>              m_answer;

        struct PathVisitor
        {
            const Parameters&           m_params;
            const SPPMPassCallback&     m_pass_callback;
            const LightSampler&         m_light_sampler;
            SamplingContext&            m_sampling_context;
            const ShadingContext&       m_shading_context;
            TextureCache&               m_texture_cache;
            const EnvironmentEDF*       m_env_edf;
            knn::Answer<float>&         m_answer;
            Spectrum&                   m_path_radiance;
            SpectrumStack&              m_path_aovs;

            PathVisitor(
                const Parameters&       params,
                const SPPMPassCallback& pass_callback,
                const LightSampler&     light_sampler,
                SamplingContext&        sampling_context,
                const ShadingContext&   shading_context,
                const Scene&            scene,
                knn::Answer<float>&     answer,
                Spectrum&               path_radiance,
                SpectrumStack&          path_aovs)
              : m_params(params)
              , m_pass_callback(pass_callback)
              , m_light_sampler(light_sampler)
              , m_sampling_context(sampling_context)
              , m_shading_context(shading_context)
              , m_texture_cache(shading_context.get_texture_cache())
              , m_env_edf(scene.get_environment()->get_environment_edf())
              , m_answer(answer)
              , m_path_radiance(path_radiance)
              , m_path_aovs(path_aovs)
            {
            }

            bool accept_scattering_mode(
                const BSDF::Mode        prev_bsdf_mode,
                const BSDF::Mode        bsdf_mode) const
            {
                assert(bsdf_mode != BSDF::Absorption);

                // No diffuse bounces.
                if (BSDF::has_diffuse(bsdf_mode))
                    return false;

                return true;
            }

            bool visit_vertex(const PathVertex& vertex)
            {
                Spectrum vertex_radiance(0.0f);
                SpectrumStack vertex_aovs(m_path_aovs.size(), 0.0f);

                if (vertex.m_bsdf && !vertex.m_bsdf->is_purely_specular())
                {
                    // Lighting from photon map.
                    add_photon_map_lighting_contribution(
                        vertex,
                        vertex_radiance,
                        vertex_aovs);
                }

                // Emitted light.
                if (vertex.m_edf && vertex.m_cos_on > 0.0)
                {
                    add_emitted_light_contribution(
                        vertex,
                        vertex_radiance,
                        vertex_aovs);
                }

                // Update the path radiance.
                vertex_radiance *= vertex.m_throughput;
                m_path_radiance += vertex_radiance;
                vertex_aovs *= vertex.m_throughput;
                m_path_aovs += vertex_aovs;

                // Proceed with this path.
                return true;
            }

            void add_photon_map_lighting_contribution(
                const PathVertex&       vertex,
                Spectrum&               vertex_radiance,
                SpectrumStack&          vertex_aovs)
            {
                // No indirect lighting if the photon map is empty.
                if (m_pass_callback.get_photon_map().empty())
                    return;

                // Find the nearby photons around the path vertex.
                const knn::Query3f query(m_pass_callback.get_photon_map(), m_answer);
                query.run(
                    Vector3f(vertex.get_point()),
                    square(m_pass_callback.get_lookup_radius()));

                // Cannot do proper density estimation if too few photons are found.
                const size_t MinPhotonCount = 8;
                const size_t photon_count = m_answer.size();
                if (photon_count < MinPhotonCount)
                    return;

                size_t included_photon_count = 0;
                float max_square_dist = 0.0f;
                Spectrum indirect_radiance(0.0f);

                // Loop over the nearby photons.
                for (size_t i = 0; i < photon_count; ++i)
                {
                    // Retrieve the i'th photon.
                    const knn::Answer<float>::Entry& photon = m_answer.get(i);
                    const SPPMPhotonData& data = m_pass_callback.get_photon_data(photon.m_index);

                    // Reject photons that are facing away.
                    if (dot(vertex.get_geometric_normal(), Vector3d(data.m_geometric_normal)) < 0.0)
                        continue;

                    // Reject photons on the wrong side of the surface.
                    if (dot(vertex.m_outgoing, Vector3d(data.m_geometric_normal)) < 0.0)
                        continue;

                    // Keep track of the farthest photon.
                    if (max_square_dist < photon.m_square_dist)
                        max_square_dist = photon.m_square_dist;

                    // Evaluate the BSDF for this photon.
                    Spectrum bsdf_value;
                    const double bsdf_prob =
                        vertex.m_bsdf->evaluate(
                            vertex.m_bsdf_data,
                            true,                                   // adjoint
                            true,                                   // multiply by |cos(incoming, normal)|
                            vertex.get_geometric_normal(),
                            vertex.get_shading_basis(),
                            vertex.m_outgoing,                      // toward the camera
                            normalize(Vector3d(data.m_incoming)),   // toward the light
                            BSDF::Diffuse,
                            bsdf_value);
                    if (bsdf_prob == 0.0)
                        continue;

                    // Accumulate reflected flux.
                    ++included_photon_count;
                    bsdf_value *= data.m_flux;
                    indirect_radiance += bsdf_value;
                }

                // Unreliable density estimation if too few photons were actually included.
                if (included_photon_count < MinPhotonCount)
                    return;

                // Density estimation.
                indirect_radiance /=
                    static_cast<float>(
                        Pi * max_square_dist * m_pass_callback.get_emitted_photon_count());

                // Add the indirect lighting contribution.
                vertex_radiance += indirect_radiance;
            }

            void add_emitted_light_contribution(
                const PathVertex&       vertex,
                Spectrum&               vertex_radiance,
                SpectrumStack&          vertex_aovs)
            {
                // Compute the emitted radiance.
                Spectrum emitted_radiance;
                vertex.compute_emitted_radiance(m_texture_cache, emitted_radiance);

                // Add the emitted light contribution.
                vertex_radiance += emitted_radiance;
                vertex_aovs.add(vertex.m_edf->get_render_layer_index(), emitted_radiance);
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

                // Update the path radiance.
                env_radiance *= throughput;
                m_path_radiance += env_radiance;
                m_path_aovs.add(m_env_edf->get_render_layer_index(), env_radiance);
            }
        };

        void view_photons(
            const ShadingPoint&     shading_point,
            Spectrum&               radiance)
        {
            const knn::Query3f query(
                m_pass_callback.get_photon_map(),
                m_answer);

            query.run(
                Vector3f(shading_point.get_point()),
                square(m_params.m_view_photons_radius));

            radiance.set(0.0f);

            const size_t photon_count = m_answer.size();

            for (size_t i = 0; i < photon_count; ++i)
            {
                const knn::Answer<float>::Entry& photon = m_answer.get(i);
                radiance += m_pass_callback.get_photon_data(photon.m_index).m_flux;
            }

            if (photon_count > 1)
                radiance /= static_cast<float>(photon_count);
        }
    };
}


//
// SPPMLightingEngineFactory class implementation.
//

SPPMLightingEngineFactory::SPPMLightingEngineFactory(
    const SPPMPassCallback&     pass_callback,
    const LightSampler&         light_sampler,
    const ParamArray&           params)
  : m_pass_callback(pass_callback)
  , m_light_sampler(light_sampler)
  , m_params(params)
{
    SPPMLightingEngine::Parameters(params).print();
}

void SPPMLightingEngineFactory::release()
{
    delete this;
}

ILightingEngine* SPPMLightingEngineFactory::create()
{
    return
        new SPPMLightingEngine(
            m_pass_callback,
            m_light_sampler,
            m_params);
}

}   // namespace renderer
