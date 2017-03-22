
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
#include "sppmlightingengine.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/directlightingintegrator.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/lighting/sppm/sppmpasscallback.h"
#include "renderer/kernel/lighting/sppm/sppmphoton.h"
#include "renderer/kernel/lighting/sppm/sppmphotonmap.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/stochasticcast.h"

// appleseed.foundation headers.
#include "foundation/math/knn.h"
#include "foundation/math/population.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace renderer  { class PixelContext; }
namespace renderer  { class TextureCache; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // 2D density estimation kernels.
    //
    // References:
    //
    //   http://en.wikipedia.org/wiki/Kernel_(statistics)
    //
    //   http://graphics.cs.kuleuven.be/publications/phdSuykens/suykens_8.pdf p. 154
    //

    template <typename T>
    inline T box2d(const T r2)
    {
        return foundation::RcpPi<T>();
    }

    template <typename T>
    inline T epanechnikov2d(const T r2)
    {
        return foundation::RcpHalfPi<T>() * (T(1.0) - r2);
    }


    //
    // Stochastic Progressive Photon Mapping (SPPM) lighting engine.
    //
    // References:
    //
    //   Progressive Photon Mapping: A Probabilistic Approach
    //   Claude Knaus, Matthias Zwicker
    //   http://www.cgg.unibe.ch/publications/2011/progressive-photon-mapping-a-probabilistic-approach
    //
    //   Stochastic Progressive Photon Mapping
    //   Toshiya Hachisuka, Henrik Wann Jensen
    //   http://cs.au.dk/~toshiya/sppm.pdf
    //
    //   Progressive Photon Mapping
    //   Toshiya Hachisuka, Shinji Ogaki, Henrik Wann Jensen
    //   http://cs.au.dk/~toshiya/ppm.pdf
    //
    //   Extended Photon Map Implementation
    //   Matt Pharr
    //   http://www.pbrt.org/plugins/exphotonmap.pdf
    //

    class SPPMLightingEngine
      : public ILightingEngine
    {
      public:
        SPPMLightingEngine(
            const SPPMPassCallback& pass_callback,
            const LightSampler&     light_sampler,
            const SPPMParameters&   params)
          : m_params(params)
          , m_pass_callback(pass_callback)
          , m_light_sampler(light_sampler)
          , m_path_count(0)
          , m_answer(m_params.m_max_photons_per_estimate)
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
                radiance);

            PathTracer<PathVisitor, false> path_tracer(     // false = not adjoint
                path_visitor,
                m_params.m_path_tracing_rr_min_path_length,
                m_params.m_path_tracing_max_path_length,
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

            return StatisticsVector::make("sppm statistics", stats);
        }

      private:
        const SPPMParameters            m_params;
        const SPPMPassCallback&         m_pass_callback;
        const LightSampler&             m_light_sampler;
        uint64                          m_path_count;
        Population<uint64>              m_path_length;
        knn::Answer<float>              m_answer;

        struct PathVisitor
        {
            const SPPMParameters&       m_params;
            const SPPMPassCallback&     m_pass_callback;
            const LightSampler&         m_light_sampler;
            SamplingContext&            m_sampling_context;
            const ShadingContext&       m_shading_context;
            const EnvironmentEDF*       m_env_edf;
            knn::Answer<float>&         m_answer;
            Spectrum&                   m_path_radiance;

            PathVisitor(
                const SPPMParameters&   params,
                const SPPMPassCallback& pass_callback,
                const LightSampler&     light_sampler,
                SamplingContext&        sampling_context,
                const ShadingContext&   shading_context,
                const Scene&            scene,
                knn::Answer<float>&     answer,
                Spectrum&               path_radiance)
              : m_params(params)
              , m_pass_callback(pass_callback)
              , m_light_sampler(light_sampler)
              , m_sampling_context(sampling_context)
              , m_shading_context(shading_context)
              , m_env_edf(scene.get_environment()->get_environment_edf())
              , m_answer(answer)
              , m_path_radiance(path_radiance)
            {
            }

            bool accept_scattering(
                const ScatteringMode::Mode  prev_mode,
                const ScatteringMode::Mode  next_mode) const
            {
                assert(next_mode != ScatteringMode::Absorption);

                // No diffuse bounces.
                if (ScatteringMode::has_diffuse(next_mode))
                    return false;

                return true;
            }

            void visit_vertex(const PathVertex& vertex)
            {
                Spectrum vertex_radiance(0.0f, Spectrum::Illuminance);

                if (vertex.m_bsdf)
                {
                    // Direct lighting.
                    if (m_params.m_dl_mode == SPPMParameters::RayTraced)
                        add_direct_lighting_contribution(vertex, vertex_radiance);

                    if (!vertex.m_bsdf->is_purely_specular())
                    {
                        // Lighting from photon map.
                        add_photon_map_lighting_contribution(vertex, vertex_radiance);
                    }
                }

                // Emitted light.
                if (vertex.m_edf && vertex.m_cos_on > 0.0)
                    add_emitted_light_contribution(vertex, vertex_radiance);

                // Update the path radiance.
                vertex_radiance *= vertex.m_throughput;
                m_path_radiance += vertex_radiance;
            }

            void add_direct_lighting_contribution(
                const PathVertex&       vertex,
                Spectrum&               vertex_radiance)
            {
                Spectrum dl_radiance(Spectrum::Illuminance);

                const size_t light_sample_count =
                    stochastic_cast<size_t>(
                        m_sampling_context,
                        m_params.m_dl_light_sample_count);

                const size_t bsdf_sample_count = light_sample_count;

                // Unlike in the path tracer, we need to sample the diffuse components
                // of the BSDF because we won't extend the path after a diffuse bounce.
                const DirectLightingIntegrator integrator(
                    m_shading_context,
                    m_light_sampler,
                    *vertex.m_shading_point,
                    *vertex.m_bsdf,
                    vertex.m_bsdf_data,
                    ScatteringMode::Diffuse,
                    ScatteringMode::All,
                    bsdf_sample_count,
                    light_sample_count,
                    false);             // not computing indirect lighting

                // Always sample both the lights and the BSDF.
                integrator.compute_outgoing_radiance_combined_sampling_low_variance(
                    vertex.m_sampling_context,
                    vertex.m_outgoing,
                    dl_radiance);

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_dl_light_sample_count > 0.0f)
                    dl_radiance *= m_params.m_rcp_dl_light_sample_count;

                // Add the direct lighting contributions.
                vertex_radiance += dl_radiance;
            }

            void add_photon_map_lighting_contribution(
                const PathVertex&       vertex,
                Spectrum&               vertex_radiance)
            {
                const SPPMPhotonMap& photon_map = m_pass_callback.get_photon_map();

                // No indirect lighting if the photon map is empty.
                if (photon_map.empty())
                    return;

                const Vector3f point(vertex.get_point());
                const float radius = m_pass_callback.get_lookup_radius();

                // Find the nearby photons around the path vertex.
                const knn::Query3f query(photon_map, m_answer);
                query.run(point, radius * radius);
                const size_t photon_count = m_answer.size();

                // Compute the square radius of the lookup disk.
                float max_square_dist;
                if (photon_count < m_params.m_max_photons_per_estimate)
                    max_square_dist = radius * radius;
                else
                {
                    max_square_dist = 0.0f;
                    for (size_t i = 0; i < photon_count; ++i)
                    {
                        const float square_dist = m_answer.get(i).m_square_dist;
                        if (max_square_dist < square_dist)
                            max_square_dist = square_dist;
                    }
                }
                const float rcp_max_square_dist = 1.0f / max_square_dist;

                // Accumulate photons contributions.
                Spectrum indirect_radiance(Spectrum::Illuminance);
                if (m_params.m_photon_type == SPPMParameters::Monochromatic)
                {
                    indirect_radiance.resize(Spectrum::Samples);
                    indirect_radiance.set(0.0f);
                    accumulate_mono_photons(
                        vertex,
                        photon_count,
                        rcp_max_square_dist,
                        indirect_radiance);
                }
                else
                {
                    indirect_radiance.set(0.0f);
                    accumulate_poly_photons(
                        vertex,
                        photon_count,
                        rcp_max_square_dist,
                        indirect_radiance);
                }

                // Estimate photon density.
                indirect_radiance *= rcp_max_square_dist;

                // Add the indirect lighting contribution.
                vertex_radiance += indirect_radiance;
            }

            void accumulate_mono_photons(
                const PathVertex&       vertex,
                const size_t            photon_count,
                const float             rcp_max_square_dist,
                Spectrum&               radiance)
            {
                const SPPMPhotonMap& photon_map = m_pass_callback.get_photon_map();
                const Vector3f normal(vertex.get_geometric_normal());

                for (size_t i = 0; i < photon_count; ++i)
                {
                    // Retrieve the i'th photon.
                    const knn::Answer<float>::Entry& entry = m_answer.get(i);
                    const SPPMMonoPhoton& photon =
                        m_pass_callback.get_mono_photon(
                            photon_map.remap(entry.m_index));

                    // Reject photons from the opposite hemisphere as they won't contribute.
                    if (dot(normal, photon.m_incoming) <= 0.0f)
                        continue;

                    // Reject photons on a surface with too different an orientation.
                    const float NormalThreshold = 1.0e-3f;
                    if (dot(normal, photon.m_geometric_normal) < NormalThreshold)
                        continue;

#if 0
                    // Reject photons on the wrong side of the surface.
                    if (dot(vertex.m_outgoing, Vector3d(photon.m_geometric_normal)) <= 0.0)
                        continue;
#endif

#if 0
                    // Reject photons too far away along the surface's normal.
                    const Vector3f& photon_position = photon_map.get_point(photon.m_index);
                    const Vector3f point_to_photon = photon_position - point;
                    const float vdist = abs(dot(normal, point_to_photon));
                    if (vdist > 0.1f * radius)
                        continue;
#endif

                    // Evaluate the BSDF for this photon.
                    Spectrum bsdf_value;
                    const float bsdf_prob =
                        vertex.m_bsdf->evaluate(
                            vertex.m_bsdf_data,
                            false,                                      // not adjoint
                            true,                                       // multiply by |cos(incoming, normal)|
                            Vector3f(vertex.get_geometric_normal()),
                            Basis3f(vertex.get_shading_basis()),
                            Vector3f(vertex.m_outgoing.get_value()),    // toward the camera
                            normalize(photon.m_incoming),               // toward the light
                            ScatteringMode::Diffuse,
                            bsdf_value);
                    if (bsdf_prob == 0.0f)
                        continue;

                    // Make sure the BSDF value is spectral.
                    Spectrum spectral_bsdf_value;
                    Spectrum::upgrade(bsdf_value, spectral_bsdf_value);

                    // The photons store flux but we are computing reflected radiance.
                    // The first step of the flux -> radiance conversion is done here.
                    // The conversion will be completed when doing density estimation.
                    float bsdf_mono_value = spectral_bsdf_value[photon.m_flux.m_wavelength];
                    bsdf_mono_value /= abs(dot(photon.m_incoming, photon.m_geometric_normal));
                    bsdf_mono_value *= photon.m_flux.m_amplitude;

                    // Apply kernel weight.
                    bsdf_mono_value *= epanechnikov2d(entry.m_square_dist * rcp_max_square_dist);

                    // Accumulate reflected flux.
                    radiance[photon.m_flux.m_wavelength] += bsdf_mono_value;
                }
            }

            void accumulate_poly_photons(
                const PathVertex&       vertex,
                const size_t            photon_count,
                const float             rcp_max_square_dist,
                Spectrum&               radiance)
            {
                const SPPMPhotonMap& photon_map = m_pass_callback.get_photon_map();
                const Vector3f normal(vertex.get_geometric_normal());

                for (size_t i = 0; i < photon_count; ++i)
                {
                    // Retrieve the i'th photon.
                    const knn::Answer<float>::Entry& entry = m_answer.get(i);
                    const SPPMPolyPhoton& photon =
                        m_pass_callback.get_poly_photon(
                            photon_map.remap(entry.m_index));

                    // Reject photons from the opposite hemisphere as they won't contribute.
                    if (dot(normal, photon.m_incoming) <= 0.0f)
                        continue;

                    // Reject photons on a surface with too different an orientation.
                    const float NormalThreshold = 1.0e-3f;
                    if (dot(normal, photon.m_geometric_normal) < NormalThreshold)
                        continue;

#if 0
                    // Reject photons on the wrong side of the surface.
                    if (dot(vertex.m_outgoing, Vector3d(photon.m_geometric_normal)) <= 0.0)
                        continue;
#endif

#if 0
                    // Reject photons too far away along the surface's normal.
                    const Vector3f& photon_position = photon_map.get_point(photon.m_index);
                    const Vector3f point_to_photon = photon_position - point;
                    const float vdist = abs(dot(normal, point_to_photon));
                    if (vdist > 0.1f * radius)
                        continue;
#endif

                    // Evaluate the BSDF for this photon.
                    Spectrum bsdf_value;
                    const float bsdf_prob =
                        vertex.m_bsdf->evaluate(
                            vertex.m_bsdf_data,
                            false,                                      // not adjoint
                            true,                                       // multiply by |cos(incoming, normal)|
                            Vector3f(vertex.get_geometric_normal()),
                            Basis3f(vertex.get_shading_basis()),
                            Vector3f(vertex.m_outgoing.get_value()),    // toward the camera
                            normalize(photon.m_incoming),               // toward the light
                            ScatteringMode::Diffuse,
                            bsdf_value);
                    if (bsdf_prob == 0.0f)
                        continue;

                    // The photons store flux but we are computing reflected radiance.
                    // The first step of the flux -> radiance conversion is done here.
                    // The conversion will be completed when doing density estimation.
                    bsdf_value /= abs(dot(photon.m_incoming, photon.m_geometric_normal));
                    bsdf_value *= photon.m_flux;

                    // Apply kernel weight.
                    bsdf_value *= epanechnikov2d(entry.m_square_dist * rcp_max_square_dist);

                    // Accumulate reflected flux.
                    radiance += bsdf_value;
                }
            }

            void add_emitted_light_contribution(
                const PathVertex&       vertex,
                Spectrum&               vertex_radiance)
            {
                // Compute the emitted radiance.
                Spectrum emitted_radiance(Spectrum::Illuminance);
                vertex.compute_emitted_radiance(m_shading_context, emitted_radiance);

                // Add the emitted light contribution.
                vertex_radiance += emitted_radiance;
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

                // Update the path radiance.
                env_radiance *= vertex.m_throughput;
                m_path_radiance += env_radiance;
            }
        };

        void view_photons(
            const ShadingPoint&     shading_point,
            Spectrum&               radiance)
        {
            const SPPMPhotonMap& photon_map = m_pass_callback.get_photon_map();
            const knn::Query3f query(photon_map, m_answer);

            query.run(
                Vector3f(shading_point.get_point()),
                square(m_params.m_view_photons_radius));

            radiance.set(0.0f);

            const size_t photon_count = m_answer.size();

            if (m_params.m_photon_type == SPPMParameters::Monochromatic)
            {
                for (size_t i = 0; i < photon_count; ++i)
                {
                    const knn::Answer<float>::Entry& photon = m_answer.get(i);
                    const SpectrumLine& flux =
                        m_pass_callback.get_mono_photon(photon_map.remap(photon.m_index)).m_flux;
                    radiance[flux.m_wavelength] += flux.m_amplitude;
                }
            }
            else
            {
                for (size_t i = 0; i < photon_count; ++i)
                {
                    const knn::Answer<float>::Entry& photon = m_answer.get(i);
                    radiance += m_pass_callback.get_poly_photon(photon_map.remap(photon.m_index)).m_flux;
                }
            }

            const float m = max_value(radiance);
            if (m > 0.0f)
                radiance /= m;
        }
    };
}


//
// SPPMLightingEngineFactory class implementation.
//

SPPMLightingEngineFactory::SPPMLightingEngineFactory(
    const SPPMPassCallback&     pass_callback,
    const LightSampler&         light_sampler,
    const SPPMParameters&       params)
  : m_pass_callback(pass_callback)
  , m_light_sampler(light_sampler)
  , m_params(params)
{
    m_params.print();
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

Dictionary SPPMLightingEngineFactory::get_params_metadata()
{
    Dictionary metadata;
    add_common_params_metadata(metadata, false);

    metadata.dictionaries().insert(
        "enable_caustics",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "true")
            .insert("label", "Enable Caustics")
            .insert("help", "Enable caustics"));

    metadata.dictionaries().insert(
        "photon_type",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "mono|poly")
            .insert("default", "poly")
            .insert("label", "Photon Type")
            .insert("help", "Photon Type")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "mono",
                        Dictionary()
                            .insert("label", "Mono")
                            .insert("help", "Monochromatic photons"))
                    .insert(
                        "poly",
                        Dictionary()
                            .insert("label", "Poly")
                            .insert("help", "Polychromatic photons"))));

    metadata.dictionaries().insert(
        "dl_type",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "rt|sppm|off")
            .insert("default", "rt")
            .insert("label", "Direct Lighting")
            .insert("help", "Method used to estimate direct lighting")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "rt",
                        Dictionary()
                            .insert("label", "Ray Tracing")
                            .insert("help", "Use ray tracing to estimate direct lighting"))
                    .insert(
                        "sppm",
                        Dictionary()
                            .insert("label", "Photon Maps")
                            .insert("help", "Use photon maps to estimate direct lighting"))
                    .insert(
                        "off",
                        Dictionary()
                            .insert("label", "Disabled")
                            .insert("help", "Do not estimate direct lighting"))));

    metadata.dictionaries().insert(
        "photon_tracing_max_path_length",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("label", "Max Photon Path Length")
            .insert("help", "Maximum number of photon bounces"));

    metadata.dictionaries().insert(
        "photon_tracing_rr_min_path_length",
        Dictionary()
            .insert("type", "int")
            .insert("default", "6")
            .insert("help", "Consider pruning low contribution photons starting with this bounce"));

    metadata.dictionaries().insert(
        "path_tracing_max_path_length",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("label", "Max Path Length")
            .insert("help", "Maximum number of path bounces"));

    metadata.dictionaries().insert(
        "path_tracing_rr_min_path_length",
        Dictionary()
            .insert("type", "int")
            .insert("default", "6")
            .insert("help", "Consider pruning low contribution paths starting with this bounce"));

    metadata.dictionaries().insert(
        "light_photons_per_pass",
        Dictionary()
            .insert("type", "int")
            .insert("default", "1000000")
            .insert("label", "Light Photons per Pass")
            .insert("help", "Number of photons per render pass"));

    metadata.dictionaries().insert(
        "env_photons_per_pass",
        Dictionary()
            .insert("type", "int")
            .insert("default", "1000000")
            .insert("label", "IBL Photons per Pass")
            .insert("help", "Number of environment photons per render pass"));

    metadata.dictionaries().insert(
        "initial_radius",
        Dictionary()
            .insert("type", "float")
            .insert("default", "0.1")
            .insert("unit", "percent")
            .insert("min", "0.0")
            .insert("max", "100.0")
            .insert("label", "Initial Radius")
            .insert("help", "Initial photon gathering radius in percent of the scene diameter."));

    metadata.dictionaries().insert(
        "max_photons_per_estimate",
        Dictionary()
            .insert("type", "int")
            .insert("default", "100")
            .insert("min", "1")
            .insert("label", "Max Photons per Estimate")
            .insert("help", "Maximum number of photons used to estimate radiance"));

    metadata.dictionaries().insert(
        "alpha",
        Dictionary()
            .insert("type", "float")
            .insert("default", "0.7")
            .insert("label", "Alpha")
            .insert("help", "Evolution rate of photon gathering radius"));

    return metadata;
}

}   // namespace renderer
