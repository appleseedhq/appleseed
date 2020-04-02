
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/lighting/sppm/sppmimporton.h"
#include "renderer/kernel/lighting/sppm/sppmlightingengineworkingset.h"
#include "renderer/kernel/lighting/sppm/sppmpasscallback.h"
#include "renderer/kernel/lighting/sppm/sppmphoton.h"
#include "renderer/kernel/lighting/sppm/sppmphotonmap.h"
#include "renderer/kernel/rendering/pixelcontext.h"
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
#include "foundation/math/knn.h"
#include "foundation/math/population.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/bitmask.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <memory>

// Forward declarations.
namespace renderer  { class TextureCache; }

using namespace foundation;

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
            SPPMPassCallback&               pass_callback,
            const ForwardLightSampler&      forward_light_sampler,
            const BackwardLightSampler&     backward_light_sampler,
            const SPPMParameters&           params)
          : m_pass_callback(pass_callback)
          , m_forward_light_sampler(forward_light_sampler)
          , m_backward_light_sampler(backward_light_sampler)
          , m_params(params)
          , m_path_count(0)
          , m_working_set(pass_callback.acquire_working_set())
          , m_answer(m_params.m_max_photons_per_estimate)
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
            SamplingContext&                sampling_context,
            const PixelContext&             pixel_context,
            const ShadingContext&           shading_context,
            const ShadingPoint&             shading_point,
            ShadingComponents&              radiance,                       // output radiance, in W.sr^-1.m^-2
            AOVComponents&                  aov_components) override
        {
            if (m_params.m_view_photons)
            {
                view_photons(shading_point, radiance.m_beauty);
                return;
            }

            PathVisitor path_visitor(
                m_params,
                m_pass_callback,
                m_forward_light_sampler,
                m_backward_light_sampler,
                sampling_context,
                pixel_context,
                shading_context,
                shading_point.get_scene(),
                m_working_set,
                m_answer,
                radiance);

            VolumeVisitor volume_visitor;

            PathTracer<PathVisitor, VolumeVisitor, false> path_tracer(  // false = not adjoint
                path_visitor,
                volume_visitor,
                m_params.m_path_tracing_rr_min_path_length,
                m_params.m_path_tracing_max_bounces,
                ~std::size_t(0),            // max diffuse bounces
                ~std::size_t(0),            // max glossy bounces
                ~std::size_t(0),            // max specular bounces
                ~std::size_t(0),            // max volume bounces
                false,                      // don't clamp roughness
                shading_context.get_max_iterations());

            const std::size_t path_length =
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

            return StatisticsVector::make("sppm statistics", stats);
        }

      private:
        const SPPMPassCallback&             m_pass_callback;
        const ForwardLightSampler&          m_forward_light_sampler;
        const BackwardLightSampler&         m_backward_light_sampler;
        const SPPMParameters                m_params;
        std::uint64_t                       m_path_count;
        Population<std::uint64_t>           m_path_length;
        SPPMLightingEngineWorkingSet&       m_working_set;
        knn::Answer<float>                  m_answer;

        // todo: move out of this class.
        struct PathVisitor
        {
            const SPPMParameters&               m_params;
            const SPPMPassCallback&             m_pass_callback;
            const ForwardLightSampler&          m_forward_light_sampler;
            const BackwardLightSampler&         m_backward_light_sampler;
            SamplingContext&                    m_sampling_context;
            const PixelContext&                 m_pixel_context;
            const ShadingContext&               m_shading_context;
            const EnvironmentEDF*               m_env_edf;
            SPPMLightingEngineWorkingSet&       m_working_set;
            knn::Answer<float>&                 m_answer;
            ShadingComponents&                  m_path_radiance;

            PathVisitor(
                const SPPMParameters&           params,
                const SPPMPassCallback&         pass_callback,
                const ForwardLightSampler&      forward_light_sampler,
                const BackwardLightSampler&     backward_light_sampler,
                SamplingContext&                sampling_context,
                const PixelContext&             pixel_context,
                const ShadingContext&           shading_context,
                const Scene&                    scene,
                SPPMLightingEngineWorkingSet&   working_set,
                knn::Answer<float>&             answer,
                ShadingComponents&              path_radiance)
              : m_params(params)
              , m_pass_callback(pass_callback)
              , m_forward_light_sampler(forward_light_sampler)
              , m_backward_light_sampler(backward_light_sampler)
              , m_sampling_context(sampling_context)
              , m_pixel_context(pixel_context)
              , m_shading_context(shading_context)
              , m_env_edf(scene.get_environment()->get_environment_edf())
              , m_working_set(working_set)
              , m_answer(answer)
              , m_path_radiance(path_radiance)
            {
            }

            void on_first_diffuse_bounce(
                const PathVertex&               vertex,
                const Spectrum&                 albedo)
            {
            }

            bool accept_scattering(
                const ScatteringMode::Mode      prev_mode,
                const ScatteringMode::Mode      next_mode) const
            {
                assert(next_mode != ScatteringMode::None);

                // No diffuse bounces.
                if (ScatteringMode::has_diffuse(next_mode))
                    return false;

                return true;
            }

            void on_miss(const PathVertex& vertex)
            {
                assert(vertex.m_prev_mode != ScatteringMode::None);

                // Don't compute lighting in the first pass if importons are enabled.
                if (!m_params.m_enable_importons || m_pass_callback.get_pass_number() > 0)
                {
                    // Can't look up the environment if there's no environment EDF.
                    if (m_env_edf == nullptr)
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

                    // Optionally clamp secondary rays contribution.
                    if (m_params.m_path_tracing_has_max_ray_intensity &&
                        vertex.m_path_length > 1 &&
                        vertex.m_prev_mode != ScatteringMode::Specular)
                        clamp_contribution(env_radiance, m_params.m_path_tracing_max_ray_intensity);

                    // Update the path radiance.
                    env_radiance *= vertex.m_throughput;
                    m_path_radiance.add_emission(
                        vertex.m_path_length,
                        vertex.m_aov_mode,
                        env_radiance);
                }
            }

            void on_hit(const PathVertex& vertex)
            {
                if (m_params.m_enable_importons && m_pass_callback.get_pass_number() == 0)
                {
                    // Importons are enabled and this is the importon tracing pass: don't compute any lighting,
                    // instead render a grid of dots to illustrate the fact that we are only tracing importons.
                    if (vertex.m_path_length == 1)
                    {
                        // The point (pi.x, pi.y) is on the lattice with direction vectors (U, V)
                        // if there is an integer solution (p, q) to the following linear system:
                        //   U.x * p + V.x * q = pi.x
                        //   U.y * p + V.y * q = pi.y
                        const Vector2i U(2, 3);
                        const Vector2i V(3, -2);
                        constexpr int Scale = 1;
                        const int denom = std::abs(U.x * V.y - U.y * V.x) * Scale;
                        const Vector2i& pi = m_pixel_context.get_pixel_coords();
                        const int p_num = std::abs(U.x * pi.y - U.y * pi.x);
                        const int q_num = std::abs(V.y * pi.x - V.x * pi.y);
                        const bool on_lattice = p_num % denom == 0 && q_num % denom == 0;
                        m_path_radiance.m_beauty.set(on_lattice ? 0.8f : 0.0f);
                    }
                }
                else
                {
                    // Importons are disabled, or they are enabled and this is not the importon tracing pass:
                    // perform lighting computations as usual.
                    DirectShadingComponents vertex_radiance;

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
                    {
                        Spectrum emitted;
                        vertex.compute_emitted_radiance(m_shading_context, emitted);
                        vertex_radiance.m_emission += emitted;
                        vertex_radiance.m_beauty += emitted;
                    }

                    // Optionally clamp secondary rays contribution.
                    if (m_params.m_path_tracing_has_max_ray_intensity &&
                        vertex.m_path_length > 1 &&
                        vertex.m_prev_mode != ScatteringMode::Specular)
                        clamp_contribution(vertex_radiance, m_params.m_path_tracing_max_ray_intensity);

                    // Update the path radiance.
                    vertex_radiance *= vertex.m_throughput;
                    m_path_radiance.add(vertex.m_path_length, vertex.m_aov_mode, vertex_radiance);
                }

                // Create and store an importon if importons are enabled and no importon has yet been created
                // for that pixel. Storing importons even on all surfaces, including specular ones where no
                // photon density estimation takes place, turns the importon cloud into a faithful and complete
                // representation of the visible / reachable surfaces of the scene. This allows to use the
                // bounding box of the importon cloud as an automatic photon target if the user doesn't provide
                // any photon targets of his own.
                if (m_params.m_enable_importons)
                {
                    const Vector2u pixel_coords(m_pixel_context.get_pixel_coords());
                    if (!m_working_set.m_importon_mask->is_set(pixel_coords.x, pixel_coords.y))
                    {
                        m_working_set.m_importons.push_back(Vector3f(vertex.get_point()));
                        m_working_set.m_importon_mask->set(pixel_coords.x, pixel_coords.y);
                    }
                }
            }

            void on_scatter(PathVertex& vertex)
            {
            }

            void add_direct_lighting_contribution(
                const PathVertex&               vertex,
                DirectShadingComponents&        vertex_radiance)
            {
                DirectShadingComponents dl_radiance;

                const std::size_t light_sample_count =
                    stochastic_cast<std::size_t>(
                        m_sampling_context,
                        m_params.m_dl_light_sample_count);

                if (light_sample_count == 0)
                    return;

                const std::size_t bsdf_sample_count = light_sample_count;

                const BSDFSampler bsdf_sampler(
                    *vertex.m_bsdf,
                    vertex.m_bsdf_data,
                    ScatteringMode::Diffuse,
                    *vertex.m_shading_point);

                // Unlike in the path tracer, we need to sample the diffuse components
                // of the BSDF because we won't extend the path after a diffuse bounce.
                const DirectLightingIntegrator integrator(
                    m_shading_context,
                    m_backward_light_sampler,
                    bsdf_sampler,
                    vertex.m_shading_point->get_time(),
                    ScatteringMode::All,
                    bsdf_sample_count,
                    light_sample_count,
                    m_params.m_dl_low_light_threshold,
                    false);             // not computing indirect lighting

                // Always sample both the lights and the BSDF.
                integrator.compute_outgoing_radiance_combined_sampling_low_variance(
                    vertex.m_sampling_context,
                    vertex.m_outgoing,
                    dl_radiance,
                    nullptr);

                // Divide by the sample count when this number is less than 1.
                if (m_params.m_rcp_dl_light_sample_count > 0.0f)
                    dl_radiance *= m_params.m_rcp_dl_light_sample_count;

                // Add the direct lighting contributions.
                vertex_radiance += dl_radiance;
            }

            void add_photon_map_lighting_contribution(
                const PathVertex&               vertex,
                DirectShadingComponents&        vertex_radiance)
            {
                const SPPMPhotonMap& photon_map = m_pass_callback.get_photon_map();

                // No indirect lighting if the photon map is empty.
                if (photon_map.empty())
                    return;

                const Vector3f point(vertex.get_point());
                const float radius = m_pass_callback.get_photon_lookup_radius();

                // Find the nearby photons around the path vertex.
                const knn::Query3f query(photon_map, m_answer);
                query.run(point, radius * radius);
                const std::size_t photon_count = m_answer.size();

                // Compute the square radius of the lookup disk.
                float max_square_dist;
                if (photon_count < m_params.m_max_photons_per_estimate)
                    max_square_dist = radius * radius;
                else
                {
                    max_square_dist = 0.0f;
                    for (std::size_t i = 0; i < photon_count; ++i)
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
                vertex_radiance.m_diffuse += indirect_radiance;
                vertex_radiance.m_beauty += indirect_radiance;
            }

            void accumulate_mono_photons(
                const PathVertex&               vertex,
                const std::size_t               photon_count,
                const float                     rcp_max_square_dist,
                Spectrum&                       radiance)
            {
                const SPPMPhotonMap& photon_map = m_pass_callback.get_photon_map();
                const Vector3f normal(vertex.get_geometric_normal());

                for (std::size_t i = 0; i < photon_count; ++i)
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
                    const float vdist = std::abs(dot(normal, point_to_photon));
                    if (vdist > 0.1f * radius)
                        continue;
#endif

                    // Evaluate the BSDF for this photon.
                    BSDF::LocalGeometry local_geometry;
                    local_geometry.m_shading_point = vertex.m_shading_point;
                    local_geometry.m_geometric_normal = Vector3f(vertex.get_geometric_normal());
                    local_geometry.m_shading_basis = Basis3f(vertex.get_shading_basis());
                    DirectShadingComponents bsdf_value;
                    const float bsdf_prob =
                        vertex.m_bsdf->evaluate(
                            vertex.m_bsdf_data,
                            false,                                      // not adjoint
                            true,                                       // multiply by |cos(incoming, normal)|
                            local_geometry,
                            Vector3f(vertex.m_outgoing.get_value()),    // toward the camera
                            normalize(photon.m_incoming),               // toward the light
                            ScatteringMode::Diffuse,
                            bsdf_value);
                    if (bsdf_prob == 0.0f)
                        continue;

                    // The photons store flux but we are computing reflected radiance.
                    // The first step of the flux -> radiance conversion is done here.
                    // The conversion will be completed when doing density estimation.
                    float bsdf_mono_value = bsdf_value.m_beauty[photon.m_flux.m_wavelength];
                    bsdf_mono_value /= std::abs(dot(photon.m_incoming, photon.m_geometric_normal));
                    bsdf_mono_value *= photon.m_flux.m_amplitude;

                    // Apply kernel weight.
                    bsdf_mono_value *= epanechnikov2d(entry.m_square_dist * rcp_max_square_dist);

                    // Accumulate reflected flux.
                    radiance[photon.m_flux.m_wavelength] += bsdf_mono_value;
                }
            }

            void accumulate_poly_photons(
                const PathVertex&               vertex,
                const std::size_t               photon_count,
                const float                     rcp_max_square_dist,
                Spectrum&                       radiance)
            {
                const SPPMPhotonMap& photon_map = m_pass_callback.get_photon_map();
                const Vector3f normal(vertex.get_geometric_normal());

                for (std::size_t i = 0; i < photon_count; ++i)
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
                    const float vdist = std::abs(dot(normal, point_to_photon));
                    if (vdist > 0.1f * radius)
                        continue;
#endif

                    // Evaluate the BSDF for this photon.
                    BSDF::LocalGeometry local_geometry;
                    local_geometry.m_shading_point = vertex.m_shading_point;
                    local_geometry.m_geometric_normal = Vector3f(vertex.get_geometric_normal());
                    local_geometry.m_shading_basis = Basis3f(vertex.get_shading_basis());
                    DirectShadingComponents bsdf_value;
                    const float bsdf_prob =
                        vertex.m_bsdf->evaluate(
                            vertex.m_bsdf_data,
                            false,                                      // not adjoint
                            true,                                       // multiply by |cos(incoming, normal)|
                            local_geometry,
                            Vector3f(vertex.m_outgoing.get_value()),    // toward the camera
                            normalize(photon.m_incoming),               // toward the light
                            ScatteringMode::Diffuse,
                            bsdf_value);
                    if (bsdf_prob == 0.0f)
                        continue;

                    // The photons store flux but we are computing reflected radiance.
                    // The first step of the flux -> radiance conversion is done here.
                    // The conversion will be completed when doing density estimation.
                    bsdf_value.m_beauty /= std::abs(dot(photon.m_incoming, photon.m_geometric_normal));
                    bsdf_value.m_beauty *= photon.m_flux;

                    // Apply kernel weight.
                    bsdf_value.m_beauty *= epanechnikov2d(entry.m_square_dist * rcp_max_square_dist);

                    // Accumulate reflected flux.
                    radiance += bsdf_value.m_beauty;
                }
            }
        };

        // todo: move out of this class.
        struct VolumeVisitor
        {
            bool accept_scattering(
                const ScatteringMode::Mode  prev_mode)
            {
                return true;
            }

            void on_scatter(PathVertex& vertex)
            {
            }

            void visit_ray(PathVertex& vertex, const ShadingRay& volume_ray)
            {
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

            const std::size_t photon_count = m_answer.size();

            if (m_params.m_photon_type == SPPMParameters::Monochromatic)
            {
                for (std::size_t i = 0; i < photon_count; ++i)
                {
                    const knn::Answer<float>::Entry& photon = m_answer.get(i);
                    const SpectrumLine& flux =
                        m_pass_callback.get_mono_photon(photon_map.remap(photon.m_index)).m_flux;
                    radiance[flux.m_wavelength] += flux.m_amplitude;
                }
            }
            else
            {
                for (std::size_t i = 0; i < photon_count; ++i)
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

Dictionary SPPMLightingEngineFactory::get_params_metadata()
{
    Dictionary metadata;

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
        "enable_importons",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "true")
            .insert("label", "Enable Importons")
            .insert("help",
                "When checked, \"importons\" are traced to identify important parts of the scene, "
                "and later on photons are only stored in these important parts"));

    metadata.dictionaries().insert(
        "importon_lookup_radius",
        Dictionary()
            .insert("type", "float")
            .insert("default", "5.0")
            .insert("unit", "percent")
            .insert("min", "0.0")
            .insert("max", "100.0")
            .insert("label", "Importon Lookup Radius")
            .insert("help", "Importon lookup radius (in percents of the scene diameter) when deciding whether to store or not a photon"));

    metadata.dictionaries().insert(
        "photon_tracing_max_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "0")
            .insert("label", "Max Photon Bounces")
            .insert("help", "Maximum number of photon bounces"));

    metadata.dictionaries().insert(
        "photon_tracing_rr_min_path_length",
        Dictionary()
            .insert("type", "int")
            .insert("default", "6")
            .insert("help", "Consider pruning low contribution photons starting with this bounce"));

    metadata.dictionaries().insert(
        "path_tracing_max_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "0")
            .insert("label", "Max Bounces")
            .insert("help", "Maximum number of path bounces"));

    metadata.dictionaries().insert(
        "path_tracing_rr_min_path_length",
        Dictionary()
            .insert("type", "int")
            .insert("default", "6")
            .insert("help", "Consider pruning low contribution paths starting with this bounce"));

    metadata.dictionaries().insert(
        "path_tracing_max_ray_intensity",
        Dictionary()
            .insert("type", "float")
            .insert("default", "1.0")
            .insert("unlimited", "true")
            .insert("min", "0.0")
            .insert("label", "Max Ray Intensity")
            .insert("help", "Clamp intensity of rays (after the first bounce) to this value to reduce fireflies"));

    metadata.dictionaries().insert(
        "light_photons_per_pass",
        Dictionary()
            .insert("type", "int")
            .insert("default", "1000000")
            .insert("label", "Light Photons per Pass")
            .insert("help", "Number of light photons per render pass"));

    metadata.dictionaries().insert(
        "env_photons_per_pass",
        Dictionary()
            .insert("type", "int")
            .insert("default", "1000000")
            .insert("label", "IBL Photons per Pass")
            .insert("help", "Number of environment photons per render pass"));

    metadata.dictionaries().insert(
        "initial_photon_lookup_radius",
        Dictionary()
            .insert("type", "float")
            .insert("default", "0.1")
            .insert("unit", "percent")
            .insert("min", "0.0")
            .insert("max", "100.0")
            .insert("label", "Initial Photon Lookup Radius")
            .insert("help", "Initial photon lookup radius (in percents of the scene diameter)"));

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
            .insert("help", "Evolution rate of photon lookup radius"));

    return metadata;
}

SPPMLightingEngineFactory::SPPMLightingEngineFactory(
    SPPMPassCallback&               pass_callback,
    const ForwardLightSampler&      forward_light_sampler,
    const BackwardLightSampler&     backward_light_sampler,
    const SPPMParameters&           params)
  : m_pass_callback(pass_callback)
  , m_forward_light_sampler(forward_light_sampler)
  , m_backward_light_sampler(backward_light_sampler)
  , m_params(params)
{
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
            m_forward_light_sampler,
            m_backward_light_sampler,
            m_params);
}

}   // namespace renderer
