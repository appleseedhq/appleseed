
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
#include "sppmphotontracer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/lighting/sppm/sppmphoton.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/hash.h"
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/job.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace foundation;

namespace renderer
{

//
// SPPMPhotonTracer class implementation.
//

SPPMPhotonTracer::SPPMPhotonTracer(
    const Scene&            scene,
    const LightSampler&     light_sampler,
    const TraceContext&     trace_context,
    TextureStore&           texture_store,
    const ParamArray&       params)
  : m_params(params)
  , m_scene(scene)
  , m_light_sampler(light_sampler)
  , m_texture_cache(texture_store)
  , m_intersector(trace_context, m_texture_cache /*, m_params.m_report_self_intersections*/)
  , m_safe_scene_radius(scene.compute_radius() * (1.0 + 1.0e-3))
  , m_disk_point_prob(1.0 / (Pi * square(m_safe_scene_radius)))
{
}

size_t SPPMPhotonTracer::trace_photons(
    SPPMPhotonVector&       photons,
    const size_t            pass_hash,
    AbortSwitch&            abort_switch)
{
    size_t emitted_photon_count = 0;

    MersenneTwister rng(static_cast<uint32>(pass_hash));
    SamplingContext sampling_context(
        rng,
        0,                  // number of dimensions
        pass_hash,          // number of samples
        pass_hash);         // initial instance number -- end of sequence

    if (m_light_sampler.has_lights_or_emitting_triangles())
    {
        RENDERER_LOG_INFO(
            "tracing %s sppm light %s...",
            pretty_uint(m_params.m_light_photon_count).c_str(),
            m_params.m_light_photon_count > 1 ? "photons" : "photon");

        sampling_context.split_in_place(4, m_params.m_light_photon_count);

        for (size_t i = 0; i < m_params.m_light_photon_count && !abort_switch.is_aborted(); ++i)
        {
            trace_light_photon(photons, sampling_context);
            ++emitted_photon_count;
        }
    }

    const EnvironmentEDF* env_edf = m_scene.get_environment()->get_environment_edf();

    if (env_edf)
    {
        RENDERER_LOG_INFO(
            "tracing %s sppm environment %s...",
            pretty_uint(m_params.m_env_photon_count).c_str(),
            m_params.m_env_photon_count > 1 ? "photons" : "photon");

        sampling_context.split_in_place(2, m_params.m_env_photon_count);

        for (size_t i = 0; i < m_params.m_env_photon_count && !abort_switch.is_aborted(); ++i)
        {
            trace_env_photon(photons, sampling_context, env_edf);
            ++emitted_photon_count;
        }
    }

    return emitted_photon_count;
}

void SPPMPhotonTracer::trace_light_photon(
    SPPMPhotonVector&       photons,
    SamplingContext&        sampling_context)
{
    LightSample light_sample;
    m_light_sampler.sample(sampling_context.next_vector2<4>(), light_sample);

    if (light_sample.m_triangle)
    {
        trace_emitting_triangle_photon(
            photons,
            sampling_context,
            light_sample);
    }
    else
    {
        trace_non_physical_light_photon(
            photons,
            sampling_context,
            light_sample);
    }
}

namespace
{
    struct PathVisitor
    {
        const Spectrum          m_initial_flux;         // initial particle flux (in W)
        const bool              m_cast_indirect_light;
        SPPMPhotonVector&       m_photons;

        PathVisitor(
            const Spectrum&     initial_flux,
            const bool          cast_indirect_light,
            SPPMPhotonVector&   photons)
          : m_initial_flux(initial_flux)
          , m_cast_indirect_light(cast_indirect_light)
          , m_photons(photons)
        {
        }

        bool accept_scattering_mode(
            const BSDF::Mode    prev_bsdf_mode,
            const BSDF::Mode    bsdf_mode) const
        {
            assert(bsdf_mode != BSDF::Absorption);
            return true;
        }

        bool visit_vertex(const PathVertex& vertex)
        {
            assert(vertex.m_path_length == 1 || m_cast_indirect_light);

            // Don't store photons on surfaces without a BSDF.
            if (vertex.m_bsdf == 0)
                return true;

            // Don't store photons on purely specular surfaces.
            if (vertex.m_bsdf->is_purely_specular())
                return true;

            SPPMPhoton photon;
            photon.m_position = vertex.get_point();
            photon.m_data.m_incoming = Vector3f(vertex.m_outgoing);
            photon.m_data.m_geometric_normal = Vector3f(vertex.get_geometric_normal());
            photon.m_data.m_flux = m_initial_flux;
            photon.m_data.m_flux *= vertex.m_throughput;
            m_photons.push_back(photon);

            return m_cast_indirect_light;
        }

        void visit_environment(
            const ShadingPoint& shading_point,
            const Vector3d&     outgoing,
            const BSDF::Mode    prev_bsdf_mode,
            const double        prev_bsdf_prob,
            const Spectrum&     throughput)
        {
            // The photon escapes, nothing to do.
        }
    };
}

void SPPMPhotonTracer::trace_emitting_triangle_photon(
    SPPMPhotonVector&       photons,
    SamplingContext&        sampling_context,
    LightSample&            light_sample)
{
    // Make sure the geometric normal of the light sample is in the same hemisphere as the shading normal.
    light_sample.m_geometric_normal =
        flip_to_same_hemisphere(
            light_sample.m_geometric_normal,
            light_sample.m_shading_normal);

    const EDF* edf = light_sample.m_triangle->m_edf;

    // Evaluate the EDF inputs.
    InputEvaluator input_evaluator(m_texture_cache);
    const void* edf_data =
        input_evaluator.evaluate(edf->get_inputs(), light_sample.m_bary);

    // Sample the EDF.
    SamplingContext child_sampling_context = sampling_context.split(2, 1);
    Vector3d emission_direction;
    Spectrum edf_value;
    double edf_prob;
    edf->sample(
        edf_data,
        light_sample.m_geometric_normal,
        Basis3d(light_sample.m_shading_normal),
        child_sampling_context.next_vector2<2>(),
        emission_direction,
        edf_value,
        edf_prob);

    // Compute the initial particle weight.
    Spectrum initial_flux = edf_value;
    initial_flux *=
        static_cast<float>(
            dot(emission_direction, light_sample.m_shading_normal)
                / (light_sample.m_probability * edf_prob));

    // Make a shading point that will be used to avoid self-intersections with the light sample.
    ShadingPoint parent_shading_point;
    light_sample.make_shading_point(
        parent_shading_point,
        emission_direction,
        m_intersector);

    // Build the photon ray.
    child_sampling_context.split_in_place(1, 1);
    const ShadingRay ray(
        light_sample.m_point,
        emission_direction,
        child_sampling_context.next_double2(),
        ~0);

    // Build the path tracer.
    const bool cast_indirect_light = (edf->get_flags() & EDF::CastIndirectLight) != 0;
    PathVisitor path_visitor(
        initial_flux,
        cast_indirect_light,
        photons);
    PathTracer<PathVisitor, true> path_tracer(      // true = adjoint
        path_visitor,
        m_params.m_rr_min_path_length,
        m_params.m_max_path_length,
        m_params.m_max_iterations);

    // Trace the photon path.
    path_tracer.trace(
        child_sampling_context,
        m_intersector,
        m_texture_cache,
        ray,
        &parent_shading_point);
}

void SPPMPhotonTracer::trace_non_physical_light_photon(
    SPPMPhotonVector&       photons,
    SamplingContext&        sampling_context,
    const LightSample&      light_sample)
{
    // Sample the light.
    InputEvaluator input_evaluator(m_texture_cache);
    SamplingContext child_sampling_context = sampling_context.split(2, 1);
    Vector3d emission_position, emission_direction;
    Spectrum light_value;
    double light_prob;
    light_sample.m_light->sample(
        input_evaluator,
        child_sampling_context.next_vector2<2>(),
        emission_position,
        emission_direction,
        light_value,
        light_prob);

    // Transform the emission position and direction from assembly space to world space.
    emission_position = light_sample.m_light_transform.point_to_parent(emission_position);
    emission_direction = normalize(light_sample.m_light_transform.vector_to_parent(emission_direction));

    // Compute the initial particle weight.
    Spectrum initial_flux = light_value;
    initial_flux /= static_cast<float>(light_sample.m_probability * light_prob);

    // Build the photon ray.
    child_sampling_context.split_in_place(1, 1);
    const ShadingRay ray(
        emission_position,
        emission_direction,
        child_sampling_context.next_double2(),
        ~0);

    // Build the path tracer.
    const bool cast_indirect_light = (light_sample.m_light->get_flags() & EDF::CastIndirectLight) != 0;
    PathVisitor path_visitor(
        initial_flux,
        cast_indirect_light,
        photons);
    PathTracer<PathVisitor, true> path_tracer(      // true = adjoint
        path_visitor,
        m_params.m_rr_min_path_length,
        m_params.m_max_path_length,
        m_params.m_max_iterations);

    // Trace the photon path.
    path_tracer.trace(
        child_sampling_context,
        m_intersector,
        m_texture_cache,
        ray);
}

void SPPMPhotonTracer::trace_env_photon(
    SPPMPhotonVector&       photons,
    SamplingContext&        sampling_context,
    const EnvironmentEDF*   env_edf)
{
    // Sample the environment.
    InputEvaluator input_evaluator(m_texture_cache);
    Vector3d outgoing;
    Spectrum env_edf_value;
    double env_edf_prob;
    env_edf->sample(
        input_evaluator,
        sampling_context.next_vector2<2>(),
        outgoing,                                   // points toward the environment
        env_edf_value,
        env_edf_prob);

    // Compute the center of the tangent disk.
    const Vector3d disk_center = m_safe_scene_radius * outgoing;

    // Uniformly sample the tangent disk.
    SamplingContext child_sampling_context = sampling_context.split(2, 1);
    const Vector2d disk_point =
        m_safe_scene_radius *
        sample_disk_uniform(child_sampling_context.next_vector2<2>());

    // Compute the origin of the photon ray.
    const Basis3d basis(-outgoing);
    const Vector3d ray_origin =
        disk_center +
        disk_point[0] * basis.get_tangent_u() +
        disk_point[1] * basis.get_tangent_v();

    // Compute the initial particle weight.
    Spectrum initial_flux = env_edf_value;
    initial_flux /= static_cast<float>(m_disk_point_prob * env_edf_prob);

    // Build the photon ray.
    child_sampling_context.split_in_place(1, 1);
    const ShadingRay ray(
        ray_origin,
        -outgoing,
        child_sampling_context.next_double2(),
        ~0);

    // Build the path tracer.
    const bool cast_indirect_light = true;          // right now environments always cast indirect light
    PathVisitor path_visitor(
        initial_flux,
        cast_indirect_light,
        photons);
    PathTracer<PathVisitor, true> path_tracer(      // true = adjoint
        path_visitor,
        m_params.m_rr_min_path_length,
        m_params.m_max_path_length,
        m_params.m_max_iterations);

    // Trace the photon path.
    path_tracer.trace(
        child_sampling_context,
        m_intersector,
        m_texture_cache,
        ray);
}


//
// SPPMPhotonTracer::Parameters class implementation.
//

namespace
{
    static size_t nz(const size_t x)
    {
        return x == 0 ? ~0 : x;
    }
}

SPPMPhotonTracer::Parameters::Parameters(const ParamArray& params)
  : m_light_photon_count(params.get_optional<size_t>("light_photons_per_pass", 100000))
  , m_env_photon_count(params.get_optional<size_t>("env_photons_per_pass", 100000))
  , m_max_path_length(nz(params.get_optional<size_t>("max_path_length", 0)))
  , m_rr_min_path_length(nz(params.get_optional<size_t>("rr_min_path_length", 3)))
  , m_max_iterations(params.get_optional<size_t>("max_iterations", 1000))
{
}

}   // namespace renderer
