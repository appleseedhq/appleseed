
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/sppm/sppmphoton.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/lighting/tracer.h"
#ifdef WITH_OSL
#include "renderer/kernel/shading/oslshadergroupexec.h"
#endif
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/hash.h"
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/timer.h"
#include "foundation/platform/types.h"
#include "foundation/utility/job.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // A path visitor that creates photons at each non-specular bounces.
    //

    struct PathVisitor
    {
        const Spectrum              m_initial_flux;     // initial particle flux (in W)
        const bool                  m_store_direct;
        const bool                  m_store_indirect;
        const bool                  m_store_caustics;
        SPPMPhotonVector&           m_photons;

        PathVisitor(
            const Spectrum&         initial_flux,
            const bool              store_direct,
            const bool              store_indirect,
            const bool              store_caustics,
            SPPMPhotonVector&       photons)
          : m_initial_flux(initial_flux)
          , m_store_direct(store_direct)
          , m_store_indirect(store_indirect)
          , m_store_caustics(store_caustics)
          , m_photons(photons)
        {
        }

        bool accept_scattering(
            const BSDF::Mode        prev_bsdf_mode,
            const BSDF::Mode        bsdf_mode) const
        {
            assert(bsdf_mode != BSDF::Absorption);

            // Terminate the path at the first vertex if we aren't interested in indirect photons.
            if (!m_store_indirect)
                return false;

            if (!m_store_caustics)
            {
                // Don't follow paths leading to caustics.
                if (BSDF::has_glossy_or_specular(bsdf_mode))
                    return false;
            }

            return true;
        }

        void visit_vertex(const PathVertex& vertex)
        {
            if (vertex.m_path_length > 1 || m_store_direct)
            {
                // Don't store photons on surfaces without a BSDF.
                if (vertex.m_bsdf == 0)
                    return;

                // Don't store photons on purely specular surfaces.
                if (vertex.m_bsdf->is_purely_specular())
                    return;

                // Create and store a new photon.
                SPPMPhoton photon;
                photon.m_position = vertex.get_point();
                photon.m_data.m_incoming = Vector3f(vertex.m_outgoing);
                photon.m_data.m_geometric_normal = Vector3f(vertex.get_geometric_normal());
                photon.m_data.m_flux = m_initial_flux;
                photon.m_data.m_flux *= vertex.m_throughput;
                m_photons.push_back(photon);
            }
        }

        void visit_environment(const PathVertex& vertex)
        {
            // The photon escapes, nothing to do.
        }
    };


    //
    // A job to trace a packet of photons from area lights and non-physical lights.
    //

    class LightPhotonTracingJob
      : public IJob
    {
      public:
        LightPhotonTracingJob(
            const Scene&            scene,
            const LightSampler&     light_sampler,
            const TraceContext&     trace_context,
            TextureStore&           texture_store,
            const SPPMParameters&   params,
            SPPMPhotonVector&       global_photons,
            const size_t            photon_begin,
            const size_t            photon_end,
            const size_t            pass_hash,
            const size_t            thread_index,
#ifdef WITH_OIIO
            OIIO::TextureSystem&    oiio_texture_system,
#endif
#ifdef WITH_OSL
            OSL::ShadingSystem&     shading_system,
#endif
            AbortSwitch&            abort_switch)
          : m_scene(scene)
          , m_light_sampler(light_sampler)
          , m_texture_cache(texture_store)
          , m_intersector(trace_context, m_texture_cache)
          , m_params(params)
          , m_global_photons(global_photons)
          , m_photon_begin(photon_begin)
          , m_photon_end(photon_end)
          , m_pass_hash(pass_hash)
#ifdef WITH_OSL
          , m_shadergroup_exec(shading_system)
#endif
          , m_tracer(
                m_scene,
                m_intersector,
                m_texture_cache,
#ifdef WITH_OSL
                m_shadergroup_exec,
#endif
                m_params.m_transparency_threshold,
                m_params.m_max_iterations,
                false)
          , m_shading_context(
                m_intersector,
                m_tracer,
                m_texture_cache
#ifdef WITH_OIIO
                , oiio_texture_system
#endif
#ifdef WITH_OSL
                , m_shadergroup_exec
#endif
                , thread_index)
          , m_abort_switch(abort_switch)
        {
        }

        virtual void execute(const size_t thread_index) OVERRIDE
        {
            const uint32 instance = hash_uint32(static_cast<uint32>(m_pass_hash + m_photon_begin));
            MersenneTwister rng(instance);
            SamplingContext sampling_context(
                rng,
                4,                  // number of dimensions
                0,                  // number of samples -- unknown
                instance);          // initial instance number

            for (size_t i = m_photon_begin; i < m_photon_end && !m_abort_switch.is_aborted(); ++i)
                trace_light_photon(sampling_context);

            m_global_photons.append(m_local_photons);
        }

      private:
        const Scene&                m_scene;
        const LightSampler&         m_light_sampler;
        TextureCache                m_texture_cache;
        Intersector                 m_intersector;
        const SPPMParameters        m_params;
        SPPMPhotonVector&           m_global_photons;
        const size_t                m_photon_begin;
        const size_t                m_photon_end;
        const size_t                m_pass_hash;
        AbortSwitch&                m_abort_switch;
        SPPMPhotonVector            m_local_photons;
#ifdef WITH_OSL
        OSLShaderGroupExec          m_shadergroup_exec;
#endif
        Tracer                      m_tracer;
        ShadingContext              m_shading_context;

        void trace_light_photon(
            SamplingContext&        sampling_context)
        {
            LightSample light_sample;
            m_light_sampler.sample(sampling_context.next_vector2<4>(), light_sample);

            if (light_sample.m_triangle)
            {
                trace_emitting_triangle_photon(
                    sampling_context,
                    light_sample);
            }
            else
            {
                trace_non_physical_light_photon(
                    sampling_context,
                    light_sample);
            }
        }

        void trace_emitting_triangle_photon(
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
            edf->evaluate_inputs(input_evaluator, light_sample.m_bary);

            // Sample the EDF.
            SamplingContext child_sampling_context = sampling_context.split(2, 1);
            Vector3d emission_direction;
            Spectrum edf_value;
            double edf_prob;
            edf->sample(
                input_evaluator.data(),
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
                        / (light_sample.m_probability * edf_prob * m_params.m_light_photon_count));

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
                ShadingRay::LightRay);

            // Build the path tracer.
            const bool cast_indirect_light = (edf->get_flags() & EDF::CastIndirectLight) != 0;
            PathVisitor path_visitor(
                initial_flux,
                m_params.m_dl_mode == SPPMParameters::SPPM, // store direct lighting photons?
                cast_indirect_light,
                m_params.m_enable_caustics,
                m_local_photons);
            PathTracer<PathVisitor, true> path_tracer(      // true = adjoint
                path_visitor,
                m_params.m_photon_tracing_rr_min_path_length,
                m_params.m_photon_tracing_max_path_length,
                m_params.m_max_iterations,
                edf->get_light_near_start());               // don't illuminate points closer than the light near start value

            // Trace the photon path.
            path_tracer.trace(
                child_sampling_context,
                m_shading_context,
                ray,
                &parent_shading_point);
        }

        void trace_non_physical_light_photon(
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
            initial_flux /= static_cast<float>(light_sample.m_probability * light_prob * m_params.m_light_photon_count);

            // Build the photon ray.
            child_sampling_context.split_in_place(1, 1);
            const ShadingRay ray(
                emission_position,
                emission_direction,
                child_sampling_context.next_double2(),
                ShadingRay::LightRay);

            // Build the path tracer.
            const bool cast_indirect_light = (light_sample.m_light->get_flags() & EDF::CastIndirectLight) != 0;
            PathVisitor path_visitor(
                initial_flux,
                m_params.m_dl_mode == SPPMParameters::SPPM, // store direct lighting photons?
                cast_indirect_light,
                m_params.m_enable_caustics,
                m_local_photons);
            PathTracer<PathVisitor, true> path_tracer(      // true = adjoint
                path_visitor,
                m_params.m_photon_tracing_rr_min_path_length,
                m_params.m_photon_tracing_max_path_length,
                m_params.m_max_iterations);

            // Trace the photon path.
            path_tracer.trace(
                child_sampling_context,
                m_shading_context,
                ray);
        }
    };


    //
    // A job to trace a packet of photons from the environment.
    //

    class EnvironmentPhotonTracingJob
      : public IJob
    {
      public:
        EnvironmentPhotonTracingJob(
            const Scene&            scene,
            const LightSampler&     light_sampler,
            const TraceContext&     trace_context,
            TextureStore&           texture_store,
            const SPPMParameters&   params,
            SPPMPhotonVector&       global_photons,
            const size_t            photon_begin,
            const size_t            photon_end,
            const size_t            pass_hash,
            const size_t            thread_index,
#ifdef WITH_OIIO
            OIIO::TextureSystem&    oiio_texture_system,
#endif
#ifdef WITH_OSL
            OSL::ShadingSystem&     shading_system,
#endif
            AbortSwitch&            abort_switch)
          : m_scene(scene)
          , m_env_edf(*scene.get_environment()->get_environment_edf())
          , m_light_sampler(light_sampler)
          , m_texture_cache(texture_store)
          , m_intersector(trace_context, m_texture_cache)
          , m_params(params)
          , m_global_photons(global_photons)
          , m_photon_begin(photon_begin)
          , m_photon_end(photon_end)
          , m_pass_hash(pass_hash)
          , m_abort_switch(abort_switch)
          , m_safe_scene_radius(scene.compute_radius() * (1.0 + 1.0e-3))
          , m_disk_point_prob(1.0 / (Pi * square(m_safe_scene_radius)))
#ifdef WITH_OSL
          , m_shadergroup_exec(shading_system)
#endif
          , m_tracer(
                m_scene,
                m_intersector,
                m_texture_cache,
#ifdef WITH_OSL
                m_shadergroup_exec,
#endif
                m_params.m_transparency_threshold,
                m_params.m_max_iterations,
                false)
          , m_shading_context(
                m_intersector,
                m_tracer,
                m_texture_cache
#ifdef WITH_OIIO
                , oiio_texture_system
#endif
#ifdef WITH_OSL
                , m_shadergroup_exec
#endif
                , thread_index)
        {
        }

        virtual void execute(const size_t thread_index) OVERRIDE
        {
            const uint32 instance = hash_uint32(static_cast<uint32>(m_pass_hash + m_photon_begin));
            MersenneTwister rng(instance);
            SamplingContext sampling_context(
                rng,
                2,                  // number of dimensions
                0,                  // number of samples -- unknown
                instance);          // initial instance number

            for (size_t i = m_photon_begin; i < m_photon_end && !m_abort_switch.is_aborted(); ++i)
                trace_env_photon(sampling_context);

            m_global_photons.append(m_local_photons);
        }

      private:
        const Scene&                m_scene;
        const EnvironmentEDF&       m_env_edf;
        const LightSampler&         m_light_sampler;
        TextureCache                m_texture_cache;
        Intersector                 m_intersector;
        const SPPMParameters        m_params;
        SPPMPhotonVector&           m_global_photons;
        const size_t                m_photon_begin;
        const size_t                m_photon_end;
        const size_t                m_pass_hash;
        AbortSwitch&                m_abort_switch;
        const double                m_safe_scene_radius;
        const double                m_disk_point_prob;
        SPPMPhotonVector            m_local_photons;
#ifdef WITH_OSL
        OSLShaderGroupExec          m_shadergroup_exec;
#endif
        Tracer                      m_tracer;
        ShadingContext              m_shading_context;

        void trace_env_photon(
            SamplingContext&        sampling_context)
        {
            // Sample the environment.
            InputEvaluator input_evaluator(m_texture_cache);
            Vector3d outgoing;
            Spectrum env_edf_value;
            double env_edf_prob;
            m_env_edf.sample(
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
            initial_flux /= static_cast<float>(m_disk_point_prob * env_edf_prob * m_params.m_env_photon_count);

            // Build the photon ray.
            child_sampling_context.split_in_place(1, 1);
            const ShadingRay ray(
                ray_origin,
                -outgoing,
                child_sampling_context.next_double2(),
                ShadingRay::LightRay);

            // Build the path tracer.
            const bool cast_indirect_light = true;          // right now environments always cast indirect light
            PathVisitor path_visitor(
                initial_flux,
                m_params.m_enable_ibl,
                cast_indirect_light,
                m_params.m_enable_caustics,
                m_local_photons);
            PathTracer<PathVisitor, true> path_tracer(      // true = adjoint
                path_visitor,
                m_params.m_photon_tracing_rr_min_path_length,
                m_params.m_photon_tracing_max_path_length,
                m_params.m_max_iterations);

            // Trace the photon path.
            path_tracer.trace(
                child_sampling_context,
                m_shading_context,
                ray);
        }
    };
}


//
// SPPMPhotonTracer class implementation.
//

SPPMPhotonTracer::SPPMPhotonTracer(
    const Scene&            scene,
    const LightSampler&     light_sampler,
    const TraceContext&     trace_context,
    TextureStore&           texture_store,
#ifdef WITH_OIIO
    OIIO::TextureSystem&    oiio_texture_system,
#endif
#ifdef WITH_OSL
    OSL::ShadingSystem&     shading_system,
#endif
    const SPPMParameters&   params)
  : m_params(params)
  , m_scene(scene)
  , m_light_sampler(light_sampler)
  , m_trace_context(trace_context)
  , m_texture_store(texture_store)
  , m_total_emitted_photon_count(0)
  , m_total_stored_photon_count(0)
#ifdef WITH_OIIO
  , m_oiio_texture_system(oiio_texture_system)
#endif
#ifdef WITH_OSL
  , m_shading_system(shading_system)
#endif
{
}

void SPPMPhotonTracer::trace_photons(
    SPPMPhotonVector&       photons,
    const size_t            pass_hash,
    JobQueue&               job_queue,
    AbortSwitch&            abort_switch)
{
    // Start stopwatch.
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    // Schedule photon tracing jobs.
    size_t job_count = 0;
    size_t emitted_photon_count = 0;
    if (m_light_sampler.has_lights_or_emitting_triangles())
    {
        schedule_light_photon_tracing_jobs(
            photons,
            pass_hash,
            job_queue,
            job_count,
            emitted_photon_count,
            abort_switch);
    }
    if (m_scene.get_environment()->get_environment_edf())
    {
        schedule_environment_photon_tracing_jobs(
            photons,
            pass_hash,
            job_queue,
            job_count,
            emitted_photon_count,
            abort_switch);
    }

    // Wait until the photon tracing jobs have completed.
    job_queue.wait_until_completion();

    // Update photon tracing statistics.
    m_total_emitted_photon_count += emitted_photon_count;
    m_total_stored_photon_count += photons.size();

    // Print photon tracing statistics.
    Statistics statistics;
    statistics.insert("tracing jobs", job_count);
    statistics.insert_time("tracing time", stopwatch.measure().get_seconds());
    statistics.insert("total emitted", m_total_emitted_photon_count);
    statistics.insert(
        "total stored",
        pretty_uint(m_total_stored_photon_count) + " (" +
        pretty_percent(m_total_stored_photon_count, m_total_emitted_photon_count) +
        ")");
    RENDERER_LOG_DEBUG("%s",
        StatisticsVector::make(
            "sppm photon tracing statistics",
            statistics).to_string().c_str());
}

void SPPMPhotonTracer::schedule_light_photon_tracing_jobs(
    SPPMPhotonVector&       photons,
    const size_t            pass_hash,
    JobQueue&               job_queue,
    size_t&                 job_count,
    size_t&                 emitted_photon_count,
    AbortSwitch&            abort_switch)
{
    RENDERER_LOG_INFO(
        "tracing %s sppm light %s...",
        pretty_uint(m_params.m_light_photon_count).c_str(),
        m_params.m_light_photon_count > 1 ? "photons" : "photon");

    for (size_t i = 0, thread_index = 0; i < m_params.m_light_photon_count; i += m_params.m_photon_packet_size, ++thread_index)
    {
        const size_t photon_begin = i;
        const size_t photon_end = min(i + m_params.m_photon_packet_size, m_params.m_light_photon_count);

        job_queue.schedule(
            new LightPhotonTracingJob(
                m_scene,
                m_light_sampler,
                m_trace_context,
                m_texture_store,
                m_params,
                photons,
                photon_begin,
                photon_end,
                pass_hash,
                thread_index,
#ifdef WITH_OIIO
                m_oiio_texture_system,
#endif
#ifdef WITH_OSL
                m_shading_system,
#endif
                abort_switch));

        ++job_count;
        emitted_photon_count += photon_end - photon_begin;
    }
}

void SPPMPhotonTracer::schedule_environment_photon_tracing_jobs(
    SPPMPhotonVector&       photons,
    const size_t            pass_hash,
    JobQueue&               job_queue,
    size_t&                 job_count,
    size_t&                 emitted_photon_count,
    AbortSwitch&            abort_switch)
{
    RENDERER_LOG_INFO(
        "tracing %s sppm environment %s...",
        pretty_uint(m_params.m_env_photon_count).c_str(),
        m_params.m_env_photon_count > 1 ? "photons" : "photon");

    for (size_t i = 0, thread_index = 0; i < m_params.m_env_photon_count; i += m_params.m_photon_packet_size, ++thread_index)
    {
        const size_t photon_begin = i;
        const size_t photon_end = min(i + m_params.m_photon_packet_size, m_params.m_env_photon_count);

        job_queue.schedule(
            new EnvironmentPhotonTracingJob(
                m_scene,
                m_light_sampler,
                m_trace_context,
                m_texture_store,
                m_params,
                photons,
                photon_begin,
                photon_end,
                pass_hash,
                thread_index,
#ifdef WITH_OIIO
                m_oiio_texture_system,
#endif
#ifdef WITH_OSL
                m_shading_system,
#endif
                abort_switch));

        ++job_count;
        emitted_photon_count += photon_end - photon_begin;
    }
}

}   // namespace renderer
