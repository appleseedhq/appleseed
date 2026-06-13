
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
#include "sppmphotontracer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/forwardlightsampler.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/lighting/sppm/sppmimportonmap.h"
#include "renderer/kernel/lighting/sppm/sppmphoton.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/texturing/oiiotexturesystem.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/light/lighttarget.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/hash/hash.h"
#include "foundation/math/basis.h"
#include "foundation/math/knn/knn_anyquery.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/memory/arena.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/timers.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/job.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cassert>

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
        Spectrum                    m_initial_flux;     // initial particle flux (in W)
        const SPPMParameters&       m_params;
        const SPPMImportonMap*      m_importon_map;
        const float                 m_importon_lookup_radius;
        const bool                  m_store_direct;
        const bool                  m_store_indirect;
        const bool                  m_store_caustics;
        SPPMPhotonVector&           m_photons;

        PathVisitor(
            const Spectrum&         initial_flux,
            const SPPMParameters&   params,
            const SPPMImportonMap*  importon_map,
            const float             importon_lookup_radius,
            const bool              store_direct,
            const bool              store_indirect,
            const bool              store_caustics,
            SPPMPhotonVector&       photons)
          : m_initial_flux(initial_flux)
          , m_params(params)
          , m_importon_map(importon_map)
          , m_importon_lookup_radius(importon_lookup_radius)
          , m_store_direct(store_direct)
          , m_store_indirect(store_indirect)
          , m_store_caustics(store_caustics)
          , m_photons(photons)
        {
        }

        void on_first_diffuse_bounce(
            const PathVertex&           vertex,
            const Spectrum&             albedo)
        {
        }

        bool accept_scattering(
            const ScatteringMode::Mode  prev_mode,
            const ScatteringMode::Mode  next_mode) const
        {
            assert(next_mode != ScatteringMode::None);

            // Terminate the path at the first vertex if we aren't interested in indirect photons.
            if (!m_store_indirect)
                return false;

            if (!m_store_caustics)
            {
                // Don't follow paths leading to caustics.
                if (ScatteringMode::has_glossy_or_specular(next_mode))
                    return false;
            }

            return true;
        }

        void on_miss(const PathVertex& vertex)
        {
            // The photon escapes.
        }

        void on_hit(const PathVertex& vertex)
        {
            if (vertex.m_path_length > 1 || m_store_direct)
            {
                // Don't store photons on surfaces without a BSDF.
                if (vertex.m_bsdf == nullptr)
                    return;

                // Don't store photons on purely specular surfaces.
                if (vertex.m_bsdf->is_purely_specular())
                    return;

                const Vector3f point(vertex.get_point());

                // Check the importon map if there is one.
                if (m_importon_map != nullptr)
                {
                    const knn::AnyQuery3f query(*m_importon_map);
                    if (!query.run(point, square(m_importon_lookup_radius)))
                        return;
                }

                if (m_params.m_photon_type == SPPMParameters::Monochromatic)
                {
                    // Choose a wavelength at random.
                    vertex.m_sampling_context.split_in_place(1, 1);
                    const std::uint32_t wavelength =
                        truncate<std::uint32_t>(
                            vertex.m_sampling_context.next2<double>() * Spectrum::size());

                    // Create and store a new photon.
                    SPPMMonoPhoton photon;
                    photon.m_incoming = Vector3f(vertex.m_outgoing.get_value());
                    photon.m_geometric_normal = Vector3f(vertex.get_geometric_normal());
                    photon.m_flux.m_wavelength = wavelength;
                    photon.m_flux.m_amplitude =
                        m_initial_flux[wavelength] *
                        Spectrum::size() *
                        vertex.m_throughput[wavelength];
                    m_photons.push_back(point, photon);
                }
                else
                {
                    assert(m_params.m_photon_type == SPPMParameters::Polychromatic);

                    // Create and store a new photon.
                    SPPMPolyPhoton photon;
                    photon.m_incoming = Vector3f(vertex.m_outgoing.get_value());
                    photon.m_geometric_normal = Vector3f(vertex.get_geometric_normal());
                    photon.m_flux = m_initial_flux;
                    photon.m_flux *= vertex.m_throughput;
                    m_photons.push_back(point, photon);
                }
            }
        }

        void on_scatter(PathVertex& vertex)
        {
        }

        // This function is added for consistence with pt engine interface and it will
        // be called in the path tracer, do nothing here.
        void on_terminate(const TerminateType& terminate_type)
        {
        }
    };


    //
    // Volume visitor that does nothing.
    //

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


    //
    // A job to trace a packet of photons from area lights and non-physical lights.
    //

    class LightPhotonTracingJob
      : public IJob
    {
      public:
        LightPhotonTracingJob(
            const Scene&                    scene,
            const LightTargetArray&         photon_targets,
            const SPPMImportonMap*          importon_map,
            const float                     importon_lookup_radius,
            const ForwardLightSampler&      light_sampler,
            const TraceContext&             trace_context,
            TextureStore&                   texture_store,
            OIIOTextureSystem&              oiio_texture_system,
            OSLShadingSystem&               shading_system,
            const SPPMParameters&           params,
            SPPMPhotonVector&               global_photons,
            const size_t                    photon_begin,
            const size_t                    photon_end,
            const std::uint32_t             pass_hash,
            IAbortSwitch&                   abort_switch)
          : m_scene(scene)
          , m_photon_targets(photon_targets)
          , m_importon_map(importon_map)
          , m_importon_lookup_radius(importon_lookup_radius)
          , m_light_sampler(light_sampler)
          , m_texture_cache(texture_store)
          , m_intersector(trace_context, m_texture_cache)
          , m_oiio_texture_system(oiio_texture_system)
          , m_shadergroup_exec(shading_system, m_arena)
          , m_params(params)
          , m_tracer(
                m_scene,
                m_intersector,
                m_shadergroup_exec,
                m_params.m_transparency_threshold,
                m_params.m_max_iterations,
                false)
          , m_global_photons(global_photons)
          , m_photon_begin(photon_begin)
          , m_photon_end(photon_end)
          , m_pass_hash(pass_hash)
          , m_abort_switch(abort_switch)
        {
            const Camera* camera = scene.get_render_data().m_active_camera;
            m_shutter_open_begin_time = camera->get_shutter_open_begin_time();
            m_shutter_close_end_time = camera->get_shutter_close_end_time();
        }

        void execute(const size_t thread_index) override
        {
            // Initialize thread-local variables.
            Spectrum::set_mode(m_params.m_spectrum_mode);

            const ShadingContext shading_context(
                m_intersector,
                m_tracer,
                m_texture_cache,
                m_oiio_texture_system,
                m_shadergroup_exec,
                m_arena,
                thread_index);

            const size_t instance = hash_uint32(static_cast<std::uint32_t>(m_pass_hash + m_photon_begin));
            SamplingContext::RNGType rng(m_pass_hash, instance);
            SamplingContext sampling_context(
                rng,
                m_params.m_sampling_mode,
                4,                          // number of dimensions
                0,                          // number of samples -- unknown
                instance);                  // initial instance number

            for (size_t i = m_photon_begin; i < m_photon_end && !m_abort_switch.is_aborted(); ++i)
            {
                m_arena.clear();

                // Generate a uniform sample in [0,1)^4.
                const Vector4f light_sample_s = sampling_context.next2<Vector4f>();

                // Trace a photon.
                SamplingContext child_sampling_context(sampling_context);
                trace_light_photon(shading_context, child_sampling_context, light_sample_s);
            }

            m_global_photons.append(m_local_photons);
        }

      private:
        const Scene&                m_scene;
        const LightTargetArray&     m_photon_targets;
        const SPPMImportonMap*      m_importon_map;
        const float                 m_importon_lookup_radius;
        const ForwardLightSampler&  m_light_sampler;
        TextureCache                m_texture_cache;
        Intersector                 m_intersector;
        OIIOTextureSystem&          m_oiio_texture_system;
        Arena                       m_arena;
        OSLShaderGroupExec          m_shadergroup_exec;
        const SPPMParameters        m_params;
        Tracer                      m_tracer;
        SPPMPhotonVector&           m_global_photons;
        const size_t                m_photon_begin;
        const size_t                m_photon_end;
        const std::uint32_t         m_pass_hash;
        IAbortSwitch&               m_abort_switch;
        SPPMPhotonVector            m_local_photons;
        float                       m_shutter_open_begin_time;
        float                       m_shutter_close_end_time;

        void trace_light_photon(
            const ShadingContext&   shading_context,
            SamplingContext&        sampling_context,
            const Vector4f&         light_sample_s)
        {
            LightSample light_sample;
            m_light_sampler.sample(
                ShadingRay::Time::create_with_normalized_time(
                    light_sample_s[0],
                    m_shutter_open_begin_time,
                    m_shutter_close_end_time),
                Vector3f(light_sample_s[1], light_sample_s[2], light_sample_s[3]),
                light_sample);

            if (light_sample.m_shape)
            {
                trace_emitting_shape_photon(
                    shading_context,
                    sampling_context,
                    light_sample);
            }
            else
            {
                trace_non_physical_light_photon(
                    shading_context,
                    sampling_context,
                    light_sample);
            }
        }

        void trace_emitting_shape_photon(
            const ShadingContext&   shading_context,
            SamplingContext&        sampling_context,
            LightSample&            light_sample)
        {
            // Make sure the geometric normal of the light sample is in the same hemisphere as the shading normal.
            light_sample.m_geometric_normal =
                flip_to_same_hemisphere(
                    light_sample.m_geometric_normal,
                    light_sample.m_shading_normal);

            const Material* material = light_sample.m_shape->get_material();
            const Material::RenderData& material_data = material->get_render_data();
            const EDF* edf = material_data.m_edf;

            // Build a shading point on the light source.
            ShadingPoint light_shading_point;
            light_sample.make_shading_point(
                light_shading_point,
                light_sample.m_shading_normal,
                shading_context.get_intersector());

            if (material_data.m_shader_group)
            {
                shading_context.execute_osl_emission(
                    *material_data.m_shader_group,
                    light_shading_point);
            }

            // Sample the EDF.
            sampling_context.split_in_place(2, 1);
            Vector3f emission_direction;
            Spectrum edf_value(Spectrum::Illuminance);
            float edf_prob;
            edf->sample(
                sampling_context,
                edf->evaluate_inputs(shading_context, light_shading_point),
                Vector3f(light_sample.m_geometric_normal),
                Basis3f(Vector3f(light_sample.m_shading_normal)),
                sampling_context.next2<Vector2f>(),
                emission_direction,
                edf_value,
                edf_prob);

            // Compute the initial particle weight.
            Spectrum initial_flux = edf_value;
            initial_flux *=
                dot(emission_direction, Vector3f(light_sample.m_shading_normal)) /
                (light_sample.m_probability * edf_prob * m_params.m_light_photon_count);

            // Make a shading point that will be used to avoid self-intersections with the light sample.
            ShadingPoint parent_shading_point;
            light_sample.make_shading_point(
                parent_shading_point,
                Vector3d(emission_direction),
                m_intersector);

            // Build the photon ray.
            sampling_context.split_in_place(1, 1);
            const ShadingRay ray(
                light_sample.m_point,
                Vector3d(emission_direction),
                ShadingRay::Time::create_with_normalized_time(
                    sampling_context.next2<float>(),
                    m_shutter_open_begin_time,
                    m_shutter_close_end_time),
                VisibilityFlags::LightRay,
                0);

            // Build the path tracer.
            const bool cast_indirect_light = (edf->get_flags() & EDF::CastIndirectLight) != 0;
            PathVisitor path_visitor(
                initial_flux,
                m_params,
                m_importon_map,
                m_importon_lookup_radius,
                m_params.m_dl_mode == SPPMParameters::SPPM, // store direct lighting photons?
                cast_indirect_light,
                m_params.m_enable_caustics,
                m_local_photons);
            VolumeVisitor volume_visitor;
            PathTracer<PathVisitor, VolumeVisitor, true> path_tracer(      // true = adjoint
                path_visitor,
                volume_visitor,
                m_params.m_photon_tracing_rr_min_path_length,
                m_params.m_photon_tracing_max_bounces,
                ~size_t(0), // max diffuse bounces
                ~size_t(0), // max glossy bounces
                ~size_t(0), // max specular bounces
                ~size_t(0), // max volume bounces
                false,      // don't clamp roughness
                m_params.m_max_iterations,
                edf->get_light_near_start());               // don't illuminate points closer than the light near start value

            // Trace the photon path.
            path_tracer.trace(sampling_context, shading_context, ray, &parent_shading_point);
        }

        void trace_non_physical_light_photon(
            const ShadingContext&   shading_context,
            SamplingContext&        sampling_context,
            const LightSample&      light_sample)
        {
            // Sample the light.
            sampling_context.split_in_place(2, 1);
            Vector3d emission_position, emission_direction;
            Spectrum light_value(Spectrum::Illuminance);
            float light_prob;
            light_sample.m_light->sample(
                shading_context,
                light_sample.m_light_transform,
                sampling_context.next2<Vector2d>(),
                m_photon_targets,
                emission_position,
                emission_direction,
                light_value,
                light_prob);

            // Compute the initial particle weight.
            Spectrum initial_flux = light_value;
            initial_flux /= light_sample.m_probability * light_prob * m_params.m_light_photon_count;

            // Build the photon ray.
            sampling_context.split_in_place(1, 1);
            const ShadingRay ray(
                emission_position,
                emission_direction,
                ShadingRay::Time::create_with_normalized_time(
                    sampling_context.next2<float>(),
                    m_shutter_open_begin_time,
                    m_shutter_close_end_time),
                VisibilityFlags::LightRay,
                0);

            // Build the path tracer.
            const bool cast_indirect_light = (light_sample.m_light->get_flags() & EDF::CastIndirectLight) != 0;
            PathVisitor path_visitor(
                initial_flux,
                m_params,
                m_importon_map,
                m_importon_lookup_radius,
                m_params.m_dl_mode == SPPMParameters::SPPM, // store direct lighting photons?
                cast_indirect_light,
                m_params.m_enable_caustics,
                m_local_photons);
            VolumeVisitor volume_visitor;
            PathTracer<PathVisitor, VolumeVisitor, true> path_tracer(      // true = adjoint
                path_visitor,
                volume_visitor,
                m_params.m_photon_tracing_rr_min_path_length,
                m_params.m_photon_tracing_max_bounces,
                ~size_t(0), // max diffuse bounces
                ~size_t(0), // max glossy bounces
                ~size_t(0), // max specular bounces
                ~size_t(0), // max volume bounces
                false,      // don't clamp roughness
                m_params.m_max_iterations);

            // Trace the photon path.
            path_tracer.trace(sampling_context, shading_context, ray);
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
            const Scene&                scene,
            const LightTargetArray&     photon_targets,
            const SPPMImportonMap*      importon_map,
            const float                 importon_lookup_radius,
            const TraceContext&         trace_context,
            TextureStore&               texture_store,
            OIIOTextureSystem&          oiio_texture_system,
            OSLShadingSystem&           shading_system,
            const SPPMParameters&       params,
            SPPMPhotonVector&           global_photons,
            const size_t                photon_begin,
            const size_t                photon_end,
            const std::uint32_t         pass_hash,
            IAbortSwitch&               abort_switch)
          : m_scene(scene)
          , m_photon_targets(photon_targets)
          , m_importon_map(importon_map)
          , m_importon_lookup_radius(importon_lookup_radius)
          , m_env_edf(*scene.get_environment()->get_environment_edf())
          , m_texture_cache(texture_store)
          , m_intersector(trace_context, m_texture_cache)
          , m_oiio_texture_system(oiio_texture_system)
          , m_shadergroup_exec(shading_system, m_arena)
          , m_params(params)
          , m_tracer(
                m_scene,
                m_intersector,
                m_shadergroup_exec,
                m_params.m_transparency_threshold,
                m_params.m_max_iterations,
                false)
          , m_global_photons(global_photons)
          , m_photon_begin(photon_begin)
          , m_photon_end(photon_end)
          , m_pass_hash(pass_hash)
          , m_abort_switch(abort_switch)
        {
            const Scene::RenderData& scene_data = m_scene.get_render_data();
            m_scene_center = Vector3d(scene_data.m_center);
            m_scene_radius = scene_data.m_radius;
            m_safe_scene_diameter = scene_data.m_safe_diameter;

            const Camera* camera = scene.get_render_data().m_active_camera;
            m_shutter_open_begin_time = camera->get_shutter_open_begin_time();
            m_shutter_close_end_time = camera->get_shutter_close_end_time();
        }

        void execute(const size_t thread_index) override
        {
            // Initialize thread-local variables.
            Spectrum::set_mode(m_params.m_spectrum_mode);

            const ShadingContext shading_context(
                m_intersector,
                m_tracer,
                m_texture_cache,
                m_oiio_texture_system,
                m_shadergroup_exec,
                m_arena,
                thread_index);

            const size_t instance = hash_uint32(static_cast<std::uint32_t>(m_pass_hash + m_photon_begin));
            SamplingContext::RNGType rng(m_pass_hash, instance);
            SamplingContext sampling_context(
                rng,
                m_params.m_sampling_mode,
                2,                          // number of dimensions
                0,                          // number of samples -- unknown
                instance);                  // initial instance number

            for (size_t i = m_photon_begin; i < m_photon_end && !m_abort_switch.is_aborted(); ++i)
            {
                m_arena.clear();

                // Generate a uniform sample in [0,1)^2.
                const Vector2f env_edf_s = sampling_context.next2<Vector2f>();

                // Trace a photon.
                SamplingContext child_sampling_context(sampling_context);
                trace_env_photon(shading_context, child_sampling_context, env_edf_s);
            }

            m_global_photons.append(m_local_photons);
        }

      private:
        const Scene&                m_scene;
        const LightTargetArray&     m_photon_targets;
        const SPPMImportonMap*      m_importon_map;
        const float                 m_importon_lookup_radius;
        const EnvironmentEDF&       m_env_edf;
        TextureCache                m_texture_cache;
        Intersector                 m_intersector;
        OIIOTextureSystem&          m_oiio_texture_system;
        Arena                       m_arena;
        OSLShaderGroupExec          m_shadergroup_exec;
        const SPPMParameters        m_params;
        Tracer                      m_tracer;
        SPPMPhotonVector&           m_global_photons;
        const size_t                m_photon_begin;
        const size_t                m_photon_end;
        const std::uint32_t         m_pass_hash;
        IAbortSwitch&               m_abort_switch;
        SPPMPhotonVector            m_local_photons;
        float                       m_shutter_open_begin_time;
        float                       m_shutter_close_end_time;

        Vector3d                    m_scene_center;         // world space
        double                      m_scene_radius;         // world space
        double                      m_safe_scene_diameter;  // world space

        void trace_env_photon(
            const ShadingContext&   shading_context,
            SamplingContext&        sampling_context,
            const Vector2f&         env_edf_s)
        {
            // Sample the environment.
            Vector3f outgoing;
            Spectrum env_edf_value(Spectrum::Illuminance);
            float env_edf_prob;
            m_env_edf.sample(
                shading_context,
                env_edf_s,
                outgoing,   // points toward the environment
                env_edf_value,
                env_edf_prob);

            // Generate a uniform sample in [0,1)^2.
            sampling_context.split_in_place(2, 1);
            Vector2d s = sampling_context.next2<Vector2d>();

            // Compute the center and radius of the target disk.
            Vector3d disk_center;
            double disk_radius;
            const size_t target_count = m_photon_targets.size();
            if (target_count > 0)
            {
                const double x = s[0] * target_count;
                const size_t target_index = truncate<size_t>(x);
                s[0] = x - target_index;

                const LightTarget& target = m_photon_targets[target_index];
                disk_center = target.get_center();
                disk_radius = target.get_radius();
            }
            else
            {
                disk_center = m_scene_center;
                disk_radius = m_scene_radius;
            }

            // Compute the origin of the photon ray.
            const Basis3d basis(-Vector3d(outgoing));
            const Vector2d p = sample_disk_uniform(s);
            const Vector3d ray_origin =
                  disk_center
                - m_safe_scene_diameter * basis.get_normal()
                + disk_radius * p[0] * basis.get_tangent_u() +
                + disk_radius * p[1] * basis.get_tangent_v();
            const float disk_point_prob = 1.0f / (Pi<float>() * square(static_cast<float>(disk_radius)));

            // Compute the initial particle weight.
            Spectrum initial_flux = env_edf_value;
            initial_flux /= disk_point_prob * env_edf_prob * m_params.m_env_photon_count;

            // Build the photon ray.
            sampling_context.split_in_place(1, 1);
            const ShadingRay ray(
                ray_origin,
                -Vector3d(outgoing),
                ShadingRay::Time::create_with_normalized_time(
                    sampling_context.next2<float>(),
                    m_shutter_open_begin_time,
                    m_shutter_close_end_time),
                VisibilityFlags::LightRay,
                0);

            // Build the path tracer.
            const bool cast_indirect_light = true;  // right now environments always cast indirect light
            PathVisitor path_visitor(
                initial_flux,
                m_params,
                m_importon_map,
                m_importon_lookup_radius,
                true,
                cast_indirect_light,
                m_params.m_enable_caustics,
                m_local_photons);
            VolumeVisitor volume_visitor;
            PathTracer<PathVisitor, VolumeVisitor, true> path_tracer(   // true = adjoint
                path_visitor,
                volume_visitor,
                m_params.m_photon_tracing_rr_min_path_length,
                m_params.m_photon_tracing_max_bounces,
                ~size_t(0), // max diffuse bounces
                ~size_t(0), // max glossy bounces
                ~size_t(0), // max specular bounces
                ~size_t(0), // max volume bounces
                false,      // don't clamp roughness
                m_params.m_max_iterations);

            // Trace the photon path.
            path_tracer.trace(sampling_context, shading_context, ray);
        }
    };
}


//
// SPPMPhotonTracer class implementation.
//

SPPMPhotonTracer::SPPMPhotonTracer(
    const Scene&                scene,
    const ForwardLightSampler&  light_sampler,
    const TraceContext&         trace_context,
    TextureStore&               texture_store,
    OIIOTextureSystem&          oiio_texture_system,
    OSLShadingSystem&           shading_system,
    const SPPMParameters&       params)
  : m_params(params)
  , m_scene(scene)
  , m_light_sampler(light_sampler)
  , m_trace_context(trace_context)
  , m_texture_store(texture_store)
  , m_total_emitted_photon_count(0)
  , m_total_stored_photon_count(0)
  , m_oiio_texture_system(oiio_texture_system)
  , m_shading_system(shading_system)
{
}

namespace
{
    void collect_photon_targets(
        const Assembly&                     assembly,
        const Transformd&                   assembly_inst_transform,
        LightTargetArray&                   photon_targets)
    {
        for (const_each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
        {
            const ObjectInstance& object_instance = *i;

            if (object_instance.get_parameters().get_optional<bool>("photon_target", false))
            {
                const Transformd object_inst_transform =
                    object_instance.get_transform() * assembly_inst_transform;

                const LightTarget target(
                    object_inst_transform.to_parent(
                        object_instance.get_object().compute_local_bbox()));

                photon_targets.push_back(target);
            }
        }
    }

    void collect_photon_targets(
        const AssemblyInstanceContainer&    assembly_instances,
        const Transformd&                   parent_transform,
        LightTargetArray&                   photon_targets)
    {
        for (const_each<AssemblyInstanceContainer> i = assembly_instances; i; ++i)
        {
            // Retrieve the assembly instance.
            const AssemblyInstance& assembly_instance = *i;

            // Retrieve the assembly.
            const Assembly& assembly = assembly_instance.get_assembly();

            // Compute the cumulated transform sequence of this assembly instance.
            // todo: consider the photon targets throughout the entire time interval.
            const Transformd cumulated_transform =
                assembly_instance.transform_sequence().get_earliest_transform() * parent_transform;

            // Recurse into child assembly instances.
            collect_photon_targets(
                assembly.assembly_instances(),
                cumulated_transform,
                photon_targets);

            // Collect the photon targets of this assembly instance.
            collect_photon_targets(
                assembly,
                cumulated_transform,
                photon_targets);
        }
    }
}

void SPPMPhotonTracer::trace_photons(
    SPPMPhotonVector&       photons,
    const SPPMImportonMap*  importon_map,
    const float             importon_lookup_radius,
    const std::uint32_t     pass_hash,
    JobQueue&               job_queue,
    IAbortSwitch&           abort_switch)
{
    // Start stopwatch.
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    // Collect photon targets.
    LightTargetArray photon_targets;
    collect_photon_targets(
        m_scene.assembly_instances(),
        Transformd::identity(),
        photon_targets);

    // Schedule photon tracing jobs.
    size_t job_count = 0;
    size_t emitted_photon_count = 0;
    if (m_light_sampler.has_lights())
    {
        schedule_light_photon_tracing_jobs(
            photon_targets,
            importon_map,
            importon_lookup_radius,
            pass_hash,
            photons,
            job_queue,
            job_count,
            emitted_photon_count,
            abort_switch);
    }
    if (m_params.m_enable_ibl && m_scene.get_environment()->get_environment_edf())
    {
        schedule_environment_photon_tracing_jobs(
            photon_targets,
            importon_map,
            importon_lookup_radius,
            pass_hash,
            photons,
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
    statistics.insert("emitted", emitted_photon_count);
    statistics.insert(
        "stored",
        pretty_uint(photons.size()) +
        " (" + pretty_percent(photons.size(), emitted_photon_count) + ")");
    statistics.insert("total emitted", m_total_emitted_photon_count);
    statistics.insert(
        "total stored",
        pretty_uint(m_total_stored_photon_count) +
        " (" + pretty_percent(m_total_stored_photon_count, m_total_emitted_photon_count) + ")");
    RENDERER_LOG_DEBUG("%s",
        StatisticsVector::make(
            "sppm photon tracing statistics",
            statistics).to_string().c_str());
}

void SPPMPhotonTracer::schedule_light_photon_tracing_jobs(
    const LightTargetArray& photon_targets,
    const SPPMImportonMap*  importon_map,
    const float             importon_lookup_radius,
    const std::uint32_t     pass_hash,
    SPPMPhotonVector&       photons,
    JobQueue&               job_queue,
    size_t&                 job_count,
    size_t&                 emitted_photon_count,
    IAbortSwitch&           abort_switch)
{
    RENDERER_LOG_INFO(
        "tracing %s sppm light %s...",
        pretty_uint(m_params.m_light_photon_count).c_str(),
        m_params.m_light_photon_count > 1 ? "photons" : "photon");

    for (size_t i = 0; i < m_params.m_light_photon_count; i += m_params.m_photon_packet_size)
    {
        const size_t photon_begin = i;
        const size_t photon_end = std::min(i + m_params.m_photon_packet_size, m_params.m_light_photon_count);

        job_queue.schedule(
            new LightPhotonTracingJob(
                m_scene,
                photon_targets,
                importon_map,
                importon_lookup_radius,
                m_light_sampler,
                m_trace_context,
                m_texture_store,
                m_oiio_texture_system,
                m_shading_system,
                m_params,
                photons,
                photon_begin,
                photon_end,
                pass_hash,
                abort_switch));

        ++job_count;
        emitted_photon_count += photon_end - photon_begin;
    }
}

void SPPMPhotonTracer::schedule_environment_photon_tracing_jobs(
    const LightTargetArray& photon_targets,
    const SPPMImportonMap*  importon_map,
    const float             importon_lookup_radius,
    const std::uint32_t     pass_hash,
    SPPMPhotonVector&       photons,
    JobQueue&               job_queue,
    size_t&                 job_count,
    size_t&                 emitted_photon_count,
    IAbortSwitch&           abort_switch)
{
    RENDERER_LOG_INFO(
        "tracing %s sppm environment %s...",
        pretty_uint(m_params.m_env_photon_count).c_str(),
        m_params.m_env_photon_count > 1 ? "photons" : "photon");

    for (size_t i = 0; i < m_params.m_env_photon_count; i += m_params.m_photon_packet_size)
    {
        const size_t photon_begin = i;
        const size_t photon_end = std::min(i + m_params.m_photon_packet_size, m_params.m_env_photon_count);

        job_queue.schedule(
            new EnvironmentPhotonTracingJob(
                m_scene,
                photon_targets,
                importon_map,
                importon_lookup_radius,
                m_trace_context,
                m_texture_store,
                m_oiio_texture_system,
                m_shading_system,
                m_params,
                photons,
                photon_begin,
                photon_end,
                pass_hash,
                abort_switch));

        ++job_count;
        emitted_photon_count += photon_end - photon_begin;
    }
}

}   // namespace renderer
