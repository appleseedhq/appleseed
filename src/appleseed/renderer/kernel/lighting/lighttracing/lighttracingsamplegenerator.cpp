
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
#include "lighttracingsamplegenerator.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/rendering/globalsampleaccumulationbuffer.h"
#include "renderer/kernel/rendering/sample.h"
#include "renderer/kernel/rendering/sampleaccumulationbuffer.h"
#include "renderer/kernel/rendering/samplegeneratorbase.h"
#ifdef APPLESEED_WITH_OSL
#include "renderer/kernel/shading/oslshadergroupexec.h"
#endif
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/population.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace foundation    { class LightingConditions; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // LightTracingSampleGenerator class implementation.
    //
    // References:
    //
    //   Monte Carlo Light Tracing With Direct Computation Of Pixel Intensities
    //   http://graphics.cs.kuleuven.be/publications/MCLTWDCOPI/
    //
    //   Robust Monte Carlo Methods For Light Transport Simulation
    //   http://graphics.stanford.edu/papers/veach_thesis/thesis.pdf
    //

    class LightTracingSampleGenerator
      : public SampleGeneratorBase
    {
      public:
        struct Parameters
        {
            const bool      m_enable_ibl;                   // is image-based lighting enabled?
            const bool      m_enable_caustics;              // are caustics enabled?

            const float     m_transparency_threshold;
            const size_t    m_max_iterations;
            const bool      m_report_self_intersections;

            const size_t    m_max_path_length;              // maximum path length, ~0 for unlimited
            const size_t    m_rr_min_path_length;           // minimum path length before Russian Roulette kicks in, ~0 for unlimited

            explicit Parameters(const ParamArray& params)
              : m_enable_ibl(params.get_optional<bool>("enable_ibl", true))
              , m_enable_caustics(params.get_optional<bool>("enable_caustics", true))
              , m_transparency_threshold(params.get_optional<float>("transparency_threshold", 0.001f))
              , m_max_iterations(params.get_optional<size_t>("max_iterations", 1000))
              , m_report_self_intersections(params.get_optional<bool>("report_self_intersections", false))
              , m_max_path_length(nz(params.get_optional<size_t>("max_path_length", 0)))
              , m_rr_min_path_length(nz(params.get_optional<size_t>("rr_min_path_length", 3)))
            {
            }

            static size_t nz(const size_t x)
            {
                return x == 0 ? ~0 : x;
            }

            void print() const
            {
                RENDERER_LOG_INFO(
                    "light tracing settings:\n"
                    "  ibl              %s\n"
                    "  caustics         %s\n"
                    "  max path length  %s\n"
                    "  rr min path len. %s",
                    m_enable_ibl ? "on" : "off",
                    m_enable_caustics ? "on" : "off",
                    m_max_path_length == ~0 ? "infinite" : pretty_uint(m_max_path_length).c_str(),
                    m_rr_min_path_length == ~0 ? "infinite" : pretty_uint(m_rr_min_path_length).c_str());
            }
        };

        LightTracingSampleGenerator(
            const Scene&                scene,
            const Frame&                frame,
            const TraceContext&         trace_context,
            TextureStore&               texture_store,
            const LightSampler&         light_sampler,
            const size_t                generator_index,
            const size_t                generator_count,
#ifdef APPLESEED_WITH_OIIO
            OIIO::TextureSystem&        oiio_texture_system,
#endif
#ifdef APPLESEED_WITH_OSL
            OSL::ShadingSystem&         shading_system,
#endif
            const ParamArray&           params)
          : SampleGeneratorBase(generator_index, generator_count)
          , m_params(params)
          , m_scene(scene)
          , m_frame(frame)
          , m_safe_scene_radius(scene.compute_radius() * (1.0 + 1.0e-3))
          , m_disk_point_prob(1.0 / (Pi * square(m_safe_scene_radius)))
          , m_light_sampler(light_sampler)
          , m_texture_cache(texture_store)
          , m_intersector(trace_context, m_texture_cache, m_params.m_report_self_intersections)
#ifdef APPLESEED_WITH_OSL
          , m_shadergroup_exec(shading_system)
#endif
          , m_tracer(
                m_scene,
                m_intersector,
                m_texture_cache,
#ifdef APPLESEED_WITH_OSL
                m_shadergroup_exec,
#endif
                m_params.m_transparency_threshold,
                m_params.m_max_iterations)
          , m_shading_context(
                m_intersector,
                m_tracer,
                m_texture_cache,
#ifdef APPLESEED_WITH_OIIO
                oiio_texture_system,
#endif
#ifdef APPLESEED_WITH_OSL
                m_shadergroup_exec,
#endif
                generator_index,
                0,
                m_params.m_transparency_threshold,
                m_params.m_max_iterations)
          , m_light_sample_count(0)
          , m_path_count(0)
        {
            m_ray_dtime = scene.get_camera()->get_shutter_open_time_interval();
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual void reset() APPLESEED_OVERRIDE
        {
            SampleGeneratorBase::reset();
            m_rng = MersenneTwister();
        }

        virtual void generate_samples(
            const size_t                sample_count,
            SampleAccumulationBuffer&   buffer,
            AbortSwitch&                abort_switch) APPLESEED_OVERRIDE
        {
            m_light_sample_count = 0;

            SampleGeneratorBase::generate_samples(sample_count, buffer, abort_switch);

            static_cast<GlobalSampleAccumulationBuffer&>(buffer)
                .increment_sample_count(m_light_sample_count);
        }

        virtual StatisticsVector get_statistics() const APPLESEED_OVERRIDE
        {
            Statistics stats;
            stats.insert("path count", m_path_count);
            stats.insert("path length", m_path_length);

            return StatisticsVector::make("light tracing statistics", stats);
        }

      private:
        struct PathVisitor
        {
            const Parameters&               m_params;
            const Camera&                   m_camera;
            const Frame&                    m_frame;
            const LightingConditions&       m_lighting_conditions;
            const ShadingContext&           m_shading_context;

            const Spectrum                  m_initial_flux;         // initial particle flux (in W)
            Vector3d                        m_camera_position;      // camera position in world space
            SampleVector&                   m_samples;
            size_t                          m_sample_count;         // the number of samples added to m_samples

            PathVisitor(
                const Parameters&           params,
                const Scene&                scene,
                const Frame&                frame,
                const ShadingContext&       shading_context,
                SampleVector&               samples,
                const Spectrum&             initial_flux)
              : m_params(params)
              , m_camera(*scene.get_camera())
              , m_frame(frame)
              , m_lighting_conditions(frame.get_lighting_conditions())
              , m_shading_context(shading_context)
              , m_samples(samples)
              , m_sample_count(0)
              , m_initial_flux(initial_flux)
            {
                // Compute the world space position of the camera.
                // todo: add support for camera motion blur.
                // todo: do this outside the performance-sensitive code path.
                m_camera_position = m_camera.transform_sequence().evaluate(0.0).point_to_parent(Vector3d(0.0));
            }

            size_t get_sample_count() const
            {
                return m_sample_count;
            }

            bool accept_scattering(
                const BSDF::Mode            prev_bsdf_mode,
                const BSDF::Mode            bsdf_mode) const
            {
                assert(bsdf_mode != BSDF::Absorption);

                if (!m_params.m_enable_caustics)
                {
                    // Don't follow paths leading to caustics.
                    if (BSDF::has_glossy_or_specular(bsdf_mode))
                        return false;
                }

                return true;
            }

            void visit_area_light_vertex(
                const LightSample&          light_sample,
                const Spectrum&             light_particle_flux,
                const double                time)
            {
                // Compute the vertex-to-camera direction vector.
                const Vector3d vertex_to_camera = m_camera_position - light_sample.m_point;

                // Reject vertices on the back side of the area light.
                double cos_alpha = dot(vertex_to_camera, light_sample.m_shading_normal);
                if (cos_alpha <= 0.0)
                    return;

                // Compute the transmission factor between the vertex and the camera.
                Vector2d sample_position;
                const double transmission =
                    vertex_visible_to_camera(
                        light_sample.m_point,
                        time,
                        0,
                        sample_position);

                // Ignore occluded vertices.
                if (transmission == 0.0)
                    return;

                // Adjust cos(alpha).
                const double square_distance = square_norm(vertex_to_camera);
                const double distance = sqrt(square_distance);
                cos_alpha /= distance;

                // Compute the solid angle sustained by the pixel.
                const double solid_angle = m_camera.get_pixel_solid_angle(m_frame, sample_position);

                // Store the contribution of this vertex.
                Spectrum radiance = light_particle_flux;
                radiance *= static_cast<float>(transmission * cos_alpha / (square_distance * solid_angle));
                emit_sample(sample_position, distance, radiance);
            }

            void visit_non_physical_light_vertex(
                const Vector3d&             light_vertex,
                const Spectrum&             light_particle_flux,
                const double                time)
            {
                // Compute the transmission factor between the vertex and the camera.
                Vector2d sample_position;
                const double transmission =
                    vertex_visible_to_camera(
                        light_vertex,
                        time,
                        0,
                        sample_position);

                // Ignore occluded vertices.
                if (transmission == 0.0)
                    return;

                // Compute the square distance from the camera to the vertex.
                const double square_distance = square_norm(m_camera_position - light_vertex);
                const double distance = sqrt(square_distance);

                // Compute the solid angle sustained by the pixel.
                const double solid_angle = m_camera.get_pixel_solid_angle(m_frame, sample_position);

                // Store the contribution of this vertex.
                Spectrum radiance = light_particle_flux;
                radiance *= static_cast<float>(transmission / (square_distance * solid_angle));
                emit_sample(sample_position, distance, radiance);
            }

            void visit_vertex(const PathVertex& vertex)
            {
                // Don't process this vertex if there is no BSDF.
                if (vertex.m_bsdf == 0)
                    return;

                // Start computing the vertex-to-camera direction vector.
                Vector3d vertex_to_camera = m_camera_position - vertex.get_point();

                // Reject vertices on the back side of the shading surface.
                const Vector3d& shading_normal = vertex.get_shading_normal();
                if (dot(vertex_to_camera, shading_normal) <= 0.0)
                    return;

                // Compute the transmission factor between the vertex and the camera.
                Vector2d sample_position;
                const double transmission =
                    vertex_visible_to_camera(
                        vertex.get_point(),
                        vertex.get_time(),
                        static_cast<ShadingRay::DepthType>(vertex.m_path_length),   // ray depth = (path length - 1) + 1
                        sample_position);

                // Ignore occluded vertices.
                if (transmission == 0.0)
                    return;

                // Normalize the vertex-to-camera vector.
                const double square_distance = square_norm(vertex_to_camera);
                const double distance = sqrt(square_distance);
                vertex_to_camera /= distance;

                // Retrieve the geometric normal at the vertex.
                const Vector3d geometric_normal =
                    flip_to_same_hemisphere(
                        vertex.get_geometric_normal(),
                        shading_normal);

                // Evaluate the BSDF at the vertex position.
                Spectrum bsdf_value;
                const double bsdf_prob =
                    vertex.m_bsdf->evaluate(
                        vertex.m_bsdf_data,
                        true,                           // adjoint
                        true,                           // multiply by |cos(incoming, normal)|
                        geometric_normal,
                        vertex.get_shading_basis(),
                        vertex.m_outgoing,              // outgoing (toward the light in this context)
                        vertex_to_camera,               // incoming
                        BSDF::AllScatteringModes,       // todo: likely incorrect
                        bsdf_value);
                if (bsdf_prob == 0.0)
                    return;

                // Compute the solid angle sustained by the pixel.
                const double solid_angle = m_camera.get_pixel_solid_angle(m_frame, sample_position);

                // Store the contribution of this vertex.
                Spectrum radiance = m_initial_flux;
                radiance *= vertex.m_throughput;
                radiance *= bsdf_value;
                radiance *= static_cast<float>(transmission / (square_distance * solid_angle));
                emit_sample(sample_position, distance, radiance);
            }

            double vertex_visible_to_camera(
                const Vector3d&             vertex_position,
                const double                time,
                const ShadingRay::DepthType ray_depth,
                Vector2d&                   sample_position) const
            {
                // Compute the position of the vertex on the image plane.
                if (!m_camera.project_point(time, vertex_position, sample_position))
                    return 0.0;

                // Reject vertices that don't belong on the image plane of the camera.
                if (sample_position[0] < 0.0 || sample_position[0] >= 1.0 ||
                    sample_position[1] < 0.0 || sample_position[1] >= 1.0)
                    return 0.0;

                // Compute and return the transmission factor between the vertex and the camera.
                // Prevent self-intersections by letting the ray originate from the camera.
                return
                    m_shading_context.get_tracer().trace_between(
                        m_camera_position,
                        vertex_position,
                        time,
                        ShadingRay::CameraRay,
                        ray_depth);
            }

            void emit_sample(
                const Vector2d&             position_ndc,
                const double                distance,
                const Spectrum&             radiance)
            {
                assert(min_value(radiance) >= 0.0f);

                const Color3f linear_rgb =
                    radiance.is_rgb()
                        ? radiance.rgb()
                        : radiance.convert_to_rgb(m_lighting_conditions);

                Sample sample;
                sample.m_position = Vector2f(position_ndc);
                sample.m_values[0] = linear_rgb.r;
                sample.m_values[1] = linear_rgb.g;
                sample.m_values[2] = linear_rgb.b;
                sample.m_values[3] = 1.0f;
                sample.m_values[4] = static_cast<float>(distance);
                m_samples.push_back(sample);

                ++m_sample_count;
            }

            void visit_environment(const PathVertex& vertex)
            {
                // The particle escapes.
            }
        };

        typedef PathTracer<PathVisitor, true> PathTracerType;   // true = adjoint

        const Parameters                m_params;

        const Scene&                    m_scene;
        const Frame&                    m_frame;

        // Preserve order.
        const double                    m_safe_scene_radius;    // radius of the scene's bounding sphere + small safety margin
        const double                    m_disk_point_prob;

        const LightSampler&             m_light_sampler;
        TextureCache                    m_texture_cache;
        Intersector                     m_intersector;
#ifdef APPLESEED_WITH_OSL
        OSLShaderGroupExec              m_shadergroup_exec;
#endif
        Tracer                          m_tracer;
        const ShadingContext            m_shading_context;

        MersenneTwister                 m_rng;

        uint64                          m_light_sample_count;

        uint64                          m_path_count;
        Population<uint64>              m_path_length;

        double                          m_ray_dtime;

        virtual size_t generate_samples(
            const size_t                sequence_index,
            SampleVector&               samples) APPLESEED_OVERRIDE
        {
            SamplingContext sampling_context(
                m_rng,
                0,
                sequence_index,
                sequence_index);

            size_t stored_sample_count = 0;

            if (m_light_sampler.has_lights_or_emitting_triangles())
                stored_sample_count += generate_light_sample(sampling_context, samples);

            if (m_params.m_enable_ibl)
            {
                const EnvironmentEDF* env_edf = m_scene.get_environment()->get_environment_edf();

                if (env_edf)
                {
                    stored_sample_count +=
                        generate_environment_sample(sampling_context, env_edf, samples);
                }
            }

            ++m_light_sample_count;

            return stored_sample_count;
        }

        size_t generate_light_sample(
            SamplingContext&            sampling_context,
            SampleVector&               samples)
        {
            // Sample the light sources.
            LightSample light_sample;
            sampling_context.split_in_place(4, 1);
            m_light_sampler.sample(sampling_context.next_vector2<4>(), light_sample);

            return
                light_sample.m_triangle
                    ? generate_emitting_triangle_sample(sampling_context, light_sample, samples)
                    : generate_non_physical_light_sample(sampling_context, light_sample, samples);
        }

        size_t generate_emitting_triangle_sample(
            SamplingContext&            sampling_context,
            LightSample&                light_sample,
            SampleVector&               samples)
        {
            // Make sure the geometric normal of the light sample is in the same hemisphere as the shading normal.
            light_sample.m_geometric_normal =
                flip_to_same_hemisphere(
                    light_sample.m_geometric_normal,
                    light_sample.m_shading_normal);

            const Material* material = light_sample.m_triangle->m_material;
            const EDF* edf = material->get_edf();

            // Evaluate the EDF inputs.
            InputEvaluator input_evaluator(m_texture_cache);

            // TODO: refactor this code (est.).
            ShadingPoint shading_point;
            light_sample.make_shading_point(
                shading_point,
                light_sample.m_shading_normal,
                m_shading_context.get_intersector());
#ifdef APPLESEED_WITH_OSL
            if (const ShaderGroup* sg = material->get_osl_surface())
            {
                // TODO: get object area somehow.
                const float surface_area = 0.0f;
                m_shading_context.execute_osl_emission(*sg, shading_point, surface_area);
            }
#endif
            edf->evaluate_inputs(input_evaluator, shading_point);

            // Sample the EDF.
            sampling_context.split_in_place(2, 1);
            Vector3d emission_direction;
            Spectrum edf_value;
            double edf_prob;
            edf->sample(
                sampling_context,
                input_evaluator.data(),
                light_sample.m_geometric_normal,
                Basis3d(light_sample.m_shading_normal),
                sampling_context.next_vector2<2>(),
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

            // Build the light ray.
            sampling_context.split_in_place(1, 1);
            const ShadingRay light_ray(
                light_sample.m_point,
                emission_direction,
                sampling_context.next_double2(),
                m_ray_dtime,
                ShadingRay::LightRay);

            // Build the path tracer.
            PathVisitor path_visitor(
                m_params,
                m_scene,
                m_frame,
                m_shading_context,
                samples,
                initial_flux);
            PathTracerType path_tracer(
                path_visitor,
                m_params.m_rr_min_path_length,
                m_params.m_max_path_length,
                m_params.m_max_iterations,
                edf->get_light_near_start());               // don't illuminate points closer than the light near start value

            // Handle the light vertex separately.
            Spectrum light_particle_flux = edf_value;       // todo: only works for diffuse EDF? What we need is the light exitance
            light_particle_flux /= static_cast<float>(light_sample.m_probability);
            path_visitor.visit_area_light_vertex(
                light_sample,
                light_particle_flux,
                light_ray.m_time);

            // Trace the light path.
            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    m_shading_context,
                    light_ray,
                    &parent_shading_point);

            // Update path statistics.
            ++m_path_count;
            m_path_length.insert(path_length);

            // Return the number of samples generated when tracing this light path.
            return path_visitor.get_sample_count();
        }

        size_t generate_non_physical_light_sample(
            SamplingContext&            sampling_context,
            const LightSample&          light_sample,
            SampleVector&               samples)
        {
            // Sample the light.
            InputEvaluator input_evaluator(m_texture_cache);
            sampling_context.split_in_place(2, 1);
            Vector3d emission_position, emission_direction;
            Spectrum light_value;
            double light_prob;
            light_sample.m_light->sample(
                input_evaluator,
                sampling_context.next_vector2<2>(),
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

            // Build the light ray.
            sampling_context.split_in_place(1, 1);
            const ShadingRay light_ray(
                emission_position,
                emission_direction,
                sampling_context.next_double2(),
                m_ray_dtime,
                ShadingRay::LightRay);

            // Build the path tracer.
            PathVisitor path_visitor(
                m_params,
                m_scene,
                m_frame,
                m_shading_context,
                samples,
                initial_flux);
            PathTracerType path_tracer(
                path_visitor,
                m_params.m_rr_min_path_length,
                m_params.m_max_path_length,
                m_params.m_max_iterations);

            // Handle the light vertex separately.
            Spectrum light_particle_flux = light_value;
            light_particle_flux /= static_cast<float>(light_sample.m_probability);
            path_visitor.visit_non_physical_light_vertex(
                emission_position,
                light_particle_flux,
                light_ray.m_time);

            // Trace the light path.
            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    m_shading_context,
                    light_ray);

            // Update path statistics.
            ++m_path_count;
            m_path_length.insert(path_length);

            // Return the number of samples generated when tracing this light path.
            return path_visitor.get_sample_count();
        }

        size_t generate_environment_sample(
            SamplingContext&            sampling_context,
            const EnvironmentEDF*       env_edf,
            SampleVector&               samples)
        {
            // Sample the environment.
            sampling_context.split_in_place(2, 1);
            InputEvaluator input_evaluator(m_texture_cache);
            Vector3d outgoing;
            Spectrum env_edf_value;
            double env_edf_prob;
            env_edf->sample(
                input_evaluator,
                sampling_context.next_vector2<2>(),
                outgoing,               // points toward the environment
                env_edf_value,
                env_edf_prob);

            // Compute the center of the tangent disk.
            const Vector3d disk_center = m_safe_scene_radius * outgoing;

            // Uniformly sample the tangent disk.
            sampling_context.split_in_place(2, 1);
            const Vector2d disk_point =
                m_safe_scene_radius *
                sample_disk_uniform(sampling_context.next_vector2<2>());

            // Compute the origin of the light ray.
            const Basis3d basis(-outgoing);
            const Vector3d ray_origin =
                disk_center +
                disk_point[0] * basis.get_tangent_u() +
                disk_point[1] * basis.get_tangent_v();

            // Compute the initial particle weight.
            Spectrum initial_flux = env_edf_value;
            initial_flux /= static_cast<float>(m_disk_point_prob * env_edf_prob);

            // Build the light ray.
            sampling_context.split_in_place(1, 1);
            const ShadingRay light_ray(
                ray_origin,
                -outgoing,
                sampling_context.next_double2(),
                m_ray_dtime,
                ShadingRay::LightRay);

            // Build the path tracer.
            PathVisitor path_visitor(
                m_params,
                m_scene,
                m_frame,
                m_shading_context,
                samples,
                initial_flux);
            PathTracerType path_tracer(
                path_visitor,
                m_params.m_rr_min_path_length,
                m_params.m_max_path_length,
                m_params.m_max_iterations);

            // Trace the light path.
            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    m_shading_context,
                    light_ray);

            // Update path statistics.
            ++m_path_count;
            m_path_length.insert(path_length);

            // Return the number of samples generated when tracing this light path.
            return path_visitor.get_sample_count();
        }
    };
}


//
// LightTracingSampleGeneratorFactory class implementation.
//

LightTracingSampleGeneratorFactory::LightTracingSampleGeneratorFactory(
    const Scene&            scene,
    const Frame&            frame,
    const TraceContext&     trace_context,
    TextureStore&           texture_store,
    const LightSampler&     light_sampler,
#ifdef APPLESEED_WITH_OIIO
    OIIO::TextureSystem&    oiio_texture_system,
#endif
#ifdef APPLESEED_WITH_OSL
    OSL::ShadingSystem&     shading_system,
#endif
    const ParamArray&       params)
  : m_scene(scene)
  , m_frame(frame)
  , m_trace_context(trace_context)
  , m_texture_store(texture_store)
  , m_light_sampler(light_sampler)
#ifdef APPLESEED_WITH_OIIO
  , m_oiio_texture_system(oiio_texture_system)
#endif
#ifdef APPLESEED_WITH_OSL
  , m_shading_system(shading_system)
#endif
  , m_params(params)
{
    LightTracingSampleGenerator::Parameters(params).print();
}

void LightTracingSampleGeneratorFactory::release()
{
    delete this;
}

ISampleGenerator* LightTracingSampleGeneratorFactory::create(
    const size_t            generator_index,
    const size_t            generator_count)
{
    return
        new LightTracingSampleGenerator(
            m_scene,
            m_frame,
            m_trace_context,
            m_texture_store,
            m_light_sampler,
            generator_index,
            generator_count,
#ifdef APPLESEED_WITH_OIIO
            m_oiio_texture_system,
#endif
#ifdef APPLESEED_WITH_OSL
            m_shading_system,
#endif
            m_params);
}

SampleAccumulationBuffer* LightTracingSampleGeneratorFactory::create_sample_accumulation_buffer()
{
    const CanvasProperties& props = m_frame.image().properties();

    return
        new GlobalSampleAccumulationBuffer(
            props.m_canvas_width,
            props.m_canvas_height,
            m_frame.get_filter());
}

}   // namespace renderer
