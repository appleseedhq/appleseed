
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/rendering/accumulationframebuffer.h"
#include "renderer/kernel/rendering/globalaccumulationframebuffer.h"
#include "renderer/kernel/rendering/sample.h"
#include "renderer/kernel/rendering/samplegeneratorbase.h"
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
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/spectrum.h"
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
            const bool      m_enable_ibl;                   // image-based lighting enabled?
            const bool      m_enable_caustics;              // caustics enabled?

            const float     m_transparency_threshold;
            const size_t    m_max_iterations;
            const bool      m_report_self_intersections;

            const size_t    m_max_path_length;              // maximum path length, 0 for unlimited
            const size_t    m_rr_min_path_length;           // minimum path length before Russian Roulette is used, 0 for unlimited

            explicit Parameters(const ParamArray& params)
              : m_enable_ibl(params.get_optional<bool>("enable_ibl", true))
              , m_enable_caustics(params.get_optional<bool>("enable_caustics", true))
              , m_transparency_threshold(params.get_optional<float>("transparency_threshold", 0.001f))
              , m_max_iterations(params.get_optional<size_t>("max_iterations", 10000))
              , m_report_self_intersections(params.get_optional<bool>("report_self_intersections", false))
              , m_rr_min_path_length(params.get_optional<size_t>("rr_min_path_length", 3))
              , m_max_path_length(params.get_optional<size_t>("max_path_length", 0))
            {
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
                    m_max_path_length == 0 ? "infinite" : pretty_uint(m_max_path_length).c_str(),
                    m_rr_min_path_length == 0 ? "infinite" : pretty_uint(m_rr_min_path_length).c_str());
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
            const ParamArray&           params)
          : SampleGeneratorBase(generator_index, generator_count)
          , m_params(params)
          , m_scene(scene)
          , m_frame(frame)
          , m_env_edf(scene.get_environment()->get_environment_edf())
          , m_safe_scene_radius(scene.compute_radius() * (1.0 + 1.0e-3))
          , m_disk_point_prob(1.0 / (Pi * square(m_safe_scene_radius)))
          , m_light_sampler(light_sampler)
          , m_texture_cache(texture_store)
          , m_intersector(trace_context, m_texture_cache, m_params.m_report_self_intersections)
          , m_tracer(m_scene, m_intersector, m_texture_cache, m_params.m_transparency_threshold, m_params.m_max_iterations)
          , m_shading_context(m_intersector, m_tracer, m_texture_cache, 0, m_params.m_transparency_threshold, m_params.m_max_iterations)
          , m_path_count(0)
        {
        }

        virtual void release() override
        {
            delete this;
        }

        virtual void reset() override
        {
            SampleGeneratorBase::reset();
            m_rng = MersenneTwister();
        }

        virtual void generate_samples(
            const size_t                sample_count,
            AccumulationFramebuffer&    framebuffer,
            AbortSwitch&                abort_switch) override
        {
            m_light_sample_count = 0;

            SampleGeneratorBase::generate_samples(sample_count, framebuffer, abort_switch);

            static_cast<GlobalAccumulationFramebuffer&>(framebuffer)
                .increment_sample_count(m_light_sample_count);
        }

        virtual StatisticsVector get_statistics() const override
        {
            Statistics stats;
            stats.insert("path count", m_path_count);
            stats.insert("path length", m_path_length);

            return StatisticsVector::make("light tracing statistics", stats);
        }

      private:
        class PathVisitor
        {
          public:
            PathVisitor(
                const Parameters&           params,
                const Scene&                scene,
                const Frame&                frame,
                const ShadingContext&       shading_context,
                SampleVector&               samples,
                const Spectrum&             initial_alpha)
              : m_params(params)
              , m_camera(*scene.get_camera())
              , m_lighting_conditions(frame.get_lighting_conditions())
              , m_shading_context(shading_context)
              , m_samples(samples)
              , m_sample_count(0)
              , m_initial_alpha(initial_alpha)
            {
                // Compute the world space position and direction of the camera.
                // todo: add support for camera motion blur.
                // todo: do this outside the performance-sensitive code path.
                m_camera_transform = m_camera.transform_sequence().evaluate(0.0);
                m_camera_position = m_camera_transform.point_to_parent(Vector3d(0.0));
                m_camera_direction = m_camera_transform.vector_to_parent(Vector3d(0.0, 0.0, -1.0));
                assert(is_normalized(m_camera_direction));

                // Compute the reciprocal of the area of a single pixel.
                const size_t pixel_count = frame.image().properties().m_pixel_count;
                const Vector2d& film_dimensions = m_camera.get_film_dimensions();
                const double film_area = film_dimensions[0] * film_dimensions[1];
                m_rcp_pixel_area = pixel_count / film_area;

                // Cache the focal length.
                m_focal_length = m_camera.get_focal_length();
            }

            size_t get_sample_count() const
            {
                return m_sample_count;
            }

            bool accept_scattering_mode(
                const BSDF::Mode            prev_bsdf_mode,
                const BSDF::Mode            bsdf_mode) const
            {
                return (bsdf_mode & (BSDF::Diffuse | BSDF::Glossy | BSDF::Specular)) != 0;
            }

            template <bool IsAreaLight>
            void visit_light_vertex(
                const LightSample&          light_sample,
                const Spectrum&             light_particle_flux,
                const double                time)
            {
                Vector2d sample_position_ndc;
                Vector3d vertex_to_camera;
                double square_distance;

                const double transmission =
                    vertex_visible_to_camera(
                        light_sample.m_point,
                        time,
                        sample_position_ndc,
                        vertex_to_camera,
                        square_distance);

                // Cull occluded samples.
                if (transmission == 0.0)
                    return;

                double cos_alpha = 0.0;
                if (IsAreaLight)
                {
                    // Area lights are one-sided.
                    cos_alpha = dot(vertex_to_camera, light_sample.m_shading_normal);
                    if (cos_alpha <= 0.0)
                        return;
                }

                // Compute the flux-to-radiance conversion factor.
                const double cos_theta = abs(dot(vertex_to_camera, m_camera_direction));
                const double rcp_cos_theta = 1.0 / cos_theta;
                const double dist_pixel_to_camera = m_focal_length * rcp_cos_theta;
                const double flux_to_radiance = square(dist_pixel_to_camera * rcp_cos_theta) * m_rcp_pixel_area;

                // Compute the geometric term.
                const double g =
                    IsAreaLight
                        ? transmission * cos_alpha * cos_theta / square_distance
                        : transmission * cos_theta / square_distance;
                assert(g >= 0.0);

                // Store the contribution of this path vertex.
                Spectrum radiance = light_particle_flux;
                radiance *= static_cast<float>(g * flux_to_radiance);
                emit_sample(sample_position_ndc, radiance);
            }

            bool visit_vertex(
                SamplingContext&            sampling_context,
                const ShadingPoint&         shading_point,
                const Vector3d&             outgoing,           // in this context, toward the light
                const BSDF*                 bsdf,
                const void*                 bsdf_data,
                const size_t                path_length,
                const BSDF::Mode            prev_bsdf_mode,
                const double                prev_bsdf_prob,
                const Spectrum&             throughput)
            {
                if (!m_params.m_enable_caustics &&
                    path_length > 1 &&
                    (prev_bsdf_mode & (BSDF::Glossy | BSDF::Specular)) != 0)
                {
                    // This is a caustics path but caustics are disabled.
                    return false;
                }

                Vector2d sample_position_ndc;
                Vector3d vertex_to_camera;
                double square_distance;

                const double transmission =
                    vertex_visible_to_camera(
                        shading_point.get_point(),
                        shading_point.get_ray().m_time,
                        sample_position_ndc,
                        vertex_to_camera,
                        square_distance);

                // Cull occluded samples.
                if (transmission == 0.0)
                    return true;            // proceed with this path

                // Retrieve the shading and geometric normals at the vertex.
                const Vector3d& shading_normal = shading_point.get_shading_normal();
                const Vector3d geometric_normal =
                    flip_to_same_hemisphere(
                        shading_point.get_geometric_normal(),
                        shading_normal);

                // Evaluate the BSDF at the vertex position.
                Spectrum bsdf_value;
                const double bsdf_prob =
                    bsdf->evaluate(
                        bsdf_data,
                        true,                               // adjoint
                        true,                               // multiply by |cos(incoming, normal)|
                        geometric_normal,
                        shading_point.get_shading_basis(),
                        outgoing,                           // outgoing
                        vertex_to_camera,                   // incoming
                        BSDF::AllScatteringModes,           // todo: likely incorrect
                        bsdf_value);
                if (bsdf_prob == 0.0)
                    return true;            // proceed with this path

                // Compute the flux-to-radiance conversion factor.
                const double cos_theta = abs(dot(vertex_to_camera, m_camera_direction));
                const double rcp_cos_theta = 1.0 / cos_theta;
                const double dist_pixel_to_camera = m_focal_length * rcp_cos_theta;
                const double flux_to_radiance = square(dist_pixel_to_camera * rcp_cos_theta) * m_rcp_pixel_area;

                // Compute the geometric term.
                // cos(vertex_to_camera, shading_normal) is already accounted for in bsdf_value.
                const double g = transmission * cos_theta / square_distance;
                assert(g >= 0.0);

                // Store the contribution of this path vertex.
                Spectrum radiance = m_initial_alpha;
                radiance *= throughput;
                radiance *= bsdf_value;
                radiance *= static_cast<float>(g * flux_to_radiance);
                emit_sample(sample_position_ndc, radiance);

                // Proceed with this path.
                return true;
            }

            void visit_environment(
                const ShadingPoint&         shading_point,
                const Vector3d&             outgoing,
                const BSDF::Mode            prev_bsdf_mode,
                const Spectrum&             throughput)
            {
                // The particle escapes.
            }

          private:
            const Parameters&               m_params;
            const Camera&                   m_camera;
            const LightingConditions&       m_lighting_conditions;
            const ShadingContext&           m_shading_context;

            const Spectrum                  m_initial_alpha;        // initial particle flux (in W)
            Transformd                      m_camera_transform;     // camera transform at selected time
            Vector3d                        m_camera_position;      // camera position in world space
            Vector3d                        m_camera_direction;     // camera direction (gaze) in world space
            double                          m_rcp_pixel_area;       // reciprocal of the area of a single pixel (in m^-2)
            double                          m_focal_length;         // camera's focal length (in m)
            SampleVector&                   m_samples;
            size_t                          m_sample_count;         // the number of samples added to m_samples

            double vertex_visible_to_camera(
                const Vector3d&             vertex_position_world,
                const double                time,
                Vector2d&                   sample_position_ndc,
                Vector3d&                   vertex_to_camera,
                double&                     square_distance) const
            {
                // Transform the vertex position to camera space.
                const Vector3d vertex_position_camera =
                    m_camera_transform.point_to_local(vertex_position_world);

                // Reject vertices behind the image plane.
                if (vertex_position_camera.z > -m_camera.get_focal_length())
                    return false;

                // Compute the position of the vertex on the image plane.
                sample_position_ndc = m_camera.project(vertex_position_camera);

                // Reject vertices that don't belong on the image plane of the camera.
                if (sample_position_ndc[0] < 0.0 || sample_position_ndc[0] >= 1.0 ||
                    sample_position_ndc[1] < 0.0 || sample_position_ndc[1] >= 1.0)
                    return false;

                // Compute the transmission factor between this vertex and the camera.
                // Prevent self-intersections by letting the ray originate from the camera.
                const double transmission =
                    m_shading_context.get_tracer().trace_between(
                        m_camera_position,
                        vertex_position_world,
                        time);

                // Compute the vertex-to-camera direction vector.
                if (transmission > 0.0)
                {
                    vertex_to_camera = m_camera_position - vertex_position_world;
                    square_distance = square_norm(vertex_to_camera);
                    vertex_to_camera /= sqrt(square_distance);
                }

                return transmission;
            }

            void emit_sample(const Vector2d& position_ndc, const Spectrum& radiance)
            {
                assert(min_value(radiance) >= 0.0f);

                Sample sample;
                sample.m_position = position_ndc;
                sample.m_color.rgb() =
                    ciexyz_to_linear_rgb(
                        spectrum_to_ciexyz<float>(m_lighting_conditions, radiance));
                sample.m_color[3] = 1.0f;
                m_samples.push_back(sample);
                ++m_sample_count;
            }
        };

        typedef PathTracer<PathVisitor, true> PathTracerType;   // true = adjoint

        const Parameters                m_params;

        const Scene&                    m_scene;
        const Frame&                    m_frame;

        const EnvironmentEDF*           m_env_edf;

        // Preserve order.
        const double                    m_safe_scene_radius;    // radius of the scene's bounding sphere + small safety margin
        const double                    m_disk_point_prob;

        const LightSampler&             m_light_sampler;
        TextureCache                    m_texture_cache;
        Intersector                     m_intersector;
        Tracer                          m_tracer;
        const ShadingContext            m_shading_context;

        MersenneTwister                 m_rng;

        uint64                          m_light_sample_count;

        uint64                          m_path_count;
        Population<size_t>              m_path_length;

        virtual size_t generate_samples(
            const size_t                sequence_index,
            SampleVector&               samples) override
        {
            SamplingContext sampling_context(
                m_rng,
                0,
                sequence_index,
                sequence_index);

            size_t stored_sample_count = 0;

            if (m_light_sampler.has_lights_or_emitting_triangles())
                stored_sample_count += generate_light_sample(sampling_context, samples);

            if (m_env_edf && m_params.m_enable_ibl)
                stored_sample_count += generate_environment_sample(sampling_context, samples);

            ++m_light_sample_count;

            return stored_sample_count;
        }

        size_t generate_light_sample(
            SamplingContext&            sampling_context,
            SampleVector&               samples)
        {
            // Sample the light sources.
            sampling_context.split_in_place(3, 1);
            LightSample light_sample;
            m_light_sampler.sample(sampling_context.next_vector2<3>(), light_sample);

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

            const EDF* edf = light_sample.m_triangle->m_edf;

            // Evaluate the EDF inputs.
            InputEvaluator input_evaluator(m_texture_cache);
            const void* edf_data =
                input_evaluator.evaluate(edf->get_inputs(), light_sample.m_bary);

            // Sample the EDF.
            sampling_context.split_in_place(2, 1);
            Vector3d emission_direction;
            Spectrum edf_value;
            double edf_prob;
            edf->sample(
                edf_data,
                light_sample.m_geometric_normal,
                Basis3d(light_sample.m_shading_normal),
                sampling_context.next_vector2<2>(),
                emission_direction,
                edf_value,
                edf_prob);

            // Compute the initial particle weight.
            Spectrum initial_alpha = edf_value;
            initial_alpha *=
                static_cast<float>(
                    dot(emission_direction, light_sample.m_shading_normal)
                        / (light_sample.m_probability * edf_prob));

            // Manufacture a shading point at the position of the light sample.
            // It will be used to avoid self-intersections.
            ShadingPoint parent_shading_point;
            m_intersector.manufacture_hit(
                parent_shading_point,
                ShadingRay(light_sample.m_point, emission_direction, 0.0, 0.0, 0.0f, ~0),
                light_sample.m_triangle->m_assembly_instance,
                light_sample.m_triangle->m_object_instance_index,
                light_sample.m_triangle->m_region_index,
                light_sample.m_triangle->m_triangle_index,
                light_sample.m_triangle->m_triangle_support_plane);

            // Build the light ray.
            sampling_context.split_in_place(1, 1);
            const ShadingRay light_ray(
                light_sample.m_point,
                emission_direction,
                sampling_context.next_double2(),
                ~0);

            // Build the path tracer.
            PathVisitor path_visitor(
                m_params,
                m_scene,
                m_frame,
                m_shading_context,
                samples,
                initial_alpha);
            PathTracerType path_tracer(
                path_visitor,
                m_params.m_rr_min_path_length,
                m_params.m_max_path_length,
                m_params.m_max_iterations);

            // Handle the light vertex separately.
            Spectrum light_particle_flux = edf_value;       // todo: only works for diffuse EDF? What we need is the light exitance
            light_particle_flux /= static_cast<float>(light_sample.m_probability);
            path_visitor.visit_light_vertex<true>(
                light_sample,
                light_particle_flux,
                light_ray.m_time);

            // Trace the light path.
            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    m_intersector,
                    m_texture_cache,
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
            // Evaluate the light inputs.
            InputEvaluator input_evaluator(m_texture_cache);
            const void* light_data =
                input_evaluator.evaluate(
                    light_sample.m_light->get_inputs(),
                    light_sample.m_bary);

            // Sample the light.
            sampling_context.split_in_place(2, 1);
            Vector3d emission_direction;
            Spectrum light_value;
            double light_prob;
            light_sample.m_light->sample(
                light_data,
                sampling_context.next_vector2<2>(),
                emission_direction,
                light_value,
                light_prob);

            // Compute the initial particle weight.
            Spectrum initial_alpha = light_value;
            initial_alpha /= static_cast<float>(light_sample.m_probability * light_prob);

            // Build the light ray.
            sampling_context.split_in_place(1, 1);
            const ShadingRay light_ray(
                light_sample.m_point,
                emission_direction,
                sampling_context.next_double2(),
                ~0);

            // Build the path tracer.
            PathVisitor path_visitor(
                m_params,
                m_scene,
                m_frame,
                m_shading_context,
                samples,
                initial_alpha);
            PathTracerType path_tracer(
                path_visitor,
                m_params.m_rr_min_path_length,
                m_params.m_max_path_length,
                m_params.m_max_iterations);

            // Handle the light vertex separately.
            Spectrum light_particle_flux = light_value;
            light_particle_flux /= static_cast<float>(light_sample.m_probability);
            path_visitor.visit_light_vertex<false>(
                light_sample,
                light_particle_flux,
                light_ray.m_time);

            // Trace the light path.
            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    m_intersector,
                    m_texture_cache,
                    light_ray);

            // Update path statistics.
            ++m_path_count;
            m_path_length.insert(path_length);

            // Return the number of samples generated when tracing this light path.
            return path_visitor.get_sample_count();
        }

        size_t generate_environment_sample(
            SamplingContext&            sampling_context,
            SampleVector&               samples)
        {
            // Sample the environment.
            sampling_context.split_in_place(2, 1);
            InputEvaluator env_edf_input_evaluator(m_texture_cache);
            Vector3d outgoing;
            Spectrum env_edf_value;
            double env_edf_prob;
            m_env_edf->sample(
                env_edf_input_evaluator,
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
            Spectrum initial_alpha = env_edf_value;
            initial_alpha /= static_cast<float>(m_disk_point_prob * env_edf_prob);

            // Build the light ray.
            const ShadingRay light_ray(ray_origin, -outgoing, 0.0f, ~0);

            // Build the path tracer.
            PathVisitor path_visitor(
                m_params,
                m_scene,
                m_frame,
                m_shading_context,
                samples,
                initial_alpha);
            PathTracerType path_tracer(
                path_visitor,
                m_params.m_rr_min_path_length,
                m_params.m_max_path_length,
                m_params.m_max_iterations);

            // Trace the light path.
            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    m_intersector,
                    m_texture_cache,
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
    const ParamArray&       params)
  : m_scene(scene)
  , m_frame(frame)
  , m_trace_context(trace_context)
  , m_texture_store(texture_store)
  , m_light_sampler(light_sampler)
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
            m_params);
}

AccumulationFramebuffer* LightTracingSampleGeneratorFactory::create_accumulation_framebuffer(
    const size_t            canvas_width,
    const size_t            canvas_height)
{
    return
        new GlobalAccumulationFramebuffer(
            canvas_width,
            canvas_height);
}

}   // namespace renderer
