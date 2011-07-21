
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

// appleseed.foundation headers.
#include "foundation/image/spectrum.h"
#include "foundation/math/population.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/memory.h"

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
        LightTracingSampleGenerator(
            const Scene&                scene,
            const Frame&                frame,
            const TraceContext&         trace_context,
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
          , m_intersector(trace_context, true, m_params.m_report_self_intersections)
          , m_texture_cache(scene, m_params.m_texture_cache_size)
        {
        }

        ~LightTracingSampleGenerator()
        {
            RENDERER_LOG_DEBUG(
                "light tracing statistics:\n"
                "  paths            %s\n"
                "  path length      avg %.1f  min %s  max %s  dev %.1f\n",
                pretty_uint(m_stats.m_path_count).c_str(),
                m_stats.m_path_length.get_avg(),
                pretty_uint(m_stats.m_path_length.get_min()).c_str(),
                pretty_uint(m_stats.m_path_length.get_max()).c_str(),
                m_stats.m_path_length.get_dev());
        }

        virtual void release()
        {
            delete this;
        }

        virtual void reset()
        {
            SampleGeneratorBase::reset();
            m_rng = MersenneTwister();
        }

        virtual void generate_samples(
            const size_t                sample_count,
            AccumulationFramebuffer&    framebuffer,
            AbortSwitch&                abort_switch)
        {
            m_light_sample_count = 0;

            SampleGeneratorBase::generate_samples(sample_count, framebuffer, abort_switch);

            static_cast<GlobalAccumulationFramebuffer&>(framebuffer)
                .increment_sample_count(m_light_sample_count);
        }

      private:
        struct Parameters
        {
            const size_t    m_texture_cache_size;           // size in bytes of the texture cache
            const bool      m_report_self_intersections;
            const size_t    m_minimum_path_length;          // minimum path length before Russian Roulette is used

            explicit Parameters(const ParamArray& params)
              : m_texture_cache_size(params.get_optional<size_t>("texture_cache_size", 16 * 1024 * 1024))
              , m_report_self_intersections(params.get_optional<bool>("report_self_intersections", false))
              , m_minimum_path_length(params.get_optional<size_t>("minimum_path_length", 3))
            {
            }
        };

        struct Statistics
        {
            uint64              m_path_count;
            Population<uint64>  m_path_length;

            Statistics()
              : m_path_count(0)
            {
            }
        };

        class PathVisitor
        {
          public:
            PathVisitor(
                const Scene&                scene,
                const Frame&                frame,
                const Intersector&          intersector,
                TextureCache&               texture_cache,
                SampleVector&               samples,
                const Spectrum&             initial_alpha)
              : m_camera(*scene.get_camera())
              , m_lighting_conditions(frame.get_lighting_conditions())
              , m_shading_context(intersector, texture_cache)
              , m_samples(samples)
              , m_sample_count(0)
              , m_initial_alpha(initial_alpha)
            {
                // Compute the world space position and direction of the camera.
                m_camera_position = m_camera.get_transform().transform_point_to_parent(Vector3d(0.0));
                m_camera_direction = m_camera.get_transform().transform_vector_to_parent(Vector3d(0.0, 0.0, -1.0));
                assert(is_normalized(m_camera_direction));

                // Compute the reciprocal of the area of a single pixel.
                const size_t pixel_count = frame.properties().m_pixel_count;
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

            void visit_light_vertex(
                SamplingContext&            sampling_context,
                const LightSample&          light_sample,
                const Spectrum&             light_particle_flux)
            {
                Vector2d sample_position_ndc;
                double transmission;
                Vector3d vertex_to_camera;
                double square_distance;

                const bool visible =
                    vertex_visible_to_camera(
                        sampling_context,    
                        light_sample.m_input_params.m_point,
                        sample_position_ndc,
                        transmission,
                        vertex_to_camera,
                        square_distance);

                if (!visible)
                    return;

                // Lights are one-sided.
                const double cos_alpha = dot(vertex_to_camera, light_sample.m_input_params.m_shading_normal);
                if (cos_alpha <= 0.0)
                    return;

                // Compute the flux-to-radiance conversion factor.
                const double cos_theta = abs(dot(vertex_to_camera, m_camera_direction));
                const double rcp_cos_theta = 1.0 / cos_theta;
                const double dist_pixel_to_camera = m_focal_length * rcp_cos_theta;
                const double flux_to_radiance = square(dist_pixel_to_camera * rcp_cos_theta) * m_rcp_pixel_area;

                // Compute the geometric term.
                const double g = transmission * cos_alpha * cos_theta / square_distance;
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
                const BSDF::Mode            bsdf_mode,
                const double                bsdf_prob,
                const Spectrum&             throughput)
            {
                Vector2d sample_position_ndc;
                double transmission;
                Vector3d vertex_to_camera;
                double square_distance;

                const bool visible =
                    vertex_visible_to_camera(
                        sampling_context,
                        shading_point.get_point(),
                        sample_position_ndc,
                        transmission,
                        vertex_to_camera,
                        square_distance);

                if (!visible)
                    return true;    // proceed with this path

                // Retrieve the shading and geometric normals at the vertex.
                const Vector3d& shading_normal = shading_point.get_shading_normal();
                const Vector3d geometric_normal =
                    flip_to_same_hemisphere(
                        shading_point.get_geometric_normal(),
                        shading_normal);

                // Evaluate the BSDF at the vertex position.
                Spectrum bsdf_value;
                const bool bsdf_defined =
                    bsdf->evaluate(
                        bsdf_data,
                        true,
                        geometric_normal,
                        shading_point.get_shading_basis(),
                        outgoing,                           // outgoing
                        vertex_to_camera,                   // incoming
                        bsdf_value);
                if (!bsdf_defined)
                    return true;    // proceed with this path

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
                const BSDF::Mode            bsdf_mode,
                const Spectrum&             throughput)
            {
                // The particle escapes.
            }

          private:
            const Camera&                   m_camera;
            const LightingConditions&       m_lighting_conditions;
            const ShadingContext            m_shading_context;

            const Spectrum                  m_initial_alpha;        // initial particle flux (in W)
            Vector3d                        m_camera_position;      // camera position in world space
            Vector3d                        m_camera_direction;     // camera direction (gaze) in world space
            double                          m_rcp_pixel_area;       // reciprocal of the area of a single pixel (in m^-2)
            double                          m_focal_length;         // camera's focal length (in m)
            SampleVector&                   m_samples;
            size_t                          m_sample_count;         // the number of samples added to m_samples

            bool vertex_visible_to_camera(
                SamplingContext&            sampling_context,
                const Vector3d&             vertex_position_world,
                Vector2d&                   sample_position_ndc,
                double&                     transmission,
                Vector3d&                   vertex_to_camera,
                double&                     square_distance) const
            {
                // Transform the vertex position to camera space.
                const Vector3d vertex_position_camera =
                    m_camera.get_transform().transform_point_to_local(vertex_position_world);

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
                Tracer tracer(
                    m_shading_context.get_intersector(),
                    m_shading_context.get_texture_cache(),
                    sampling_context);
                const ShadingPoint& shading_point =
                    tracer.trace_between(
                        m_camera_position,
                        vertex_position_world,
                        transmission);

                // Reject vertices not directly visible from the camera.
                if (shading_point.hit())
                    return false;

                // Compute the vertex-to-camera direction vector.
                vertex_to_camera = m_camera_position - vertex_position_world;
                square_distance = square_norm(vertex_to_camera);
                vertex_to_camera /= sqrt(square_distance);

                return true;
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

        typedef PathTracer<
            PathVisitor,
            BSDF::Diffuse | BSDF::Glossy | BSDF::Specular,
            true    // adjoint
        > PathTracerType;

        const Parameters                m_params;
        Statistics                      m_stats;

        const Scene&                    m_scene;
        const Frame&                    m_frame;

        const EnvironmentEDF*           m_env_edf;

        // Preserve order.
        const double                    m_safe_scene_radius;    // radius of the scene's bounding sphere + small safety margin
        const double                    m_disk_point_prob;

        const LightSampler&             m_light_sampler;
        Intersector                     m_intersector;
        TextureCache                    m_texture_cache;

        MersenneTwister                 m_rng;

        uint64                          m_light_sample_count;

        virtual size_t generate_samples(
            const size_t                sequence_index,
            SampleVector&               samples)
        {
            SamplingContext sampling_context(m_rng, 0, 0, sequence_index);

            size_t stored_sample_count = 0;

            if (m_light_sampler.has_lights())
                stored_sample_count += generate_light_sample(sampling_context, samples);

            if (m_env_edf)
                stored_sample_count += generate_environment_sample(sampling_context, samples);

            ++m_light_sample_count;

            return stored_sample_count;
        }

        size_t generate_light_sample(
            SamplingContext&            sampling_context,
            SampleVector&               samples)
        {
            // Sample the light sources.
            sampling_context = sampling_context.split(3, 1);
            LightSample light_sample;
            const bool got_sample =
                m_light_sampler.sample(sampling_context.next_vector2<3>(), light_sample);
            assert(got_sample);

            // Make sure the geometric normal of the light sample is in the same hemisphere as the shading normal.
            light_sample.m_input_params.m_geometric_normal =
                flip_to_same_hemisphere(
                    light_sample.m_input_params.m_geometric_normal,
                    light_sample.m_input_params.m_shading_normal);

            // Evaluate the input values of the EDF of this light sample.
            InputEvaluator edf_input_evaluator(m_texture_cache);
            const void* edf_data =
                edf_input_evaluator.evaluate(
                    light_sample.m_edf->get_inputs(),
                    light_sample.m_input_params);

            // Sample the EDF.
            sampling_context = sampling_context.split(2, 1);
            Vector3d emission_direction;
            Spectrum edf_value;
            double edf_prob;
            light_sample.m_edf->sample(
                edf_data,
                light_sample.m_input_params.m_geometric_normal,
                Basis3d(light_sample.m_input_params.m_shading_normal),
                sampling_context.next_vector2<2>(),
                emission_direction,
                edf_value,
                edf_prob);

            // Compute the initial particle weight.
            Spectrum initial_alpha = edf_value;
            initial_alpha *=
                static_cast<float>(
                    dot(emission_direction, light_sample.m_input_params.m_shading_normal)
                        / (light_sample.m_probability * edf_prob));

            // Manufacture a ShadingPoint object at the position of the light sample.
            // It will be used to avoid self-intersections.
            const EmittingTriangle& emitting_triangle =
                m_light_sampler.get_emitting_triangle(light_sample.m_triangle_index);
            ShadingPoint parent_shading_point;
            m_intersector.manufacture_hit(
                parent_shading_point,
                ShadingRay(light_sample.m_input_params.m_point, emission_direction, 0.0, 0.0, 0.0f, ~0),
                emitting_triangle.m_assembly_instance_uid,
                emitting_triangle.m_object_instance_index,
                emitting_triangle.m_region_index,
                emitting_triangle.m_triangle_index,
                emitting_triangle.m_triangle_support_plane);

            // Build the light ray.
            const ShadingRay light_ray(
                light_sample.m_input_params.m_point,
                emission_direction,
                0.0f,
                ~0);

            // Build the path tracer.
            PathVisitor path_visitor(
                m_scene,
                m_frame,
                m_intersector,
                m_texture_cache,
                samples,
                initial_alpha);
            PathTracerType path_tracer(
                path_visitor,
                m_params.m_minimum_path_length);

            // Handle the light vertex separately.
            Spectrum light_particle_flux = edf_value;       // todo: only works for diffuse EDF? What we need is the light exitance
            light_particle_flux /= static_cast<float>(light_sample.m_probability);
            path_visitor.visit_light_vertex(
                sampling_context,
                light_sample,
                light_particle_flux);

            // Trace the light path.
            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    m_intersector,
                    m_texture_cache,
                    light_ray,
                    &parent_shading_point);

            // Update path statistics.
            ++m_stats.m_path_count;
            m_stats.m_path_length.insert(path_length);

            // Return the number of samples generated when tracing this light path.
            return path_visitor.get_sample_count();
        }

        size_t generate_environment_sample(
            SamplingContext&            sampling_context,
            SampleVector&               samples)
        {
            // Sample the environment.
            sampling_context = sampling_context.split(2, 1);
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
            sampling_context = sampling_context.split(2, 1);
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
                m_scene,
                m_frame,
                m_intersector,
                m_texture_cache,
                samples,
                initial_alpha);
            PathTracerType path_tracer(
                path_visitor,
                m_params.m_minimum_path_length);

            // Trace the light path.
            const size_t path_length =
                path_tracer.trace(
                    sampling_context,
                    m_intersector,
                    m_texture_cache,
                    light_ray);

            // Update path statistics.
            ++m_stats.m_path_count;
            m_stats.m_path_length.insert(path_length);

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
    const LightSampler&     light_sampler,
    const ParamArray&       params)
  : m_scene(scene)
  , m_frame(frame)
  , m_trace_context(trace_context)
  , m_light_sampler(light_sampler)
  , m_params(params)
{
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
