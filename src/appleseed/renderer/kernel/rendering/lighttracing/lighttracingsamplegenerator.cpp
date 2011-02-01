
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
#include "renderer/kernel/lighting/transmission.h"
#include "renderer/kernel/intersection/intersector.h"
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
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/image/spectrum.h"
#include "foundation/math/population.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng.h"
#include "foundation/math/vector.h"
#include "foundation/utility/memory.h"

// Forward declarations.
namespace foundation    { class LightingConditions; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // LightTracingSampleGenerator class implementation.
    //

    class LightTracingSampleGenerator
      : public SampleGeneratorBase
    {
      public:
        LightTracingSampleGenerator(
            const Scene&            scene,
            const Frame&            frame,
            const TraceContext&     trace_context,
            const LightSampler&     light_sampler,
            const size_t            generator_index,
            const size_t            generator_count,
            const ParamArray&       params)
          : SampleGeneratorBase(generator_index, generator_count)
          , m_params(params)
          , m_scene(scene)
          , m_frame(frame)
          , m_lighting_conditions(frame.get_lighting_conditions())
          , m_light_sampler(light_sampler)
          , m_intersector(trace_context, true, m_params.m_report_self_intersections)
          , m_texture_cache(scene, m_params.m_texture_cache_size)
        {
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
            size_t              m_path_count;
            Population<size_t>  m_path_length;

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
                const LightingConditions&   lighting_conditions,
                const Intersector&          intersector,
                TextureCache&               texture_cache,
                SampleVector&               samples,
                const Spectrum&             emitted_radiance)
              : m_camera(*scene.get_camera())
              , m_lighting_conditions(lighting_conditions)
              , m_intersector(intersector)
              , m_texture_cache(texture_cache)
              , m_samples(samples)
              , m_sample_count(0)
              , m_radiance(emitted_radiance)
            {
            }

            size_t get_sample_count() const
            {
                return m_sample_count;
            }

            void visit_vertex(
                SamplingContext&            sampling_context,
                const ShadingPoint&         shading_point,
                const Vector3d&             outgoing,
                const BSDF*                 bsdf,
                const void*                 bsdf_data,
                const BSDF::Mode            bsdf_mode,
                const double                bsdf_prob,
                const Spectrum&             throughput)
            {
                // Retrieve the world space position of this vertex.
                const Vector3d& vertex_position = shading_point.get_point();

                // Compute the position of this vertex on the image plane.
                // todo: implement proper ray/camera intersection.
                const Vector2d sample_position_ndc = m_camera.project(vertex_position);
                const Vector3d sample_position =
                    m_camera.get_transform().transform_point_to_parent(Vector3d(0.0));

                // Reject vertices not belonging on the image plane of the camera.
                if (sample_position_ndc[0] < -0.5 || sample_position_ndc[0] > 0.5 ||
                    sample_position_ndc[1] < -0.5 || sample_position_ndc[1] > 0.5)
                    return;

                // Compute the transmission factor between this vertex and the camera.
                const ShadingContext shading_context(m_intersector, m_texture_cache);
                const double transmission =
                    compute_transmission_between(
                        sampling_context,
                        shading_context,
                        vertex_position,
                        sample_position,
                        &shading_point);

                // Reject vertices not directly visible from the camera.
                if (transmission == 0.0)
                    return;

                // Update the path radiance.
                m_radiance *= throughput;

                // Shade this vertex.
                ShadingResult shading_result;
                shading_result.m_color_space = ColorSpaceSpectral;
                shading_result.m_color = m_radiance;
                shading_result.m_alpha = Alpha(1.0);
                shading_result.transform_to_linear_rgb(m_lighting_conditions);

                // Create a sample for this vertex.
                Sample sample;
                sample.m_position = sample_position_ndc;
                sample.m_color[0] = shading_result.m_color[0];
                sample.m_color[1] = shading_result.m_color[1];
                sample.m_color[2] = shading_result.m_color[2];
                sample.m_color[3] = shading_result.m_alpha[0];
                m_samples.push_back(sample);

                ++m_sample_count;
            }

            void visit_environment(
                const ShadingPoint&         shading_point,
                const Vector3d&             outgoing,
                const Spectrum&             throughput)
            {
            }

          private:
            const Camera&               m_camera;
            const LightingConditions&   m_lighting_conditions;
            const Intersector&          m_intersector;
            TextureCache&               m_texture_cache;
            SampleVector&               m_samples;
            size_t                      m_sample_count;     // the number of samples added to m_samples
            Spectrum                    m_radiance;
        };

        const Parameters                m_params;
        Statistics                      m_stats;

        const Scene&                    m_scene;
        const Frame&                    m_frame;
        const LightingConditions&       m_lighting_conditions;

        const LightSampler&             m_light_sampler;
        Intersector                     m_intersector;
        TextureCache                    m_texture_cache;

        MersenneTwister                 m_rng;
        LightSampleVector               m_light_samples;

        virtual size_t generate_samples(
            const size_t                    sequence_index,
            SampleVector&                   samples)
        {
            // Create a sampling context.
            SamplingContext sampling_context(
                m_rng,
                2,                      // number of dimensions
                0,                      // number of samples
                sequence_index);        // initial instance number

            // Generate a uniform sample in [0,1)^2 that will be used to sample the EDF.
            const Vector2d s = sampling_context.next_vector2<2>();

            // Get one light sample.
            // todo: enhance light sampler with a method to generate a single light sample.
            clear_keep_memory(m_light_samples);
            m_light_sampler.sample(sampling_context, 1, m_light_samples);
            const LightSample& light_sample = m_light_samples[0];

            // Evaluate the input values of the EDF of this light sample.
            InputEvaluator edf_input_evaluator(m_texture_cache);
            const void* edf_data =
                edf_input_evaluator.evaluate(
                    light_sample.m_edf->get_inputs(),
                    light_sample.m_input_params);

            // Sample the EDF.
            Vector3d emission_direction;
            Spectrum emitted_radiance;
            double emission_direction_probability;
            light_sample.m_edf->sample(
                edf_data,
                light_sample.m_input_params.m_geometric_normal,
                Basis3d(light_sample.m_input_params.m_shading_normal),
                s,
                emission_direction,
                emitted_radiance,
                emission_direction_probability);

            // Build the light ray.
            const ShadingRay light_ray(
                light_sample.m_input_params.m_point,
                emission_direction,
                0.0f,
                ~0);

            typedef PathTracer<
                PathVisitor,
                BSDF::Diffuse | BSDF::Glossy | BSDF::Specular,
                true                    // adjoint
            > PathTracer;

            // Build a path tracer.
            PathVisitor path_visitor(
                m_scene,
                m_lighting_conditions,
                m_intersector,
                m_texture_cache,
                samples,
                emitted_radiance);
            PathTracer path_tracer(
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

}   // namespace renderer
