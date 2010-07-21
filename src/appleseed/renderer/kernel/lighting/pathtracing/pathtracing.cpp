
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "pathtracing.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/directlighting.h"
#include "renderer/kernel/lighting/imagebasedlighting.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/inputparams.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/distance.h"
#include "foundation/math/mis.h"
#include "foundation/math/population.h"
#include "foundation/math/rr.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/string.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    //
    // Path tracing lighting engine.
    //
    // Implementation of spectral, Monte Carlo forward path tracing,
    // optionally with next event estimation.
    //
    // Reference:
    //
    //   http://citeseer.ist.psu.edu/344088.html
    //

    class PTLightingEngine
      : public ILightingEngine
    {
      public:
        // Constructor.
        PTLightingEngine(
            const LightSampler&     light_sampler,
            const ParamArray&       params)
          : m_params(params)
          , m_light_sampler(light_sampler)
        {
        }

        // Destructor.
        ~PTLightingEngine()
        {
            RENDERER_LOG_DEBUG(
                "path tracing statistics:\n"
                "  paths            %s\n"
                "  path length      avg %.1f  min %s  max %s  dev %.1f\n",
                pretty_uint(m_stats.m_path_count).c_str(),
                m_stats.m_path_length.get_avg(),
                pretty_uint(m_stats.m_path_length.get_min()).c_str(),
                pretty_uint(m_stats.m_path_length.get_max()).c_str(),
                m_stats.m_path_length.get_dev());
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Compute the lighting at a given point of the scene.
        virtual void compute_lighting(
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Spectrum&               radiance)   // output radiance, in W.sr^-1.m^-2
        {
            // Initialize path radiance.
            radiance.set(0.0f);

            // Retrieve items from the shading context.
            const Intersector& intersector = shading_context.get_intersector();
            SamplingContext sampling_context = shading_context.get_sampling_context();
            TextureCache& texture_cache = shading_context.get_texture_cache();

            ShadingPoint shading_points[2];
            size_t shading_point_index = 0;
            const ShadingPoint* shading_point_ptr = &shading_point;

            // Trace one path.
            Spectrum throughput(1.0f);
            size_t path_length = 0;
            bool specular = true;
            double bsdf_prob = -1.0;
            while (true)
            {
                // Limit the length of the path.
                const size_t MaxPathLength = 10000;
                ++path_length;
                if (path_length >= MaxPathLength)
                {
                    RENDERER_LOG_WARNING(
                        "reached path length limit (%s), terminating path",
                        pretty_int(path_length).c_str());
                    break;
                }

                // Retrieve the ray.
                const ShadingRay& ray = shading_point_ptr->get_ray();
                const Vector3d outgoing = normalize(-ray.m_dir);

                // If the ray didn't hit anything, terminate the path.
                if (!shading_point_ptr->hit())
                {
                    if (!m_params.m_next_event_estimation)
                    {
                        // Retrieve the environment's EDF.
                        const Scene& scene = shading_point_ptr->get_scene();
                        const Environment* environment = scene.get_environment();
                        const EnvironmentEDF* env_edf =
                            environment ? environment->get_environment_edf() : 0;

                        if (env_edf)
                        {
                            // Evaluate the environment's EDF.
                            InputEvaluator input_evaluator(texture_cache);
                            Spectrum emitted_radiance;
                            env_edf->evaluate(
                                input_evaluator,
                                -outgoing,
                                emitted_radiance);

                            // Update the path radiance.
                            emitted_radiance *= throughput;
                            radiance += emitted_radiance;
                        }
                    }

                    // Terminate the path.
                    break;
                }

                // Retrieve the material at the shading point.
                const Material* material = shading_point_ptr->get_material();

                // Terminate the path if the surface has no material.
                if (material == 0)
                    break;

                // Retrieve the geometry of the intersection.
                const InputParams& input_params = shading_point_ptr->get_input_params();
                const Vector3d& point = shading_point_ptr->get_point();
                const Vector3d& geometric_normal = shading_point_ptr->get_geometric_normal();
                const Vector3d& shading_normal = shading_point_ptr->get_shading_normal();
                const Basis3d& shading_basis = shading_point_ptr->get_shading_basis();

                // Retrieve the surface shader.
                const SurfaceShader& surface_shader = material->get_surface_shader();

                // Evaluate the alpha mask at the shading point.
                Alpha alpha_mask;
                surface_shader.evaluate_alpha_mask(
                    shading_context,
                    *shading_point_ptr,
                    alpha_mask);

                // Handle alpha masking.
                const double cutoff_prob = 1.0 - alpha_mask[0];
                if (cutoff_prob > 0.0)
                {
                    if (sampling_context.next_double1() >= 1.0 - cutoff_prob)
                    {
                        // Construct a ray that continues in the same direction as the incoming ray.
                        const ShadingRay cutoff_ray(
                            point,
                            ray.m_dir,
                            0.0f,           // ray time
                            ~0);            // ray flags

                        // Trace the ray.
                        shading_points[shading_point_index].clear();
                        intersector.trace(
                            cutoff_ray,
                            shading_points[shading_point_index],
                            shading_point_ptr);

                        // Update the pointers to the shading points.
                        shading_point_ptr = &shading_points[shading_point_index];
                        shading_point_index = 1 - shading_point_index;

                        continue;
                    }
                }

                // Retrieve the BSDF.
                // Terminate the path if the material has no BSDF.
                const BSDF* bsdf = material->get_bsdf();
                if (bsdf == 0)
                    break;

                // Evaluate the input values of the BSDF.
                InputEvaluator bsdf_input_evaluator(texture_cache);
                const void* bsdf_data =
                    bsdf_input_evaluator.evaluate(
                        bsdf->get_inputs(),
                        input_params);

                // Retrieve the EDF.
                const EDF* edf = material->get_edf();

                // Evaluate the input values of the EDF (if any).
                InputEvaluator edf_input_evaluator(texture_cache);
                const void* edf_data = edf
                    ? edf_input_evaluator.evaluate(
                        edf->get_inputs(),
                        input_params)
                    : 0;

                if (m_params.m_next_event_estimation)
                {
                    // Generate a single light sample.
                    clear_keep_memory(m_light_samples);
                    m_light_sampler.sample(
                        sampling_context,
                        point,
                        shading_normal,
                        m_params.m_dl_sample_count,
                        m_light_samples);

                    // Compute the incident radiance due to this light sample.
                    Spectrum direct_radiance;
                    compute_direct_lighting(
                        shading_context,
                        point,
                        geometric_normal,
                        shading_basis,
                        outgoing,
                        *bsdf,
                        bsdf_data,
                        m_light_samples,
                        direct_radiance,
                        shading_point_ptr);

                    // Compute image-based lighting.
                    Spectrum ibl_radiance;
                    compute_image_based_lighting(
                        shading_context,
                        shading_point_ptr->get_scene(),
                        point,
                        geometric_normal,
                        shading_basis,
                        outgoing,
                        *bsdf,
                        bsdf_data,
                        m_params.m_ibl_bsdf_sample_count,
                        m_params.m_ibl_env_sample_count,
                        ibl_radiance,
                        shading_point_ptr);

                    // Update the path radiance.
                    direct_radiance += ibl_radiance;
                    direct_radiance *= throughput;
                    radiance += direct_radiance;

                    if (edf)
                    {
                        // Compute the emitted radiance.
                        Spectrum emitted_radiance;
                        edf->evaluate(
                            edf_data,
                            geometric_normal,
                            shading_basis,
                            outgoing,
                            emitted_radiance);

                        if (!specular)
                        {
                            // Compute the probability density with respect to surface area
                            // of choosing this point through sampling of the light sources.
                            const double sample_probability =
                                m_light_sampler.evaluate_pdf(*shading_point_ptr);

                            // Compute the probability density with respect to surface area
                            // of the direction obtained through sampling of the BSDF.
                            double px = bsdf_prob;
                            px *= max(dot(outgoing, shading_normal), 0.0);
                            px /= square(shading_point_ptr->get_distance());

                            // Multiply the emitted radiance by the MIS weight.
                            const double mis_weight = mis_power2(px, sample_probability);
                            emitted_radiance *= static_cast<float>(mis_weight);
                        }

                        // Update the path radiance.
                        emitted_radiance *= throughput;
                        radiance += emitted_radiance;
                    }
                }
                else
                {
                    if (edf)
                    {
                        // Compute the emitted radiance.
                        Spectrum emitted_radiance;
                        edf->evaluate(
                            edf_data,
                            geometric_normal,
                            shading_basis,
                            outgoing,
                            emitted_radiance);

                        // Update the path radiance.
                        emitted_radiance *= throughput;
                        radiance += emitted_radiance;
                    }
                }

                // Generate a uniform sample in [0,1)^4.
                sampling_context = sampling_context.split(4, 1);
                const Vector4d s = sampling_context.next_vector2<4>();

                // Sample the BSDF.
                Vector3d incoming;
                Spectrum bsdf_value;
                BSDF::Mode mode;
                bsdf->sample(
                    bsdf_data,
                    geometric_normal,
                    shading_basis,
                    Vector3d(s[0], s[1], s[2]),
                    outgoing,
                    incoming,
                    bsdf_value,
                    bsdf_prob,
                    mode);
                specular = (mode == BSDF::Specular);

                // Handle absorption.
                if (mode == BSDF::None)
                    break;

                // Multiply the BSDF value by cos(theta).
                assert(is_normalized(incoming));
                const double cos_in = abs(dot(incoming, shading_normal));
                bsdf_value *= static_cast<float>(cos_in);

                // Update the path throughput.
                throughput *= bsdf_value;

                // Use Russian Roulette to cut the path without introducing bias.
                if (path_length >= m_params.m_minimum_path_length)
                {
                    const double scattering_prob =
                        min(static_cast<double>(max_value(bsdf_value)), 1.0);

                    if (!pass_rr(scattering_prob, s[3]))
                        break;

                    assert(scattering_prob > 0.0);
                    throughput /= static_cast<float>(scattering_prob);
                }

                // Construct the scattered ray.
                const ShadingRay scattered_ray(
                    point,
                    incoming,
                    0.0f,           // ray time
                    ~0);            // ray flags

                // Trace the ray.
                shading_points[shading_point_index].clear();
                intersector.trace(
                    scattered_ray,
                    shading_points[shading_point_index],
                    shading_point_ptr);

                // Update the pointers to the shading points.
                shading_point_ptr = &shading_points[shading_point_index];
                shading_point_index = 1 - shading_point_index;
            }

            // Update path statistics.
            ++m_stats.m_path_count;
            m_stats.m_path_length.insert(path_length);
        }

      private:
        // Parameters.
        struct Parameters
        {
            const bool          m_next_event_estimation;    // use next event estimation?
            const size_t        m_minimum_path_length;      // minimum path length before Russian Roulette is used
            const size_t        m_dl_sample_count;          // number of samples used to estimate direct illumination
            const size_t        m_ibl_bsdf_sample_count;    // number of samples (in BSDF sampling) used to estimate IBL
            const size_t        m_ibl_env_sample_count;     // number of samples (in environment sampling) used to estimate IBL

            // Constructor, extract parameters.
            explicit Parameters(const ParamArray& params)
              : m_next_event_estimation ( params.get_optional<bool>("next_event_estimation", true) )
              , m_minimum_path_length   ( params.get_optional<size_t>("minimum_path_length", 3) )
              , m_dl_sample_count       ( params.get_optional<size_t>("dl_samples", 1) )
              , m_ibl_bsdf_sample_count ( params.get_optional<size_t>("ibl_bsdf_samples", 2) )
              , m_ibl_env_sample_count  ( params.get_optional<size_t>("ibl_env_samples", 2) )
            {
            }
        };

        // Statistics.
        struct Statistics
        {
            size_t              m_path_count;               // number of paths
            Population<size_t>  m_path_length;              // path length

            // Constructor.
            Statistics()
              : m_path_count(0)
            {
            }
        };

        const Parameters        m_params;
        Statistics              m_stats;
        const LightSampler&     m_light_sampler;
        LightSampleVector       m_light_samples;
    };

}   // anonymous namespace


//
// PTLightingEngineFactory class implementation.
//

// Constructor.
PTLightingEngineFactory::PTLightingEngineFactory(
    const LightSampler& light_sampler,
    const ParamArray&   params)
  : m_light_sampler(light_sampler)
  , m_params(params)
{
}

// Delete this instance.
void PTLightingEngineFactory::release()
{
    delete this;
}

// Return a new path tracing lighting engine instance.
ILightingEngine* PTLightingEngineFactory::create()
{
    return new PTLightingEngine(m_light_sampler, m_params);
}

// Return a new path tracing lighting engine instance.
ILightingEngine* PTLightingEngineFactory::create(
    const LightSampler& light_sampler,
    const ParamArray&   params)
{
    return new PTLightingEngine(light_sampler, params);
}

}   // namespace renderer
