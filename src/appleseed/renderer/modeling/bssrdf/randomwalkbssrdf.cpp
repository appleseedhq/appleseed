
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Artem Bishev, The appleseedhq Organization
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
#include "randomwalkbssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/glassbsdf.h"
#include "renderer/modeling/bsdf/lambertianbrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/sss.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/phasefunction.h"
#include "foundation/math/rr.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/arena.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <algorithm>
#include <cstddef>

// Forward declarations.
namespace renderer      { class BSDFSample; }
namespace renderer      { class BSSRDFSample; }
namespace renderer      { class ShadingContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Random-walk BSSRDF.
    //
    // References:
    //
    //   Path Traced Subsurface Scattering using Anisotropic Phase Functions
    //   and Non-Exponential Free Flights, Pixar Technical Memo 17-07
    //   https://graphics.pixar.com/library/PathTracedSubsurface/paper.pdf [1]
    //
    //   Johannes Meng, Johannes Hanika, Carsten Dachsbacher
    //   Improving the Dwivedi Sampling Scheme,
    //   Journal Computer Graphics Forum Vol. 35 Issue 4, pp. 37-44, July 2016.
    //   https://jo.dreggn.org/home/2016_dwivedi.pdf [2]
    //   https://jo.dreggn.org/home/2016_dwivedi_additional.pdf (supplement material) [3]
    //

    const char* Model = "randomwalk_bssrdf";

    class RandomwalkBSSRDF
      : public BSSRDF
    {
      public:
        RandomwalkBSSRDF(
            const char*             name,
            const ParamArray&       params)
          : BSSRDF(name, params)
        {
            m_inputs.declare("weight", InputFormatFloat, "1.0");
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("mfp", InputFormatSpectralReflectance);
            m_inputs.declare("mfp_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("ior", InputFormatFloat);
            m_inputs.declare("fresnel_weight", InputFormatFloat, "1.0");
            m_inputs.declare("volume_anisotropy", InputFormatFloat, "0.0");
            m_inputs.declare("surface_roughness", InputFormatFloat, "0.01");

            const string lambertian_brdf_name = string(name) + "_lambertian_brdf";
            m_lambertian_brdf = LambertianBRDFFactory().create(lambertian_brdf_name.c_str(), ParamArray());
            m_lambertian_brdf_data.m_reflectance.set(1.0f);
            m_lambertian_brdf_data.m_reflectance_multiplier = 1.0f;

            m_glass_bsdf = create_glass_bsdf(name, "ggx");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool on_frame_begin(
            const Project&              project,
            const BaseGroup*            parent,
            OnFrameBeginRecorder&       recorder,
            IAbortSwitch*               abort_switch) override
        {
            if (!BSSRDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            const OnFrameBeginMessageContext context("bssrdf", this);

            const string surface_bsdf =
                m_params.get_required<string>(
                    "surface_bsdf_model",
                    "diffuse",
                    make_vector("diffuse", "glass"),
                    context);

            if (surface_bsdf == "diffuse")
                m_use_glass_bsdf = false;
            else if (surface_bsdf == "glass" && m_glass_bsdf->on_frame_begin(project, parent, recorder, abort_switch))
                m_use_glass_bsdf = true;
            else return false;

            return true;
        }

        void on_frame_end(
            const Project&              project,
            const BaseGroup*            parent) override
        {
            BSSRDF::on_frame_end(project, parent);

            if (m_use_glass_bsdf)
                m_glass_bsdf->on_frame_end(project, parent);
        }

        size_t compute_input_data_size() const override
        {
            return sizeof(RandomwalkBSSRDFInputValues);
        }

        void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const override
        {
            RandomwalkBSSRDFInputValues* values =
                static_cast<RandomwalkBSSRDFInputValues*>(data);

            auto& precomputed = values->m_precomputed;

            // Apply multipliers to input values.
            values->m_reflectance *= values->m_reflectance_multiplier;
            values->m_mfp *= values->m_mfp_multiplier;

            // Clamp input values.
            clamp_in_place(values->m_reflectance, 0.001f, 0.999f);
            clamp_low_in_place(values->m_mfp, 1.0e-6f);

            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                precomputed.m_albedo[i] = albedo_from_reflectance_anisotropic(
                    values->m_reflectance[i], values->m_volume_anisotropy);
                precomputed.m_extinction[i] = rcp(values->m_mfp[i]);

                // Compute diffusion length, required by Dwivedi sampling.
                const float kappa = min(compute_rcp_diffusion_length(precomputed.m_albedo[i]), 0.99f);
                if (i == 0 || kappa < precomputed.m_rcp_diffusion_length)
                    precomputed.m_rcp_diffusion_length = kappa;
            }

            precomputed.m_eta = compute_eta(shading_point, values->m_ior);
        }

        static GlassBSDFInputValues* create_glass_inputs(
            Arena&                              arena,
            const RandomwalkBSSRDFInputValues*  bssrdf_values)
        {
            auto glass_values = arena.allocate_noinit<GlassBSDFInputValues>();

            glass_values->m_refraction_tint.set(1.0f);
            glass_values->m_anisotropy = 0.0f;
            glass_values->m_surface_transmittance.set(1.0f);
            glass_values->m_surface_transmittance_multiplier = 1.0f;
            glass_values->m_ior = bssrdf_values->m_ior;
            glass_values->m_roughness = bssrdf_values->m_surface_roughness;
            glass_values->m_energy_compensation = 1.0f;

            return glass_values;
        }

        static float albedo_from_reflectance_anisotropic(const float r, const float g)
        {
            const float s = 4.09712f + 4.20863f * r - sqrt(9.59271f + r * (41.6808f + 17.7126f * r));
            const float s2 = s * s;
            return (1.0f - s2) / (1.0f - g * s2);
        }

        static Vector3f sample_direction_given_cosine(
            const Vector3f& normal,
            const float cosine,
            const float s)
        {
            const float sine = std::sqrt(saturate(1.0f - cosine * cosine));
            const Vector2f tangent = sample_circle_uniform(s);
            Basis3f basis(normal);
            return
                basis.get_tangent_u() * tangent.x * sine +
                basis.get_tangent_v() * tangent.y * sine +
                basis.get_normal()    * cosine;
        }

        static bool test_rr(
            SamplingContext&    sampling_context,
            BSSRDFSample&       bssrdf_sample)
        {
            // Generate a uniform sample in [0,1).
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();

            // Compute the probability of extending this path.
            const float scattering_prob =
                std::min(max_value(bssrdf_sample.m_value), 0.99f);

            // Russian Roulette.
            if (!pass_rr(scattering_prob, s))
                return false;

            // Adjust throughput to account for terminated paths.
            assert(scattering_prob > 0.0f);
            bssrdf_sample.m_value /= scattering_prob;

            return true;
        }

        // Compute the probability to pick classical sampling instead of biased (Dwivedi) sampling.
        static float compute_classical_sampling_probability(const float anisotropy)
        {
            return max(0.1f, pow(abs(anisotropy), 3.0f));
        }

        bool sample(
            const ShadingContext&   shading_context,
            SamplingContext&        sampling_context,
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            BSSRDFSample&           bssrdf_sample,
            BSDFSample&             bsdf_sample) const override
        {
            // Get input values.
            const RandomwalkBSSRDFInputValues* values =
                static_cast<const RandomwalkBSSRDFInputValues*>(data);
            const Spectrum& extinction = values->m_precomputed.m_extinction;
            const Spectrum& albedo = values->m_precomputed.m_albedo;
            const float rcp_diffusion_length = values->m_precomputed.m_rcp_diffusion_length;
            const float diffusion_length = rcp(rcp_diffusion_length);

            if (values->m_weight == 0.0f)
                return false;

            // We use Henyey-Greenstein phase function for the volume bounces.
            HenyeyPhaseFunction phase_function(-values->m_volume_anisotropy);

            // Compute the probability of classical sampling.
            // Currently we always use classical sampling.
            const float classical_sampling_prob = 1.0f;

            // Initialize BSSRDF value.
            bssrdf_sample.m_value.set(1.0f);
            bssrdf_sample.m_probability = 1.0f;

            Vector3d scattering_point;
            Vector3f slab_normal;
            Vector3f direction;
            bool transmitted = false;
            if (m_use_glass_bsdf)
            {
                bool volume_scattering_occurred;
                if (!trace_zero_scattering_path_glass(
                    shading_context,
                    sampling_context,
                    extinction,
                    create_glass_inputs(shading_context.get_arena(), values),
                    outgoing_point,
                    outgoing_dir,
                    bssrdf_sample,
                    bsdf_sample,
                    volume_scattering_occurred,
                    scattering_point,
                    slab_normal,
                    direction))
                {
                    return false;
                }

                if (!volume_scattering_occurred)
                    return true;    // BSDF is already sampled here, so we just leave the method.
            }
            else
            {
                const Vector3f outgoing_normal(outgoing_point.get_shading_normal());
                const float cos_on = min(abs(dot(outgoing_dir, outgoing_normal)), 1.0f);
                float fo = 1.0f;
                if (values->m_fresnel_weight != 0.0f)
                {
                    // Fresnel factor at incoming direction.
                    fresnel_transmittance_dielectric(fo, values->m_precomputed.m_eta, cos_on);
                    fo = lerp(1.0f, fo, values->m_fresnel_weight);
                }

                bool volume_scattering_occurred;
                if (!trace_zero_scattering_path_diffuse(
                    shading_context,
                    sampling_context,
                    extinction,
                    outgoing_point,
                    outgoing_dir,
                    bssrdf_sample,
                    bsdf_sample,
                    volume_scattering_occurred,
                    scattering_point,
                    slab_normal,
                    direction))
                {
                    return false;
                }

                bssrdf_sample.m_value *= fo;

                if (!volume_scattering_occurred)
                {
                    bssrdf_sample.m_value *= values->m_reflectance;
                    transmitted = true;
                }
            }

            // Initialize the number of iterations.
            size_t n_iteration = 0;
            const size_t MaxIterationsCount = 256;
            const size_t MinRRIteration = 4;

            // Continue random walk until we reach the surface from inside.
            while (!transmitted && ++n_iteration < MaxIterationsCount)
            {
                if (n_iteration >= MinRRIteration && !test_rr(sampling_context, bssrdf_sample))
                    break;  // sample has not passed Russian Roulette test

                // Choose color channel used for distance sampling.
                sampling_context.split_in_place(1, 1);
                Spectrum channel_cdf, channel_pdf;
                build_cdf_and_pdf(bssrdf_sample.m_value, channel_cdf, channel_pdf);
                const size_t channel =
                    sample_cdf(
                        &channel_cdf[0],
                        &channel_cdf[0] + channel_cdf.size(),
                        sampling_context.next2<float>());
                if (albedo[channel] == 0.0f || bssrdf_sample.m_value[channel] == 0.0f)
                    break;

                // Find next random-walk direction.
                sampling_context.split_in_place(3, 1);
                const Vector3f s = sampling_context.next2<Vector3f>();
                Vector3f new_direction;
                float cosine;

                // Determine if we do biased (Dwivedi) sampling or classical sampling.
                sampling_context.split_in_place(1, 1);
                const bool is_biased = classical_sampling_prob < sampling_context.next2<float>();

                if (is_biased)
                {
                    cosine = sample_cosine_dwivedi(diffusion_length, s[0]);
                    new_direction = sample_direction_given_cosine(slab_normal, cosine, s[1]);
                }
                else
                {
                    phase_function.sample(direction, Vector2f(s[0], s[1]), new_direction);
                    new_direction = -new_direction;
                    cosine = dot(new_direction, slab_normal);
                }

                // Update transmission by the albedo.
                bssrdf_sample.m_value *= albedo;

                // Construct a new ray in the sampled direction.
                ShadingRay new_ray(
                    scattering_point,
                    Vector3d(new_direction),
                    outgoing_point.get_time(),
                    VisibilityFlags::ShadowRay,
                    outgoing_point.get_ray().m_depth + 1);

                // Sample distance assuming that the ray is infinite.
                const float extinction_bias = 1.0f - cosine * rcp_diffusion_length;
                const float effective_extinction = (is_biased ? extinction_bias : 1.0f) * extinction[channel];
                float distance = sample_exponential_distribution(s[2], effective_extinction);

                // Trace the ray up to the sampled distance.
                new_ray.m_tmax = distance;
                bssrdf_sample.m_incoming_point.clear();
                shading_context.get_intersector().trace(
                    new_ray,
                    bssrdf_sample.m_incoming_point);
                transmitted = bssrdf_sample.m_incoming_point.hit_surface();
                scattering_point = new_ray.point_at(distance);
                if (transmitted)
                    distance = static_cast<float>(bssrdf_sample.m_incoming_point.get_distance());

                // Compute transmission for this distance sample and apply MIS.
                Spectrum transmission;
                compute_transmission(
                    distance,
                    extinction,
                    channel_pdf,
                    transmitted,
                    transmission);

                bssrdf_sample.m_value *= transmission;
                direction = new_direction;
            }

            if (!transmitted)
                return false;  // sample was lost inside the object

            bssrdf_sample.m_brdf = m_lambertian_brdf.get();
            bssrdf_sample.m_brdf_data = &m_lambertian_brdf_data;
            bssrdf_sample.m_incoming_point.flip_side();

            // Sample the BSDF at the incoming point.
            bsdf_sample.m_mode = ScatteringMode::None;
            bsdf_sample.m_shading_point = &bssrdf_sample.m_incoming_point;
            bsdf_sample.m_geometric_normal = Vector3f(bssrdf_sample.m_incoming_point.get_geometric_normal());
            bsdf_sample.m_shading_basis = Basis3f(bssrdf_sample.m_incoming_point.get_shading_basis());
            bsdf_sample.m_outgoing = Dual3f(bsdf_sample.m_geometric_normal);      // chosen arbitrarily (no outgoing direction at the incoming point)
            bssrdf_sample.m_brdf->sample(
                sampling_context,
                bssrdf_sample.m_brdf_data,
                false,
                true,
                bssrdf_sample.m_modes,
                bsdf_sample);
            if (bsdf_sample.m_mode == ScatteringMode::None)
                return false;

            const float cos_in = min(abs(dot(
                bsdf_sample.m_geometric_normal,
                bsdf_sample.m_incoming.get_value())), 1.0f);
            float fi;
            if (values->m_fresnel_weight == 0.0f)
                fi = 1.0f;
            else
            {
                // Fresnel factor at incoming direction.
                fresnel_transmittance_dielectric(fi, values->m_precomputed.m_eta, cos_in);
                fi = lerp(1.0f, fi, values->m_fresnel_weight);
            }

            // Normalization constant.
            const float c = 1.0f - fresnel_first_moment_x2(values->m_precomputed.m_eta);

            bssrdf_sample.m_value *= fi / c;
            bssrdf_sample.m_value *= values->m_weight;

            return true;
        }

        bool trace_zero_scattering_path_glass(
            const ShadingContext&   shading_context,
            SamplingContext&        sampling_context,
            const Spectrum&         extinction,
            GlassBSDFInputValues*   glass_inputs,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            BSSRDFSample&           bssrdf_sample,
            BSDFSample&             bsdf_sample,
            bool&                   volume_scattering_occurred,
            Vector3d&               scattering_point,
            Vector3f&               slab_normal,
            Vector3f&               direction) const
        {
            // Initialize the number of iterations.
            size_t n_iteration = 0;

            const ShadingPoint* shading_point_ptr = &outgoing_point;
            size_t next_point_idx = 0;
            ShadingPoint shading_points[2];

            // Choose color channel used for distance sampling.
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();
            const size_t channel = truncate<size_t>(s * Spectrum::size());

            // Initially, the ray can only be refracted inside the object.
            glass_inputs->m_reflection_tint.set(0.0f);

            // Trace path until the first volume scattering event occurs or until the ray is refracted outside the object.
            volume_scattering_occurred = false;
            direction = -outgoing_dir;
            const size_t MaxIterationsCount = 32;
            while (!volume_scattering_occurred)
            {
                if (++n_iteration > MaxIterationsCount)
                    return false;

                // Sample glass BSDF.
                m_glass_bsdf->prepare_inputs(shading_context.get_arena(), *shading_point_ptr, glass_inputs);
                bsdf_sample.m_shading_point = shading_point_ptr;
                bsdf_sample.m_geometric_normal = Vector3f(shading_point_ptr->get_geometric_normal());
                bsdf_sample.m_shading_basis = Basis3f(shading_point_ptr->get_shading_basis());
                bsdf_sample.m_outgoing = Dual3f(-direction);
                bsdf_sample.m_mode = ScatteringMode::None;
                m_glass_bsdf->sample(sampling_context, glass_inputs, false, true, ScatteringMode::All, bsdf_sample);
                const bool crossing_interface =
                    dot(bsdf_sample.m_outgoing.get_value(), bsdf_sample.m_geometric_normal) *
                    dot(bsdf_sample.m_incoming.get_value(), bsdf_sample.m_geometric_normal) < 0.0;
                if (bsdf_sample.m_mode == ScatteringMode::None)
                    return false;

                assert(n_iteration != 1 || crossing_interface);  // no reflection should happen at the entry point.
                if (n_iteration != 1 && crossing_interface)
                {
                    if (!ScatteringMode::has_glossy(bssrdf_sample.m_modes))
                        return false;
                    // The ray was refracted with zero scattering.
                    glass_inputs->m_reflection_tint.set(0.0f);
                    m_glass_bsdf->prepare_inputs(shading_context.get_arena(), *shading_point_ptr, glass_inputs);
                    bssrdf_sample.m_brdf = m_glass_bsdf.get();
                    bssrdf_sample.m_brdf_data = glass_inputs;
                    bssrdf_sample.m_incoming_point = *shading_point_ptr;
                    return true;
                }
                bssrdf_sample.m_value *= bsdf_sample.m_value.m_glossy;
                bssrdf_sample.m_value /= bsdf_sample.m_probability;
                glass_inputs->m_reflection_tint.set(1.0f);

                // Sample distance (we always use classical sampling here).
                sampling_context.split_in_place(1, 1);
                const double distance = sample_exponential_distribution(
                    sampling_context.next2<double>(), static_cast<double>(extinction[channel]));

                // Trace the initial ray through the object completely.
                shading_points[next_point_idx].clear();
                direction = bsdf_sample.m_incoming.get_value();
                ShadingRay ray(
                    shading_point_ptr->get_point(),
                    Vector3d(direction),
                    outgoing_point.get_time(),
                    VisibilityFlags::SubsurfaceRay,
                    outgoing_point.get_ray().m_depth + 1);
                shading_context.get_intersector().trace(
                    ray,
                    shading_points[next_point_idx],
                    shading_point_ptr);
                if (!shading_points[next_point_idx].is_valid())
                    return false;
                // todo: check if the hit is on the same SSS set.
                const double ray_length = shading_points[next_point_idx].get_distance();

                // Check if volume scattering event occurred and compute transmission.
                volume_scattering_occurred = ray_length > distance;
                if (volume_scattering_occurred)
                {
                    scattering_point = ray.point_at(distance);

                    // Determine slab normal of the closest surface point.
                    const bool outgoing_point_is_closer = distance < 0.5f * ray_length;
                    slab_normal = outgoing_point_is_closer
                        ? Vector3f(shading_point_ptr->get_geometric_normal())
                        : Vector3f(-shading_points[next_point_idx].get_geometric_normal());
                }
                Spectrum transmission;
                compute_transmission(
                    static_cast<float>(volume_scattering_occurred ? distance : ray_length),
                    extinction,
                    !volume_scattering_occurred,
                    transmission);
                bssrdf_sample.m_value *= transmission;

                // Swap shading points.
                shading_point_ptr = &shading_points[next_point_idx];
                next_point_idx = 1 - next_point_idx;
            }

            return true;
        }

        bool trace_zero_scattering_path_diffuse(
            const ShadingContext&   shading_context,
            SamplingContext&        sampling_context,
            const Spectrum&         extinction,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            BSSRDFSample&           bssrdf_sample,
            BSDFSample&             bsdf_sample,
            bool&                   volume_scattering_occurred,
            Vector3d&               scattering_point,
            Vector3f&               slab_normal,
            Vector3f&               direction) const
        {
            // Pick initial random-walk direction uniformly at random.
            sampling_context.split_in_place(2, 1);
            Vector3d initial_dir = sample_hemisphere_cosine(sampling_context.next2<Vector2d>());
            initial_dir.y = -initial_dir.y;
            initial_dir = outgoing_point.get_shading_basis().transform_to_parent(initial_dir);
            direction = static_cast<Vector3f>(initial_dir);

            // Choose color channel used for distance sampling.
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();
            const size_t channel = truncate<size_t>(s * Spectrum::size());

            // Determine the thickness of the slab by tracing the first ray.
            ShadingRay ray(
                outgoing_point.get_point(),
                initial_dir,
                outgoing_point.get_time(),
                VisibilityFlags::SubsurfaceRay,
                outgoing_point.get_ray().m_depth + 1);
            bssrdf_sample.m_incoming_point.clear();
            shading_context.get_intersector().trace(
                ray,
                bssrdf_sample.m_incoming_point,
                &outgoing_point);
            if (!bssrdf_sample.m_incoming_point.is_valid())
                return false;

            // Sample distance (we always use classical sampling here).
            const double ray_length = bssrdf_sample.m_incoming_point.get_distance();
            sampling_context.split_in_place(1, 1);
            const double distance = sample_exponential_distribution(
                sampling_context.next2<double>(), static_cast<double>(extinction[channel]));
            volume_scattering_occurred = distance < ray_length;
            Spectrum transmission;
            compute_transmission(
                static_cast<float>(volume_scattering_occurred ? distance : ray_length),
                extinction,
                !volume_scattering_occurred,
                transmission);
            bssrdf_sample.m_value *= transmission;

            if (volume_scattering_occurred)
            {
                scattering_point = ray.point_at(distance);

                // Determine slab normal of the closest surface point.
                const bool outgoing_point_is_closer = distance < 0.5f * ray_length;
                slab_normal = outgoing_point_is_closer
                    ? Vector3f(outgoing_point.get_geometric_normal())
                    : Vector3f(bssrdf_sample.m_incoming_point.get_geometric_normal());
            }

            return true;
        }

        static void compute_transmission(
            const float         distance,
            const Spectrum&     extinction,
            const Spectrum&     channel_pdf,
            const bool          transmitted,
            Spectrum&           transmission)
        {
            float mis_base = 0.0f;

            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                const float x = -distance * extinction[i];
                assert(FP<float>::is_finite(x));
                transmission[i] = std::exp(x);
                if (!transmitted)
                    transmission[i] *= extinction[i];

                // One-sample estimator (Veach: 9.2.4 eq. 9.15).
                mis_base += transmission[i] * channel_pdf[i];
            }

            transmission *= rcp(mis_base);
        }

        static void compute_transmission(
            const float         distance,
            const Spectrum&     extinction,
            const bool          transmitted,
            Spectrum&           transmission)
        {
            float mis_base = 0.0f;
            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                const float x = -distance * extinction[i];
                assert(FP<float>::is_finite(x));
                transmission[i] = std::exp(x);
                if (!transmitted)
                    transmission[i] *= extinction[i];

                // One-sample estimator (Veach: 9.2.4 eq. 9.15).
                mis_base += transmission[i];
            }
            transmission *= Spectrum::size() / mis_base;
        }

        float compute_total_refraction_intensity(
            const void* data,
            const float cos_in) const
        {
            const RandomwalkBSSRDFInputValues* values =
                static_cast<const RandomwalkBSSRDFInputValues*>(data);

            float fi;

            if (values->m_fresnel_weight == 0.0f)
                fi = 1.0f;
            else
            {
                // Fresnel factor at incoming direction.
                fresnel_transmittance_dielectric(fi, values->m_precomputed.m_eta, cos_in);
                fi = lerp(1.0f, fi, values->m_fresnel_weight);
            }

            // Normalization constant.
            const float c = 1.0f - fresnel_first_moment_x2(values->m_precomputed.m_eta);

            return fi / c;
        }

        void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3f&         incoming_dir,
            const int               modes,
            Spectrum&               value) const override
        {
            // Deterministic approach is not possible here.
            value.set(0.0f);
        }

        static auto_release_ptr<BSDF> create_glass_bsdf(
            const char*             bssrdf_name,
            const char*             mdf_name)
        {
            const string glass_bsdf_name = string(bssrdf_name) + "_glass_bsdf_" + mdf_name;

            auto_release_ptr<BSDF> bsdf =
                GlassBSDFFactory().create(
                    glass_bsdf_name.c_str(),
                    ParamArray()
                        .insert("mdf", mdf_name)
                        .insert("volume_parameterization", "transmittance"));

            return bsdf;
        }

        auto_release_ptr<BSDF>          m_lambertian_brdf;
        LambertianBRDFInputValues       m_lambertian_brdf_data;
        bool                            m_use_glass_bsdf;

        auto_release_ptr<BSDF>          m_glass_bsdf;
    };
}


//
// RandomwalkBSSRDFFactory class implementation.
//

void RandomwalkBSSRDFFactory::release()
{
    delete this;
}

const char* RandomwalkBSSRDFFactory::get_model() const
{
    return Model;
}

Dictionary RandomwalkBSSRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Random-Walk BSSRDF");
}

DictionaryArray RandomwalkBSSRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "weight")
            .insert("label", "Weight")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance")
            .insert("label", "Diffuse Surface Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Diffuse Surface Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mfp")
            .insert("label", "Mean Free Path")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mfp_multiplier")
            .insert("label", "Mean Free Path Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ior")
            .insert("label", "Index of Refraction")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "2.5")
                    .insert("type", "hard"))
            .insert("use", "required")
            .insert("default", "1.3"));

    metadata.push_back(
        Dictionary()
            .insert("name", "fresnel_weight")
            .insert("label", "Fresnel Weight")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_anisotropy")
            .insert("label", "Volume Anisotropy")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-1.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "surface_bsdf_model")
            .insert("label", "Surface BSDF Model")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Glass BSDF", "glass")
                    .insert("Diffuse BTDF", "diffuse"))
            .insert("use", "required")
            .insert("default", "diffuse")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "surface_roughness")
            .insert("label", "Surface Roughness")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("visible_if",
                Dictionary().insert("surface_bsdf_model", "glass")));

    return metadata;
}

auto_release_ptr<BSSRDF> RandomwalkBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new RandomwalkBSSRDF(name, params));
}

}   // namespace renderer
