
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Artem Bishev, The appleseedhq Organization
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
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/sss.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/rr.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <algorithm>
#include <cstddef>

// Forward declarations.
namespace foundation    { class Arena; }
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

    class RandomWalkBSSRDF
      : public BSSRDF
    {
      public:
        RandomWalkBSSRDF(
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
            m_inputs.declare("zero_scattering_weight", InputFormatFloat, "1.0");

            const string brdf_name = string(name) + "_brdf";
            m_brdf = LambertianBRDFFactory().create(brdf_name.c_str(), ParamArray()).release();
            m_brdf_data.m_reflectance.set(1.0f);
            m_brdf_data.m_reflectance_multiplier = 1.0f;
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        size_t compute_input_data_size() const override
        {
            return sizeof(RandomWalkBSSRDFInputValues);
        }

        void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const override
        {
            RandomWalkBSSRDFInputValues* values =
                static_cast<RandomWalkBSSRDFInputValues*>(data);

            auto& precomputed = values->m_precomputed;

            // Apply multipliers to input values.
            values->m_reflectance *= values->m_reflectance_multiplier;
            values->m_mfp *= values->m_mfp_multiplier;

            // Clamp input values.
            clamp_in_place(values->m_reflectance, 0.001f, 0.999f);
            clamp_low_in_place(values->m_mfp, 1.0e-6f);

            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                precomputed.m_albedo[i] = albedo_from_reflectance(values->m_reflectance[i]);
                precomputed.m_extinction[i] = rcp(values->m_mfp[i]);

                // Compute diffusion length, required by Dwivedi sampling.
                const float kappa = min(compute_rcp_diffusion_length(precomputed.m_albedo[i]), 0.99f);
                if (i == 0 || kappa < precomputed.m_rcp_diffusion_length)
                    precomputed.m_rcp_diffusion_length = kappa;
            }

            values->m_precomputed.m_eta = compute_eta(shading_point, values->m_ior);
        }

        static float albedo_from_reflectance(const float r)
        {
            return 1.0f - exp(r * (-5.09406f + r * (2.61188f - 4.31805f * r)));
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
            const RandomWalkBSSRDFInputValues* values =
                static_cast<const RandomWalkBSSRDFInputValues*>(data);
            const Spectrum& extinction = values->m_precomputed.m_extinction;
            const Spectrum& albedo = values->m_precomputed.m_albedo;
            const float rcp_diffusion_length = values->m_precomputed.m_rcp_diffusion_length;
            const float diffusion_length = rcp(rcp_diffusion_length);

            // Compute the probability of classical sampling.
            // The probability of classical sampling is high if phase function is anisotropic.
            const float classical_sampling_prob = compute_classical_sampling_probability(0.0f);

            // Initialize BSSRDF value.
            bssrdf_sample.m_value.set(values->m_weight);
            bssrdf_sample.m_probability = 1.0f;

            // Pick initial random-walk direction uniformly at random.
            sampling_context.split_in_place(2, 1);
            Vector3d initial_dir = sample_hemisphere_uniform(sampling_context.next2<Vector2d>());
            initial_dir.y = -initial_dir.y;
            initial_dir = outgoing_point.get_shading_basis().transform_to_parent(initial_dir);
            ShadingRay ray(
                outgoing_point.get_point(),
                initial_dir,
                outgoing_point.get_time(),
                VisibilityFlags::ShadowRay,
                outgoing_point.get_ray().m_depth + 1);

            sampling_context.split_in_place(1, 2);

            // Choose color channel used for distance sampling.
            const float s = sampling_context.next2<float>();
            const size_t channel = truncate<size_t>(s * Spectrum::size());
            Spectrum channel_pdf(1.0f / Spectrum::size());

            // Sample distance (we always use classical sampling here).
            const float distance = sample_exponential_distribution(
                sampling_context.next2<float>(), extinction[channel]);

            // Trace the initial ray through the object completely.
            bssrdf_sample.m_incoming_point.clear();
            shading_context.get_intersector().trace(
                ray,
                bssrdf_sample.m_incoming_point,
                &outgoing_point);
            if (!bssrdf_sample.m_incoming_point.is_valid())
                return false;
            const float ray_length = static_cast<float>(bssrdf_sample.m_incoming_point.get_distance());
            bool transmitted = ray_length <= distance;
            compute_transmission(
                transmitted ? ray_length : distance,
                extinction,
                channel_pdf,
                transmitted,
                bssrdf_sample.m_value);

            // Determine the closest surface point and corresponding slab normal.
            const bool outgoing_point_is_closer = distance < 0.5f * ray_length;
            const Vector3f near_slab_normal = Vector3f(outgoing_point.get_geometric_normal());
            const Vector3f far_slab_normal = Vector3f(-bssrdf_sample.m_incoming_point.get_geometric_normal());
            const Vector3f& slab_normal = outgoing_point_is_closer ? near_slab_normal : far_slab_normal;
            Vector3d current_point = ray.point_at(distance);

            // Continue random walk until we reach the surface from inside.
            size_t n_iteration = 0;
            const size_t MaxIterationsCount = 64;
            const size_t MinRRIteration = 4;
            while (!transmitted && ++n_iteration < MaxIterationsCount)
            {
                if (n_iteration >= MinRRIteration && !test_rr(sampling_context, bssrdf_sample))
                    break;  // sample has not passed Rusian Roulette test

                sampling_context.split_in_place(1, 2);

                // Choose color channel used for distance sampling.
                Spectrum channel_cdf;
                build_cdf_and_pdf(bssrdf_sample.m_value, channel_cdf, channel_pdf);
                const size_t channel = sample_cdf(
                    &channel_cdf[0],
                    &channel_cdf[0] + channel_cdf.size(),
                    sampling_context.next2<float>());
                if (albedo[channel] == 0.0f || bssrdf_sample.m_value[channel] == 0.0f) break;

                // Determine if we do biased (Dwivedi) sampling or classical sampling.
                const bool is_biased = classical_sampling_prob < sampling_context.next2<float>();

                // Find next random-walk direction.
                sampling_context.split_in_place(3, 1);
                const Vector3f s = sampling_context.next2<Vector3f>();
                Vector3f direction;
                float cosine;
                if (is_biased)
                {
                    cosine = sample_cosine_dwivedi(diffusion_length, s[0]);
                    direction = sample_direction_given_cosine(slab_normal, cosine, s[1]);
                }
                else
                {
                    direction = sample_sphere_uniform(Vector2f(s[0], s[1]));
                    cosine = dot(direction, slab_normal);
                }

                // Update transmission by the albedo.
                bssrdf_sample.m_value *= albedo;

                // Construct a new ray in the sampled direction.
                ShadingRay ray(
                    current_point,
                    Vector3d(direction),
                    outgoing_point.get_time(),
                    VisibilityFlags::ShadowRay,
                    ray.m_depth + 1);

                // Sample distance assuming that the ray is infinite.
                const float extinction_bias = 1.0f - cosine * rcp_diffusion_length;
                const float effective_extinction = (is_biased ? extinction_bias : 1.0f) * extinction[channel];
                float distance = sample_exponential_distribution(s[2], effective_extinction);

                // Trace the ray up to the sampled distance.
                ray.m_tmax = distance;
                bssrdf_sample.m_incoming_point.clear();
                shading_context.get_intersector().trace(
                    ray,
                    bssrdf_sample.m_incoming_point);
                transmitted = bssrdf_sample.m_incoming_point.hit_surface();
                current_point = ray.point_at(distance);
                if (transmitted)
                {
                    distance = static_cast<float>(
                        bssrdf_sample.m_incoming_point.get_distance());
                }

                // Compute transmission for this distance sample and apply MIS.
                Spectrum transmission;
                compute_transmission(
                    distance,
                    extinction,
                    channel_pdf,
                    transmitted,
                    transmission);

                const float q_direction = 0.5f / evaluate_cosine_dwivedi(diffusion_length, cosine);
                float q_distance = std::exp(-distance * extinction[channel] * cosine * rcp_diffusion_length);
                if (!transmitted) q_distance /= extinction_bias;
                const float q = q_direction * q_distance;  // PDF of classical sampling divided by PDF of biased samling

                // MIS between classical and biased sampling.
                const float mis_weight = rcp(mix(q, 1.0f, classical_sampling_prob));

                bssrdf_sample.m_value *= transmission;
                bssrdf_sample.m_value *= mis_weight * (is_biased ? q : 1.0f);
            }

            if (!transmitted) return false;  // sample was lost inside the object

            if (n_iteration == 0)
                bssrdf_sample.m_value *= values->m_zero_scattering_weight;

            bssrdf_sample.m_brdf = m_brdf;
            bssrdf_sample.m_brdf_data = &m_brdf_data;
            bssrdf_sample.m_incoming_point.flip_side();

            // Sample the BSDF at the incoming point.
            bsdf_sample.m_shading_point = &bssrdf_sample.m_incoming_point;
            bsdf_sample.m_geometric_normal = Vector3f(bssrdf_sample.m_incoming_point.get_geometric_normal());
            bsdf_sample.m_shading_basis = Basis3f(bssrdf_sample.m_incoming_point.get_shading_basis());
            bsdf_sample.m_outgoing = Dual3f(bsdf_sample.m_geometric_normal);      // chosen arbitrarily (no outgoing direction at the incoming point)
            bssrdf_sample.m_brdf->sample(
                sampling_context,
                bssrdf_sample.m_brdf_data,
                false,
                true,
                ScatteringMode::All,
                bsdf_sample);

            // Take Fresnel term into account.
            const float cos_on = min(abs(dot(
                near_slab_normal,
                outgoing_dir)), 1.0f);
            const float cos_in = min(abs(dot(
                bsdf_sample.m_geometric_normal,
                bsdf_sample.m_incoming.get_value())), 1.0f);
            bssrdf_sample.m_value *= compute_total_refraction_intensity(data, cos_on, cos_in);

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
                if (!transmitted) transmission[i] *= extinction[i];

                // One-sample estimator (Veach: 9.2.4 eq. 9.15).
                mis_base += transmission[i] * channel_pdf[i];
            }
            transmission *= rcp(mis_base);
        }

        float compute_total_refraction_intensity(
            const void* data,
            const float cos_on,
            const float cos_in) const
        {
            const RandomWalkBSSRDFInputValues* values =
                static_cast<const RandomWalkBSSRDFInputValues*>(data);

            float fo, fi;

            if (values->m_fresnel_weight == 0.0f)
                fo = fi = 1.0f;
            else
            {
                // Fresnel factor at outgoing direction.
                fresnel_transmittance_dielectric(fo, values->m_precomputed.m_eta, cos_on);
                fo = lerp(1.0f, fo, values->m_fresnel_weight);

                // Fresnel factor at incoming direction.
                fresnel_transmittance_dielectric(fi, values->m_precomputed.m_eta, cos_in);
                fi = lerp(1.0f, fi, values->m_fresnel_weight);
            }

            // Normalization constant.
            const float c = 1.0f - fresnel_first_moment_x2(values->m_precomputed.m_eta);

            return fo * fi / c;
        }

        void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3f&         incoming_dir,
            Spectrum&               value) const override
        {
            // Deterministic approach is not possible here.
            value.set(0.0f);
        }

        const BSDF*                     m_brdf;
        LambertianBRDFInputValues       m_brdf_data;
    };
}


//
// RandomWalkBSSRDFFactory class implementation.
//

void RandomWalkBSSRDFFactory::release()
{
    delete this;
}

const char* RandomWalkBSSRDFFactory::get_model() const
{
    return Model;
}

Dictionary RandomWalkBSSRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Random-Walk BSSRDF");
}

DictionaryArray RandomWalkBSSRDFFactory::get_input_metadata() const
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
        .insert("name", "zero_scattering_weight")
        .insert("label", "Zero Scattering Weight")
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

    return metadata;
}

auto_release_ptr<BSSRDF> RandomWalkBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new RandomWalkBSSRDF(name, params));
}

}   // namespace renderer
