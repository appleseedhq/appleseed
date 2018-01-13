
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
                // Compute single-scattering albedo from multiple-scattering albedo.
                const float albedo = albedo_from_reflectance(values->m_reflectance[i]);

                // Compute extinction coefficient.
                const float s = normalized_diffusion_s_mfp(values->m_reflectance[i]);
                const float extinction = s / values->m_mfp[i];

                precomputed.m_albedo[i] = albedo;
                precomputed.m_extinction[i] = extinction;

                // Compute diffusion length, required by Dwivedi sampling.
                const float kappa = min(compute_rcp_diffusion_length(albedo), 0.99f);
                precomputed.m_rcp_diffusion_length[i] = kappa;
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
            const Spectrum& rcp_diffusion_length = values->m_precomputed.m_rcp_diffusion_length;

            // Compute the probability of classical sampling.
            // The probability of classical sampling is high if phase function is anisotropic.
            const float classical_sampling_prob =
                UseDwivediSampling ? compute_classical_sampling_probability(0.0f) : 1.0f;

            // Initialize BSSRDF value.
            bssrdf_sample.m_value.set(values->m_weight);
            bssrdf_sample.m_probability = 1.0f;

            // Pick initial random-walk direction uniformly.
            sampling_context.split_in_place(2, 1);
            Vector3d initial_dir = sample_hemisphere_uniform(sampling_context.next2<Vector2d>());
            initial_dir.y = -initial_dir.y;
            initial_dir = outgoing_point.get_shading_basis().transform_to_parent(initial_dir);
            ShadingRay ray = ShadingRay(
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
            const float weight = compute_transmission_classical_sampling(
                transmitted ? ray_length : distance,
                extinction,
                channel_pdf,
                transmitted,
                bssrdf_sample.m_value);
            bssrdf_sample.m_value *= rcp(weight);

            // Determine the closest surface point and corresponding slab normal.
            const bool outgoing_point_is_closer = distance < 0.5f * ray_length;
            const Vector3f near_slab_normal = Vector3f(outgoing_point.get_geometric_normal());
            const Vector3f far_slab_normal = Vector3f(-bssrdf_sample.m_incoming_point.get_geometric_normal());
            const Vector3f& slab_normal = outgoing_point_is_closer ? far_slab_normal : near_slab_normal;
            Vector3d current_point = ray.point_at(distance);

            // Continue random walk until we reach the surface from inside.
            int n_iteration = 0;
            const int MaxIterationsCount = 64;
            const int MinRRIteration = 4;
            while (!transmitted)
            {
                if (++n_iteration > MaxIterationsCount)
                    return false;   // path got lost inside the object

                if (n_iteration > MinRRIteration && !test_rr(sampling_context, bssrdf_sample))
                    return false;   // sample has not passed Rusian Roulette test

                sampling_context.split_in_place(1, 2);

                // Choose color channel used for distance sampling.
                Spectrum channel_cdf;
                build_cdf_and_pdf(bssrdf_sample.m_value, channel_cdf, channel_pdf);
                const size_t channel = sample_cdf(
                    &channel_cdf[0],
                    &channel_cdf[0] + channel_cdf.size(),
                    sampling_context.next2<float>());
                if (albedo[channel] == 0.0f || bssrdf_sample.m_value[channel] == 0.0f)
                    return false; // path got lost inside the object

                // Determine if we do biased (Dwivedi) sampling or classical sampling.
                bool is_biased = classical_sampling_prob < sampling_context.next2<float>();

                // Find next random-walk direction.
                sampling_context.split_in_place(3, 1);
                const Vector3f s = sampling_context.next2<Vector3f>();
                Vector3f incoming;
                float cosine;
                if (is_biased)
                {
                    cosine = sample_cosine_dwivedi(rcp(rcp_diffusion_length[channel]), s[0]);
                    incoming = sample_direction_given_cosine(slab_normal, cosine, s[1]);
                }
                else
                {
                    incoming = sample_sphere_uniform(Vector2f(s[0], s[1]));
                    cosine = dot(incoming, slab_normal);
                }

                // Update transmission by the albedo.
                bssrdf_sample.m_value *= albedo;

                // Construct a new ray in the sampled direction.
                ray = ShadingRay(
                    current_point,
                    Vector3d(incoming),
                    outgoing_point.get_time(),
                    VisibilityFlags::ShadowRay,
                    ray.m_depth + 1
                );

                // Sample distance assuming that the ray is infinite.
                const float effective_extinction = is_biased ?
                    extinction[channel] * (1.0f - cosine * rcp_diffusion_length[channel]) :
                    extinction[channel];
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
                const float weight_classical = compute_transmission_classical_sampling(
                    distance,
                    extinction,
                    channel_pdf,
                    transmitted,
                    transmission);
                if (UseDwivediSampling)
                {
                    const float weight_dwivedi = compute_mis_dwivedi_sampling(
                        distance,
                        extinction,
                        rcp_diffusion_length,
                        cosine,
                        channel_pdf,
                        transmitted);
                    transmission *= rcp(mix(
                        weight_dwivedi,
                        weight_classical,
                        classical_sampling_prob));
                }
                else
                    transmission *= rcp(weight_classical);
                bssrdf_sample.m_value *= transmission;
            }

            if (UseDwivediSampling)
            {
                // This is an ugly way to get rid of fireflies
                // that happen on some long paths when the biased sampling is on.
                clamp_in_place(bssrdf_sample.m_value, 0.0f, 3.0f);
                // TODO: Find the actual reason of the fireflies.
            }

            if (n_iteration == 1)
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

        //
        // The functions below use one-sample estimator to implement spectral MIS (Veach: 9.2.4 eq. 9.15).
        // They return a weight of the form
        //    W = c1 * p1 + c2 * p2 + ... + cN * pN
        // where:
        //    N is a number of channels,
        //    ci is a probability to pick i-th estimator from the set of N given estimators,
        //    pi is a PDF value of the given sample if i-th estimator is picked.
        // Note that different weights W can be mixed for different sets of estimators,
        // and f(X) / W is the MIS estimator for the function f(X) being integrated.
        //

        static float compute_transmission_classical_sampling(
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

                mis_base += transmission[i] * channel_pdf[i];
            }
            return mis_base;
        }

        static float compute_mis_dwivedi_sampling(
            const float         distance,
            const Spectrum&     extinction,
            const Spectrum&     rcp_diffusion_length,
            const float         cosine,
            const Spectrum&     channel_pdf,
            const bool          transmitted)
        {
            float mis_base = 0.0f;
            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                const float effective_extinction = extinction[i] * (1.0f - cosine * rcp_diffusion_length[i]);
                const float x = -distance * effective_extinction;
                assert(FP<float>::is_finite(x));
                const float distance_prob = (transmitted ? 1.0f : effective_extinction) * std::exp(x);
                const float direction_prob = evaluate_cosine_dwivedi(rcp(rcp_diffusion_length[i]), cosine);

                mis_base += distance_prob * channel_pdf[i] * direction_prob;
            }
            // When we sample direction, we do not divide the value by normalization cofficient 4pi (sphere),
            // therefore here we need to multiply direction probabilities by 4pi and divide it by 2pi (circle).
            return 2.0f * mis_base;
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
            const RandomWalkBSSRDFInputValues* values =
                static_cast<const RandomWalkBSSRDFInputValues*>(data);

            throw ExceptionNotImplemented();  // deterministic approach is not possible here
        }

        const BSDF*                     m_brdf;
        LambertianBRDFInputValues       m_brdf_data;

        // Experimental:
        const bool                      UseDwivediSampling = false;
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
