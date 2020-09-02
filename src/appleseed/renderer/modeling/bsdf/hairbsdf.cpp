
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018-2019 Girish Ramesh, The appleseedhq Organization
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
#include "hairbsdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/fresnel.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/basis.h"
#include "foundation/math/specialfunctions.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <numeric>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;

namespace renderer
{

namespace
{

    //
    // Normalized logistic function over the range [-pi, pi].
    //

    inline float trimmed_logistic_func(const float value, const float scale)
    {
        const float num = logistic(value, scale);
        const float den = 1.0f - 2.0f * logistic_cdf(-Pi<float>(), scale);
        assert(den != 0);
        return num / den;
    }

    //
    // Sample trimmed logistic function over the range [-pi, pi]
    // using random sample in [0, 1].
    //

    inline float sample_trimmed_logistic(const float sample, const float scale)
    {
        const float trimmed_log_cdf = 1.0f - 2.0f * logistic_cdf(-Pi<float>(), scale);
        const float x = -scale * std::log(1.0f / (sample * trimmed_log_cdf + logistic_cdf(-Pi<float>(), scale)) - 1.0f);
        return clamp(x, -Pi<float>(), Pi<float>());
    }

    //
    // Computes the total azimuthal angle difference for each component of scattering.
    //

    inline float calc_phi(const int p, const float gamma_o, const float gamma_t)
    {
        return (2.0f * static_cast<float> (p) * gamma_t - 2.0f * gamma_o + static_cast<float> (p * Pi<float>()));
    }

    //
    // Wraps angle, in radians, to the interval [-pi, pi].
    //

    inline float wrap_to_pi(float angle)
    {
        while (angle > Pi<float>())
            angle -= TwoPi<float>();

        while (angle < -Pi<float>())
            angle += TwoPi<float>();

        return angle;
    }

    //
    // Longitudinal scattering function.
    //

    float longitudinal(
        const float cos_theta_i,
        const float cos_theta_o,
        const float sin_theta_i,
        const float sin_theta_o,
        const float v)
    {
        const float bessel_arg = cos_theta_i * cos_theta_o / v;
        const float exp_arg = sin_theta_i * sin_theta_o / v;

        // Low roughness values lead to precision issues
        // in the longitudinal integral.
        // https://publons.com/review/414383/

        if (v <= 0.1f)
        {
            return
                (std::exp(log_bessel(bessel_arg) -
                exp_arg - (1.0f / v) +
                0.6931f + std::log(1.0f / (2.0f * v))));
        }
        else
        {
            return
                (std::exp(-exp_arg) * bessel(bessel_arg)) /
                (std::sinh(1.0f / v) * 2.0f * v);
        }
    }

    //
    // Sampling function for longitudinal scattering.
    //

    float sample_longitudinal(const float sin_tilt, const float cos_tilt, const float v, const Vector2f sample_M)
    {
        const float cos_theta =
            1.0f + v * std::log(sample_M[0] +
            (1.0f - sample_M[0]) * std::exp(-2.0f / v));
        const float sin_theta = static_cast<float>(std::sqrt(std::max(0.0f, 1 - cos_theta * cos_theta)));
        const float cos_phi = std::cos(TwoPi<float>() * sample_M[1]);
        return (-cos_theta * sin_tilt + sin_theta * cos_phi * cos_tilt);
    }

    //
    // Accumulate sum of luminances for a Spectrum array.
    //

    static float sum_luminance(const std::array<Spectrum, 4>& retAp)
    {
        return std::accumulate(retAp.begin(), retAp.end(), 0.0f, [](float s, const Spectrum& ap)
        {
            return s + luminance(ap.illuminance_to_rgb(g_std_lighting_conditions));
        });
    }

    //
    // Attenuation function for each component of scattering.
    //

    void attenuation(
        const float                 cos_theta_o,
        const float                 eta,
        const float                 h,
        const Spectrum&             T,
        std::array<Spectrum, 4>&    ret)
    {

        // R (Primary Specular).
        const float cos_gamma_o = std::sqrt(std::max(0.0f, 1.0f - h * h));
        const float cos_theta = cos_gamma_o * cos_theta_o;
        float f;
        fresnel_reflectance_dielectric(f, 1.0f / eta, cos_theta);
        ret[0].set(f);

        Spectrum Tf = T;
        Tf *= f;

        Spectrum one_minus_Tf(1.0f);
        one_minus_Tf -= Tf;

        // TT (Transmitted component).
        ret[1] = T;
        ret[1] *= square(1.0f - f);

        // TRT (Total internally reflected component).
        ret[2] = ret[1];
        ret[2] *= Tf;

        // Higher orders of scattering.
        ret[3] = ret[2];
        ret[3] *= Tf;
        ret[3] /= one_minus_Tf;

    }

    //
    // Method to compute a discrete PDF based on the attenuation function.
    //

    void attenuation_pdf(
        const float             cos_theta_o,
        const float             eta,
        const float             h,
        const Spectrum&         T,
        std::array<float, 4>&   ret)
    {
        std::array<Spectrum, 4> retAp;
        attenuation(cos_theta_o, eta, h, T, retAp);

        const float sumY = sum_luminance(retAp);
        for (int i = 0; i <= 3; i++)
            ret[i] = luminance(retAp[i].illuminance_to_rgb(g_std_lighting_conditions)) / sumY;
    }

    //
    // Azimuthal scattering function.
    //

    inline float azimuthal(const float phi, const int p, const float s, const float gamma_o, const float gamma_t)
    {
        const float dphi = wrap_to_pi(phi - calc_phi(p, gamma_o, gamma_t));
        return trimmed_logistic_func(dphi, s);
    }

    //
    // BSDF computation function.
    //

    void calc_bsdf(
        const float                 cos_theta_i,
        const float                 sin_theta_i,
        const float                 cos_theta_o,
        const float                 sin_theta_o,
        const float                 cos_theta_o_R,
        const float                 sin_theta_o_R,
        const float                 cos_theta_o_TT,
        const float                 sin_theta_o_TT,
        const float                 cos_theta_o_TRT,
        const float                 sin_theta_o_TRT,
        const float                 v,
        const float                 dphi,
        const float                 s,
        const float                 gamma_o,
        const float                 gamma_t,
        std::array<Spectrum, 4>&    ap,
        Spectrum&                   bsdf_f)
    {

        // RR contribution of scattering.
        bsdf_f +=
            ap[0] *
            longitudinal(
                cos_theta_i,
                cos_theta_o_R,
                sin_theta_i,
                sin_theta_o_R,
                v) *
            azimuthal(dphi, 0, s, gamma_o, gamma_t);

        // TT contribution of scattering.
        bsdf_f +=
            ap[1] *
            longitudinal(
                cos_theta_i,
                cos_theta_o_TT,
                sin_theta_i,
                sin_theta_o_TT,
                v * 0.25f) *
            azimuthal(dphi, 1, s, gamma_o, gamma_t);


        // TRT contribution of scattering.
        bsdf_f +=
            ap[2] *
            longitudinal(
                cos_theta_i,
                cos_theta_o_TRT,
                sin_theta_i,
                sin_theta_o_TRT,
                v * 4.0f) *
            azimuthal(dphi, 2, s, gamma_o, gamma_t);

        // TRRT+ contribution of scattering.
        bsdf_f +=
            ap[3] *
            longitudinal(
                cos_theta_i,
                cos_theta_o,
                sin_theta_i,
                sin_theta_o,
                v * 4.0f) *
            RcpTwoPi<float>();


    }

    //
    // PDF computation function.
    //

    void calc_pdf(
        const float             cos_theta_i,
        const float             sin_theta_i,
        const float             cos_theta_o,
        const float             sin_theta_o,
        const float             cos_theta_o_R,
        const float             sin_theta_o_R,
        const float             cos_theta_o_TT,
        const float             sin_theta_o_TT,
        const float             cos_theta_o_TRT,
        const float             sin_theta_o_TRT,
        const float             v,
        const float             dphi,
        const float             s,
        const float             gamma_o,
        const float             gamma_t,
        std::array<float, 4>&   ap_pdf,
        float&                  bsdf_pdf)
    {

        // RR contribution of scattering.
        bsdf_pdf +=
            ap_pdf[0] *
            longitudinal(
                cos_theta_i,
                cos_theta_o_R,
                sin_theta_i,
                sin_theta_o_R,
                v) *
            azimuthal(dphi, 0, s, gamma_o, gamma_t);

        // TT contribution of scattering.
        bsdf_pdf +=
            ap_pdf[1] *
            longitudinal(
                cos_theta_i,
                cos_theta_o_TT,
                sin_theta_i,
                sin_theta_o_TT,
                v * 0.25f) *
            azimuthal(dphi, 1, s, gamma_o, gamma_t);


        // TRT contribution of scattering.
        bsdf_pdf +=
            ap_pdf[2] *
            longitudinal(
                cos_theta_i,
                cos_theta_o_TRT,
                sin_theta_i,
                sin_theta_o_TRT,
                v * 4.0f) *
            azimuthal(dphi, 2, s, gamma_o, gamma_t);

        // TRRT+ contribution of scattering.
        bsdf_pdf +=
            ap_pdf[3] *
            longitudinal(
                cos_theta_i,
                cos_theta_o,
                sin_theta_i,
                sin_theta_o,
                v * 4.0f) *
            RcpTwoPi<float>();
    }

    //
    // Computes absorption coefficient based on eumelanin and pheomelanin concentrations.
    //

    Spectrum sigma_a_from_melanin(float melanin_conc, const float melanin_ratio)
    {
        const float melanin = -std::log(std::max(1.0f - melanin_conc, 0.0001f));
        const float ce = melanin * (1.0f - melanin_ratio);
        const float cp = melanin * melanin_ratio;
        const Color3f eumelanin_sigma_a(0.506f, 0.841f, 1.653f);
        const Color3f pheomelanin_sigma_a(0.343f, 0.733f, 1.924f);
        const Color3f sigma_a = (ce * eumelanin_sigma_a + cp * pheomelanin_sigma_a);
        Spectrum ret;
        ret.set(sigma_a, g_std_lighting_conditions, Spectrum::Intent::Reflectance);
        return ret;
    }

    //
    // Computes absorption coefficient based on azimuthal roughness and surface reflectance.
    //

    Spectrum sigma_a_from_rgb(const Spectrum& reflectance, const float beta_n)
    {
        Spectrum sigma_a;
        for (std::size_t i = 0, e = reflectance.size(); i < e; ++i)
        {
            float s = std::log(reflectance[i]) /
                (5.969f - 0.215f * beta_n + 2.532f * beta_n * beta_n -
                    10.73f * pow_int(beta_n, 3) + 5.574f * pow_int(beta_n, 4) + 0.245f * pow_int(beta_n, 5));
            sigma_a[i] = s * s;
        }
        return sigma_a;
    }

    //
    // Hair BSDF.
    //
    // References:
    //
    //   [1] A Practical and Controllable Hair and Fur Model for Production Path Tracing.
    //       https://benedikt-bitterli.me/pchfm/pchfm.pdf
    //
    //   [2] The Implementation of a hair scattering model.
    //       http://www.pbrt.org/hair.pdf
    //
    //   [3] Importance Sampling for Physically-Based Hair Fiber Models.
    //       https://cg.ivd.kit.edu/publications/pubhanika/2013_hairbrief.pdf
    //
    //   [4] An Energy-Conserving Hair Reflectance Model.
    //       http://www.eugenedeon.com/wp-content/uploads/2014/04/egsrhair.pdf
    //

    namespace
    {
        const char* Model = "hair_bsdf";
    }

    class HairBSDFImpl
      : public BSDF
    {
      public:
        HairBSDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, AllBSDFTypes, ScatteringMode::Glossy, params)
        {
            m_inputs.declare("reflectance", InputFormat::SpectralReflectance);
            m_inputs.declare("melanin", InputFormat::Float, "0.3");
            m_inputs.declare("melanin_redness", InputFormat::Float, "0.0");
            m_inputs.declare("eta", InputFormat::Float, "1.55");
            m_inputs.declare("beta_M", InputFormat::Float, "0.3");
            m_inputs.declare("beta_N", InputFormat::Float, "0.3");
            m_inputs.declare("alpha", InputFormat::Float, "2.0");
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
            return sizeof(InputValues);
        }

        void prepare_inputs(
            Arena&                      arena,
            const ShadingPoint&         shading_point,
            void*                       data) const override
        {
            InputValues* values = static_cast<InputValues*>(data);

            new (&values->m_precomputed) InputValues::Precomputed();

            values->m_precomputed.m_h = -1.0f + 2.0f * shading_point.get_bary()[0];
            if (is_zero(values->m_reflectance))
                values->m_precomputed.m_sigma_a =
                sigma_a_from_melanin(values->m_melanin, values->m_melanin_redness);
            else
                values->m_precomputed.m_sigma_a =
                sigma_a_from_rgb(values->m_reflectance, values->m_beta_N);

            // Longitudinal roughness parameter.
            values->m_precomputed.m_v =
                0.726f * values->m_beta_M +
                0.812f * values->m_beta_M * values->m_beta_M +
                3.7f * pow_int(values->m_beta_M, 20);
            values->m_precomputed.m_v *= values->m_precomputed.m_v;

            // Scale factor to convert logistic function to gaussian.
            values->m_precomputed.m_s =
                0.626657069f * (0.265f * values->m_beta_N +
                1.194f * values->m_beta_N * values->m_beta_N +
                5.372f * pow_int(values->m_beta_N, 22));

            const float ang_in_radians = deg_to_rad<float>(values->m_alpha);
            values->m_precomputed.m_angle_R = -2.0f * ang_in_radians;
            values->m_precomputed.m_angle_TT = ang_in_radians;
            values->m_precomputed.m_angle_TRT = 4.0f * ang_in_radians;
        }

        void sample(
            SamplingContext&            sampling_context,
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Dual3f&               outgoing,
            const int                   modes,
            BSDFSample&                 sample) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return;

            const InputValues* values = static_cast<const InputValues*>(data);
            Vector3f out = outgoing.get_value();
            const Vector3f wo = local_geometry.m_shading_basis.transform_to_local(out);

            sampling_context.split_in_place(2, 1);
            Vector2f sample_M = sampling_context.next2<Vector2f>();
            sampling_context.split_in_place(2, 1);
            Vector2f sample_N = sampling_context.next2<Vector2f>();

            const float sin_theta_o = wo.x;
            const float cos_theta_o = std::sqrt(std::max(0.0f, 1.0f - sin_theta_o * sin_theta_o));
            const float phi_o = std::atan2(wo.y, wo.z);

            const float sin_theta_t = sin_theta_o / values->m_eta;
            const float cos_theta_t = std::sqrt(std::max(0.0f, 1.0f - sin_theta_t * sin_theta_t));

            const float eta_p = std::sqrt(values->m_eta * values->m_eta - sin_theta_o * sin_theta_o) / cos_theta_o;
            const float sin_gamma_t = values->m_precomputed.m_h / eta_p;
            const float cos_gamma_t = std::sqrt(std::max(0.0f, 1.0f - sin_gamma_t * sin_gamma_t));
            const float gamma_t = std::asin(clamp(sin_gamma_t, -1.0f, 1.0f));
            const float gamma_o = std::asin(clamp(values->m_precomputed.m_h, -1.0f, 1.0f));

            // Compute attenuation for the single path through the hair cylinder.
            const Spectrum T = exp(-values->m_precomputed.m_sigma_a * (( 2.0f * cos_gamma_t) / cos_theta_t));

            // Determine which scattering component to sample for hair scattering.
            std::array<Spectrum, 4> ap;
            attenuation(cos_theta_o, values->m_eta, values->m_precomputed.m_h, T, ap);
            std::array<float, 4> ap_pdf;
            attenuation_pdf(cos_theta_o, values->m_eta, values->m_precomputed.m_h, T, ap_pdf);
            int p;
            for (p = 0; p < 3; ++p)
            {
                if (sample_N[0] < ap_pdf[p])
                    break;
                sample_N[0] -= ap_pdf[p];
            }

            sample_M[0] = std::max(sample_M[0], float(1e-5));
            const float theta_o = std::asin(clamp(sin_theta_o, -1.0f, 1.0f));

            // Deviation of outgoing angle due to cuticular scales.
            const float theta_o_R = theta_o + values->m_precomputed.m_angle_R;
            const float theta_o_TT = theta_o + values->m_precomputed.m_angle_TT;
            const float theta_o_TRT = theta_o + values->m_precomputed.m_angle_TRT;

            // Sample longitudinal function for given component of scattering.

            float sin_theta_i;
            if (p == 0)
            {
                sin_theta_i = sample_longitudinal(
                    std::sin(theta_o_R),
                    std::cos(theta_o_R),
                    values->m_precomputed.m_v,
                    sample_M);
            }
            else if (p == 1)
            {
                sin_theta_i = sample_longitudinal(
                    std::sin(theta_o_TT),
                    std::cos(theta_o_TT),
                    values->m_precomputed.m_v * 0.25f,
                    sample_M);
            }
            else
            {
                sin_theta_i = sample_longitudinal(
                    std::sin(theta_o_TRT),
                    std::cos(theta_o_TRT),
                    values->m_precomputed.m_v * 4.0f,
                    sample_M);
            }
            const float cos_theta_i = std::sqrt(std::max(0.0f, 1.0f - sin_theta_i * sin_theta_i));

            // Sample azimuthal scattering function to compute change in azimuthal direction.
            float dphi;
            dphi = calc_phi(p, gamma_o, gamma_t) +
            sample_trimmed_logistic(sample_N[1], values->m_precomputed.m_s);

            // Compute wi from sampled hair scattering angles.
            const float phi_i = phi_o + dphi;
            const Vector3f wi = Vector3f(sin_theta_i, cos_theta_i * std::sin(phi_i), cos_theta_i * std::cos(phi_i));

            Spectrum bsdf_f(0.0f);
            float bsdf_pdf(0.0f);

            calc_bsdf(
                cos_theta_i,
                sin_theta_i,
                cos_theta_o,
                sin_theta_o,
                std::abs(std::cos(theta_o_R)),
                std::sin(theta_o_R),
                std::abs(std::cos(theta_o_TT)),
                std::sin(theta_o_TT),
                std::abs(std::cos(theta_o_TRT)),
                std::sin(theta_o_TRT),
                values->m_precomputed.m_v,
                dphi,
                values->m_precomputed.m_s,
                gamma_o,
                gamma_t,
                ap,
                bsdf_f);

            calc_pdf(
                cos_theta_i,
                sin_theta_i,
                cos_theta_o,
                sin_theta_o,
                std::abs(std::cos(theta_o_R)),
                std::sin(theta_o_R),
                std::abs(std::cos(theta_o_TT)),
                std::sin(theta_o_TT),
                std::abs(std::cos(theta_o_TRT)),
                std::sin(theta_o_TRT),
                values->m_precomputed.m_v,
                dphi,
                values->m_precomputed.m_s,
                gamma_o,
                gamma_t,
                ap_pdf,
                bsdf_pdf);

            const float angle_bsdf = std::abs(wi.y);

            if (angle_bsdf > 0)
                bsdf_f /= angle_bsdf;

            sample.set_to_scattering(ScatteringMode::Glossy, bsdf_pdf);

            sample.m_value.m_glossy = bsdf_f;
            sample.m_value.m_beauty = bsdf_f;

            sample.m_incoming = Dual3f(local_geometry.m_shading_basis.transform_to_parent(wi));
        }

        float evaluate(
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes,
            DirectShadingComponents&    value) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3f wo = local_geometry.m_shading_basis.transform_to_local(outgoing);
            const Vector3f wi = local_geometry.m_shading_basis.transform_to_local(incoming);

            // Compute outgoing terms.
            const float sin_theta_o = wo.x;
            const float cos_theta_o = std::sqrt(std::max(0.0f, 1.0f - sin_theta_o * sin_theta_o));
            const float phi_o = std::atan2(wo.y, wo.z);
            const float gamma_o = std::asin(clamp(values->m_precomputed.m_h, -1.0f, 1.0f));

            // Compute incoming terms.
            const float sin_theta_i = wi.x;
            const float cos_theta_i = std::sqrt(std::max(0.0f, 1.0f - sin_theta_i * sin_theta_i));
            const float phi_i = std::atan2(wi.y, wi.z);

            // Compute terms for the refracted ray.
            const float sin_theta_t = sin_theta_o / values->m_eta;
            const float cos_theta_t = std::sqrt(std::max(0.0f, 1.0f - sin_theta_t * sin_theta_t));

            // Compute the modified refraction coefficient.
            const float eta_p = std::sqrt(values->m_eta * values->m_eta - sin_theta_o * sin_theta_o) / cos_theta_o;
            const float sin_gamma_t = values->m_precomputed.m_h / eta_p;
            const float cos_gamma_t = std::sqrt(std::max(0.0f, 1.0f - sin_gamma_t * sin_gamma_t));
            const float gamma_t = std::asin(clamp(sin_gamma_t, -1.0f, 1.0f));
            const float theta_o = std::asin(clamp(sin_theta_o, -1.0f, 1.0f));

            // Deviation of outgoing angle due to cuticular scales.
            const float theta_o_R = theta_o + values->m_precomputed.m_angle_R;
            const float theta_o_TT = theta_o + values->m_precomputed.m_angle_TT;
            const float theta_o_TRT = theta_o + values->m_precomputed.m_angle_TRT;

            // Compute attenuation for the single path through the hair cylinder.
            const Spectrum T = exp(-values->m_precomputed.m_sigma_a * ((2.0f * cos_gamma_t) / cos_theta_t));

            const float dphi = phi_i - phi_o;
            std::array<Spectrum, 4> ap;
            attenuation(cos_theta_o, values->m_eta, values->m_precomputed.m_h, T, ap);
            std::array<float, 4> ap_pdf;
            attenuation_pdf(cos_theta_o, values->m_eta, values->m_precomputed.m_h, T, ap_pdf);

            Spectrum bsdf_f(0.0f);
            float bsdf_pdf = 0.0f;

            calc_bsdf(
                cos_theta_i,
                sin_theta_i,
                cos_theta_o,
                sin_theta_o,
                std::abs(std::cos(theta_o_R)),
                std::sin(theta_o_R),
                std::abs(std::cos(theta_o_TT)),
                std::sin(theta_o_TT),
                std::abs(std::cos(theta_o_TRT)),
                std::sin(theta_o_TRT),
                values->m_precomputed.m_v,
                dphi,
                values->m_precomputed.m_s,
                gamma_o,
                gamma_t,
                ap,
                bsdf_f);

            calc_pdf(
                cos_theta_i,
                sin_theta_i,
                cos_theta_o,
                sin_theta_o,
                std::abs(std::cos(theta_o_R)),
                std::sin(theta_o_R),
                std::abs(std::cos(theta_o_TT)),
                std::sin(theta_o_TT),
                std::abs(std::cos(theta_o_TRT)),
                std::sin(theta_o_TRT),
                values->m_precomputed.m_v,
                dphi,
                values->m_precomputed.m_s,
                gamma_o,
                gamma_t,
                ap_pdf,
                bsdf_pdf);

            const float angle_bsdf = std::abs(wi.y);

            if (angle_bsdf > 0)
                bsdf_f /= angle_bsdf;

            value.m_glossy = bsdf_f;
            value.m_beauty = bsdf_f;

            return bsdf_pdf;
        }

        float evaluate_pdf(
            const void*                 data,
            const bool                  adjoint,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3f wo = local_geometry.m_shading_basis.transform_to_local(outgoing);
            const Vector3f wi = local_geometry.m_shading_basis.transform_to_local(incoming);

            // Compute outgoing terms.
            const float sin_theta_o = wo.x;
            const float cos_theta_o = std::sqrt(std::max(0.0f, 1.0f - sin_theta_o * sin_theta_o));
            const float phi_o = std::atan2(wo.y, wo.z);
            const float gamma_o = std::asin(clamp(values->m_precomputed.m_h, -1.0f, 1.0f));

            // Compute incoming terms.
            const float sin_theta_i = wi.x;
            const float cos_theta_i = std::sqrt(std::max(0.0f, 1.0f - sin_theta_i * sin_theta_i));
            const float phi_i = std::atan2(wi.y, wi.z);

            // Compute terms for the refracted ray.
            const float sin_theta_t = sin_theta_o / values->m_eta;
            const float cos_theta_t = std::sqrt(std::max(0.0f, 1.0f - sin_theta_t * sin_theta_t));

            // Compute the modified refraction coefficient.
            const float eta_p = std::sqrt(values->m_eta * values->m_eta - sin_theta_o * sin_theta_o) / cos_theta_o;
            const float sin_gamma_t = values->m_precomputed.m_h / eta_p;
            const float cos_gamma_t = std::sqrt(std::max(0.0f, 1.0f - sin_gamma_t * sin_gamma_t));
            const float gamma_t = std::asin(clamp(sin_gamma_t, -1.0f, 1.0f));
            const float theta_o = std::asin(clamp(sin_theta_o, -1.0f, 1.0f));

            // Deviation of outgoing angle due to cuticular scales.
            const float theta_o_R = theta_o + values->m_precomputed.m_angle_R;
            const float theta_o_TT = theta_o + values->m_precomputed.m_angle_TT;
            const float theta_o_TRT = theta_o + values->m_precomputed.m_angle_TRT;

            // Compute attenuation for the single path through the hair cylinder.
            const Spectrum T = exp(-values->m_precomputed.m_sigma_a * ((2.0f * cos_gamma_t) / cos_theta_t));

            const float dphi = phi_i - phi_o;
            std::array<float, 4> ap_pdf;
            attenuation_pdf(cos_theta_o, values->m_eta, values->m_precomputed.m_h, T, ap_pdf);

            float bsdf_pdf = 0.0f;

            calc_pdf(
                cos_theta_i,
                sin_theta_i,
                cos_theta_o,
                sin_theta_o,
                std::abs(std::cos(theta_o_R)),
                std::sin(theta_o_R),
                std::abs(std::cos(theta_o_TT)),
                std::sin(theta_o_TT),
                std::abs(std::cos(theta_o_TRT)),
                std::sin(theta_o_TRT),
                values->m_precomputed.m_v,
                dphi,
                values->m_precomputed.m_s,
                gamma_o,
                gamma_t,
                ap_pdf,
                bsdf_pdf);

            return bsdf_pdf;
        }


    private:
        typedef HairBSDFInputValues InputValues;
    };

    typedef BSDFWrapper<HairBSDFImpl> HairBSDF;
}


//
// HairBSDFFactory class implementation.
//

void HairBSDFFactory::release()
{
    delete this;
}

const char* HairBSDFFactory::get_model() const
{
    return Model;
}

Dictionary HairBSDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Hair BSDF");
}

DictionaryArray HairBSDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance")
            .insert("label", "Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                    Dictionary()
                        .insert("color", "Colors")
                        .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.0"));


    metadata.push_back(
        Dictionary()
            .insert("name", "melanin")
            .insert("label", "Melanin Concentration")
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
            .insert("default", "0.3"));

    metadata.push_back(
        Dictionary()
            .insert("name", "melanin_redness")
            .insert("label", "Melanin Redness")
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
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "eta")
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
            .insert("use", "optional")
            .insert("default", "1.55"));

    metadata.push_back(
        Dictionary()
            .insert("name", "beta_M")
            .insert("label", "Longitudinal Roughness")
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
            .insert("default", "0.3"));

    metadata.push_back(
        Dictionary()
            .insert("name", "beta_N")
            .insert("label", "Azimuthal Roughness")
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
            .insert("default", "0.3"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha")
            .insert("label", "Cuticle Scale Angle")
            .insert("type", "numeric")
            .insert("min",
                    Dictionary()
                        .insert("value", "0.0")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "5.0")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "2.0"));

    return metadata;
}

auto_release_ptr<BSDF> HairBSDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new HairBSDF(name, params));
}

}   // namespace renderer
