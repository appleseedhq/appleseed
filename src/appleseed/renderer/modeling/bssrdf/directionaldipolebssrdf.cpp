
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "directionaldipolebssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// standard headers.
#include <algorithm>

using namespace foundation;

namespace renderer
{

namespace
{

    const char* Model = "directional_dipole_bssrdf";

    //
    // Directional dipole BSSRDF.
    //

    //
    // references:
    //
    // [1] Directional Dipole for Subsurface Scattering
    // Jeppe Revall Frisvad, Toshiya Hachisuka and Thomas Kim Kjeldsen.
    // http://www.ci.i.u-tokyo.ac.jp/~hachisuka/dirpole.pdf
    //
    // [2] Texture mapping for the Better Dipole model
    // Christophe Hery
    // http://graphics.pixar.com/library/TexturingBetterDipole/paper.pdf
    //

    class DirectionalDipoleBSSRDF
      : public BSSRDF
    {
      public:
        DirectionalDipoleBSSRDF(
            const char*                 name,
            const ParamArray&           params)
          : BSSRDF(name, params)
        {
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("mean_free_path", InputFormatSpectralReflectance);
            m_inputs.declare("mean_free_path_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("anisotropy", InputFormatScalar);
            m_inputs.declare("from_ior", InputFormatScalar);
            m_inputs.declare("to_ior", InputFormatScalar);
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual size_t compute_input_data_size(
            const Assembly&             assembly) const
        {
            return align(sizeof(DirectionalDipoleBSSRDFInputValues), 16);
        }

        virtual void evaluate_inputs(
            const ShadingContext&       shading_context,
            InputEvaluator&             input_evaluator,
            const ShadingPoint&         shading_point,
            const size_t                offset = 0) const APPLESEED_OVERRIDE
        {
            BSSRDF::evaluate_inputs(shading_context, input_evaluator, shading_point, offset);

            char* ptr = reinterpret_cast<char*>(input_evaluator.data());
            DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<DirectionalDipoleBSSRDFInputValues*>(ptr + offset);

            values->m_mean_free_path *= static_cast<float>(values->m_mean_free_path_multiplier);

            if (values->m_reflectance.size() != values->m_mean_free_path.size())
            {
                if (values->m_reflectance.is_spectral())
                    Spectrum::upgrade(values->m_mean_free_path, values->m_mean_free_path);
                else
                    values->m_mean_free_path.convert_to_rgb(*m_lighting_conditions);
            }

            // Convert values and precompute stuff.
            values->m_channel_cdf.set(0.0f);
            double sum_alpha_prime = 0.0;
            values->m_max_mean_free_path = 0.0;

            const double eta = values->m_to_ior / values->m_from_ior;
            const double c1 = fresnel_moment_1(eta);
            const double c2 = fresnel_moment_2(eta);

            for (size_t i = 0, e = values->m_reflectance.size(); i < e; ++i)
            {
                const double alpha_prime = compute_alpha_prime(
                    clamp(static_cast<double>(values->m_reflectance[i]), 0.0, 1.0), c1, c2);

                sum_alpha_prime += alpha_prime;
                values->m_channel_weights[i] = alpha_prime;
                values->m_channel_cdf[i] = sum_alpha_prime;
                const double mfp = static_cast<double>(values->m_mean_free_path[i]);
                values->m_max_mean_free_path = std::max(values->m_max_mean_free_path, mfp);

                const double sigma_tr = 1.0 / mfp;
                const double sigma_t_prime = sigma_tr / std::sqrt( 3.0 * (1.0 - alpha_prime));

                values->m_sigma_s_prime[i] = alpha_prime * sigma_t_prime;
                values->m_sigma_a[i] = sigma_t_prime - values->m_sigma_s_prime[i];
            }

            if (sum_alpha_prime > 0.0)
            {
                const float rcp_sum_alpha_prime = static_cast<float>(1.0 / sum_alpha_prime);
                values->m_channel_weights *= rcp_sum_alpha_prime;
                values->m_channel_cdf *= rcp_sum_alpha_prime;
            }
        }

        virtual void evaluate(
            const void*                 data,
            const ShadingPoint&         outgoing_point,
            const Vector3d&             outgoing_dir,
            const ShadingPoint&         incoming_point,
            const Vector3d&             incoming_dir,
            Spectrum&                   value) const APPLESEED_OVERRIDE
        {
            const DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<const DirectionalDipoleBSSRDFInputValues*>(data);

            value.resize(values->m_sigma_a.size());
            value.set(0.0f);

            bssrdf(
                values,
                incoming_point.get_point(),
                incoming_point.get_shading_normal(),
                incoming_dir,
                outgoing_point.get_point(),
                outgoing_point.get_shading_normal(),
                value);

            // Reciprocal evaluation with the reciprocity hack.
            // Not sure we want it.
            /*
            bssrdf(
                values,
                outgoing_point.get_point(),
                outgoing_point.get_shading_normal(),
                outgoing_dir,
                incoming_point.get_point(),
                incoming_point.get_shading_normal(),
                value);

            value *= 0.5f;
            */
        }

      private:
        virtual Vector2d sample(
            const void*     data,
            const Vector3d& r,
            size_t&         ch) const APPLESEED_OVERRIDE
        {
            const DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<const DirectionalDipoleBSSRDFInputValues*>(data);

            // Sample a color channel.
            const float* cdf = &(values->m_channel_cdf[0]);
            ch = std::upper_bound(
                cdf,
                cdf + values->m_channel_cdf.size(),
                r[0]) - cdf;

            // Sample a radius and an angle.
            const double radius = -std::log(1.0 - r[1]) * values->m_mean_free_path[ch];
            const double phi = 2.0 * Pi * r[2];

            return Vector2d(
                radius * std::cos(phi),
                radius * std::sin(phi));
        }

        virtual double pdf(
            const void*     data,
            const size_t    channel,
            const double    dist) const APPLESEED_OVERRIDE
        {
            const DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<const DirectionalDipoleBSSRDFInputValues*>(data);

            const double s = 1.0 / values->m_mean_free_path[channel];
            return s * std::exp(-s * dist) * values->m_channel_weights[channel];
        }

        static double compute_rd(double alpha_prime, double two_c1, double three_c2)
        {
            double cphi = 0.25 * (1.0 - two_c1);
            double ce = 0.5 * (1.0 - three_c2);
            double mu_tr_D = std::sqrt((1.0 - alpha_prime) * (2.0 - alpha_prime) / 3.0);
            double myexp = std::exp(-((1.0 + three_c2) / cphi) * mu_tr_D);
            return 0.5 * square(alpha_prime) * std::exp(-std::sqrt(3.0 * (1.0 - alpha_prime) / (2.0 - alpha_prime))) * (ce * (1.0 + myexp) + cphi / mu_tr_D * (1.0 - myexp));
        }

        static double compute_alpha_prime(double rd, double c1, double c2)
        {
            const double c12 = 2.0 * c1;
            const double c23 = 3.0 * c2;

            double x0 = 0.0, x1 = 1.0, xmid;

            // For now simple bisection.
            for (size_t i = 0, iters = 50; i < iters; ++i)
            {
                xmid = 0.5 * (x0 + x1);
                const double f = compute_rd(xmid, c12, c23);
                f < rd ? x0 = xmid : x1 = xmid;
            }

            return xmid;
        }

        static double sp_d(
            const double    sigma_a,
            const double    sigma_s_prime,
            const double    eta,
            const double    dot_xw,
            const double    dot_wn,
            const double    dot_xn,
            const double    r)
        {
            const double cp = (1.0 - 2.0 * fresnel_moment_1(eta)) * 0.25;
            const double ce = (1.0 - 3.0 * fresnel_moment_2(eta)) * 0.5;
            const double cp_norm = 1.0 / (1.0 - 2.0 * fresnel_moment_1(1.0 / eta));

            const double r2 = square(r);
            const double r3_rcp = 1.0 / (r2 * r);

            const double sigma_t_prime = sigma_a + sigma_s_prime;
            const double D = 1.0 / (3.0 * sigma_t_prime);
            const double sigma_tr = std::sqrt(sigma_a / D);
            const double s_tr_r = sigma_tr * r;
            const double s_tr_r_one = 1.0 + s_tr_r;

            const double four_pisq_rcp = 1.0 / (4.0 * square(Pi));
            const double t0 = cp_norm * four_pisq_rcp * std::exp(-s_tr_r) * r3_rcp;
            const double t1 = r2 / D + 3.0 * s_tr_r_one * dot_xw;
            const double t2 = 3.0 * D * s_tr_r_one * dot_wn;
            const double t3 = (s_tr_r_one + 3.0 * D * (3.0 * s_tr_r_one + square(s_tr_r)) / r2 * dot_xw) * dot_xn;
            return t0 * (cp * t1 - ce * (t2 - t3));
        }

        static void bssrdf(
            const DirectionalDipoleBSSRDFInputValues*   values,
            const Vector3d&                             xi,
            const Vector3d&                             ni,
            const Vector3d&                             wi,
            const Vector3d&                             xo,
            const Vector3d&                             no,
            Spectrum&                                   result)
        {
            // Distance.
            const Vector3d xoxi = xo - xi;
            const double r = norm(xoxi);
            const double r2 = square(r);

            // Modified normal.
            const Vector3d ni_s = cross(normalize(xoxi), normalize(cross(ni, xoxi)));

            // Directions of ray sources.
            const double eta = values->m_to_ior / values->m_from_ior;
            const double nnt = 1.0 / eta;
            const double ddn = -dot(wi, ni);
            const Vector3d wr = normalize(wi * -nnt - ni * (ddn * nnt + std::sqrt(1.0 - nnt * nnt * (1.0 - ddn * ddn))));
            const Vector3d wv = wr - ni_s * (2.0 * dot(ni_s, wr));

            const double cp = (1.0 - 2.0 * fresnel_moment_1(eta)) * 0.25;
            const double ce = (1.0 - 3.0 * fresnel_moment_2(eta)) * 0.5;
            const double A = (1.0 - ce) / (2.0 * cp);

            const double dot_xiwr = dot(xoxi, wr);
            const double dot_wrn = dot(wr, no);
            const double dot_xin = dot(xoxi, no);
            const double dot_wvn = dot(wv, no);

            for (size_t i = 0, e = result.size(); i < e; ++i)
            {
                const double sigma_a = values->m_sigma_a[i];
                const double sigma_s_prime = values->m_sigma_s_prime[i];
                const double sigma_t_prime = sigma_a + sigma_s_prime;
                const double D = 1.0 / (3.0 * sigma_t_prime);
                const double alpha_prime = sigma_s_prime / sigma_t_prime;
                const double de = 2.131 * D / std::sqrt(alpha_prime);

                const double sigma_s = sigma_s_prime / (1.0 - values->m_anisotropy);
                const double sigma_t = sigma_a + sigma_s;

                // Distance to real sources.
                const double cos_beta = -std::sqrt((r2 - square(dot(wr, xoxi))) / (r2 + square(de)));

                double dr;

                if (dot_wrn > 0.0)
                    dr = std::sqrt((D * dot_wrn) * ((D * dot_wrn) - de * cos_beta * 2.0) + r2);
                else
                    dr = std::sqrt(1.0 / square(3.0 * sigma_t) + r2);

                // Distance to virtual source.
                const Vector3d xoxv = xo - (xi + ni_s * (2.0 * A * de));
                const double dv = norm(xoxv);

                const double sp_i = sp_d(
                    sigma_a,
                    sigma_s_prime,
                    eta,
                    dot_xiwr,
                    dot_wrn,
                    dot_xin,
                    dr);

                const double sp_v = sp_d(
                    sigma_a,
                    sigma_s_prime,
                    eta,
                    dot(xoxv, wv),
                    dot_wvn,
                    dot(xoxv, no),
                    dv);

                // Clamp negative values to zero.
                result[i] += std::max(sp_i - sp_v, 0.0);
            }
        }
    };
}

//
// DirectionalDipoleBSSRDFFactory class implementation.
//

const char* DirectionalDipoleBSSRDFFactory::get_model() const
{
    return Model;
}

Dictionary DirectionalDipoleBSSRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Directional Dipole BSSRDF");
}

DictionaryArray DirectionalDipoleBSSRDFFactory::get_input_metadata() const
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
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mean_free_path")
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
            .insert("name", "mean_free_path_multiplier")
            .insert("label", "Mean Free Path Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropy")
            .insert("label", "Anisotropy")
            .insert("type", "numeric")
            .insert("min_value", "-1.0")
            .insert("max_value", "1.0")
            .insert("use", "required")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "from_ior")
            .insert("label", "From Index of Refraction")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "5.0")
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "to_ior")
            .insert("label", "To Index of Refraction")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "5.0")
            .insert("use", "required")
            .insert("default", "1.3"));

    return metadata;
}

auto_release_ptr<BSSRDF> DirectionalDipoleBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new DirectionalDipoleBSSRDF(name, params));
}

}   // namespace renderer
