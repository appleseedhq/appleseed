
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
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class ShadingContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Directional dipole BSSRDF.
    //
    // Reference:
    //
    //   Directional Dipole for Subsurface Scattering
    //   Jeppe Revall Frisvad, Toshiya Hachisuka and Thomas Kim Kjeldsen.
    //   http://www.ci.i.u-tokyo.ac.jp/~hachisuka/dirpole.pdf
    //

    const char* Model = "directional_dipole_bssrdf";

    class DirectionalDipoleBSSRDF
      : public BSSRDF
    {
      public:
        DirectionalDipoleBSSRDF(
            const char*             name,
            const ParamArray&       params)
          : BSSRDF(name, params)
        {
            m_inputs.declare("weight", InputFormatScalar, "1.0");
            m_inputs.declare("sigma_a", InputFormatSpectralReflectance);
            m_inputs.declare("sigma_a_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("sigma_s", InputFormatSpectralReflectance);
            m_inputs.declare("sigma_s_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("anisotropy", InputFormatScalar);
            m_inputs.declare("outside_ior", InputFormatScalar);
            m_inputs.declare("inside_ior", InputFormatScalar);
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
            const Assembly&         assembly) const APPLESEED_OVERRIDE
        {
            return align(sizeof(DirectionalDipoleBSSRDFInputValues), 16);
        }

        virtual void evaluate_inputs(
            const ShadingContext&   shading_context,
            InputEvaluator&         input_evaluator,
            const ShadingPoint&     shading_point,
            const size_t            offset = 0) const APPLESEED_OVERRIDE
        {
            BSSRDF::evaluate_inputs(shading_context, input_evaluator, shading_point, offset);

            char* ptr = reinterpret_cast<char*>(input_evaluator.data());
            DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<DirectionalDipoleBSSRDFInputValues*>(ptr + offset);

            assert(values->m_sigma_a.size() == values->m_sigma_s.size());

            /*
            values->m_reflectance *= static_cast<float>(values->m_reflectance_multiplier);
            values->m_dmfp *= static_cast<float>(values->m_dmfp_multiplier);

            if (values->m_dmfp.size() != values->m_reflectance.size())
            {
                if (values->m_dmfp.is_spectral())
                    Spectrum::upgrade(values->m_reflectance, values->m_reflectance);
                else
                    values->m_reflectance = values->m_reflectance.convert_to_rgb(*m_lighting_conditions);
            }

            values->m_sigma_s.resize(values->m_dmfp.size());
            values->m_sigma_a.resize(values->m_dmfp.size());

            // Relative refractive index.
            const double eta = values->m_inside_ior / values->m_outside_ior;
            const double rcp_g_complent = 1.0 / (1.0 - values->m_anisotropy);

            for (size_t i = 0, e = values->m_reflectance.size(); i < e; ++i)
            {
                // Input values: diffuse reflectance and diffuse mean free path.
                const double rd = saturate(static_cast<double>(values->m_reflectance[i]));
                const double dmfp = static_cast<double>(values->m_dmfp[i]);

                // Find alpha' by numerically inverting Rd(alpha').
                const double alpha_prime =
                    compute_alpha_prime(
                        ComputeRdBetterDipole(eta),
                        rd);

                const double sigma_t_prime =
                    reduced_extinction_coefficient(dmfp, alpha_prime);

                // Compute scattering coefficient.
                const double sigma_s_prime = static_cast<float>(alpha_prime * sigma_t_prime);
                values->m_sigma_s[i] = sigma_s_prime * rcp_g_complent;

                // Compute absorption coefficient.
                values->m_sigma_a[i] = static_cast<float>(sigma_t_prime) - sigma_s_prime;
            }
            */
        }

        virtual void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3d&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3d&         incoming_dir,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<const DirectionalDipoleBSSRDFInputValues*>(data);

            value.resize(values->m_sigma_a.size());
            bssrdf(
                values,
                incoming_point.get_point(),
                incoming_point.get_shading_normal(),
                incoming_dir,
                outgoing_point.get_point(),
                outgoing_point.get_shading_normal(),
                value);

            // Hack to make the BSSRDF reciprocal (section 6.3).
#if 0
            Spectrum tmp;
            tmp.resize(values->m_sigma_a.size());
            bssrdf(
                values,
                outgoing_point.get_point(),
                outgoing_point.get_shading_normal(),
                outgoing_dir,
                incoming_point.get_point(),
                incoming_point.get_shading_normal(),
                tmp);
                value += tmp;
                value *= 0.5f;
#endif

            value *= static_cast<float>(values->m_weight);
        }

      private:
        virtual bool do_sample(
            const void*             data,
            BSSRDFSample&           sample,
            Vector2d&               point) const APPLESEED_OVERRIDE
        {
            const DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<const DirectionalDipoleBSSRDFInputValues*>(data);

            if (values->m_weight == 0.0)
                return false;

            sample.set_is_directional(true);
            sample.set_eta(values->m_inside_ior / values->m_outside_ior);

            sample.get_sampling_context().split_in_place(3, 1);
            const Vector3d s = sample.get_sampling_context().next_vector2<3>();

            // Sample a color channel uniformly.
            const size_t channel = truncate<size_t>(s[0] * values->m_sigma_a.size());
            sample.set_channel(channel);

            // Sample a radius by importance sampling the attenuation.
            // Volumetric Path Tracing, Steve Marschner, section 3.
            // http://www.cs.cornell.edu/courses/cs6630/2012sp/notes/09volpath.pdf
            const double sigma_t = values->m_sigma_a[channel] + values->m_sigma_s[channel];
            const double radius = -log(1.0 - s[1]) / sigma_t;

            // Sample an angle.
            const double phi = TwoPi * s[2];

            // Return point on disk.
            point = Vector2d(radius * cos(phi), radius * sin(phi));

            return true;
        }

        virtual double do_pdf(
            const void*             data,
            const size_t            channel,
            const double            dist) const APPLESEED_OVERRIDE
        {
            const DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<const DirectionalDipoleBSSRDFInputValues*>(data);

            // PDF of the sampled channel.
            const double pdf_channel = 1.0 / values->m_sigma_a.size();

            // PDF of the sampled radius.
            const double sigma_t = values->m_sigma_a[channel] + values->m_sigma_s[channel];
            const double pdf_radius = sigma_t * exp(-sigma_t * dist);

            // PDF of the sampled angle.
            const double pdf_angle = RcpTwoPi;

            // Compute and return the final PDF.
            return pdf_channel * pdf_radius * pdf_angle;
        }

        // Diffusive part of the BSSRDF.
        static double sd_prime(
            const double            eta,
            const double            cp,
            const double            ce,
            const double            D,
            const double            sigma_a,
            const double            dot_xw,
            const double            dot_wn,
            const double            dot_xn,
            const double            r)                                                  // distance from point of incidence
        {
            const double sigma_tr = sqrt(sigma_a / D);                                  // effective transport coefficient

            const double r2 = square(r);
            const double sigma_tr_r = sigma_tr * r;
            const double sigma_tr_r_one = 1.0 + sigma_tr_r;
            const double cp_rcp_eta = 1.0 - fresnel_moment_two_c1(1.0 / eta);           // Cphi(1/eta) * 4

            const double t0 = exp(-sigma_tr_r) / (cp_rcp_eta * FourPiSquare * r2 * r);
            const double t1 = r2 / D + 3.0 * sigma_tr_r_one * dot_xw;
            const double t2 = 3.0 * D * sigma_tr_r_one * dot_wn;
            const double t3 = (sigma_tr_r_one + 3.0 * D * (3.0 * sigma_tr_r_one + square(sigma_tr_r)) / r2 * dot_xw) * dot_xn;

            return t0 * (cp * t1 - ce * (t2 - t3));
        }

        // Evaluate the directional dipole BSSRDF.
        static void bssrdf(
            const DirectionalDipoleBSSRDFInputValues*   values,
            const Vector3d&                             xi,
            const Vector3d&                             ni,
            const Vector3d&                             wi,
            const Vector3d&                             xo,
            const Vector3d&                             no,
            Spectrum&                                   result)
        {
            // Compute square distance between points of incidence and emergence.
            const Vector3d xoxi = xo - xi;
            const double r2 = square_norm(xoxi);

            // Compute normal to modified tangent plane.
            const Vector3d ni_star = cross(xoxi / sqrt(r2), normalize(cross(ni, xoxi)));

            // Compute direction of ray sources.
            const double eta = values->m_inside_ior / values->m_outside_ior;            // relative refractive index
            const double nnt = 1.0 / eta;
            const double ddn = -dot(wi, ni);
            const Vector3d wr = normalize(wi * -nnt - ni * (ddn * nnt + sqrt(1.0 - square(nnt) * (1.0 - square(ddn)))));
            const Vector3d wv = -reflect(wr, ni_star);                                  // direction of the virtual ray source

            // Precompute some stuff.
            const double cp = 0.25 * (1.0 - fresnel_moment_two_c1(eta));
            const double ce = 0.5 * (1.0 - fresnel_moment_three_c2(eta));
            const double A = (1.0 - ce) / (2.0 * cp);                                   // reflection parameter
            const double dot_xoxi_wr = dot(xoxi, wr);
            const double dot_wr_no = dot(wr, no);
            const double dot_xoxi_no = dot(xoxi, no);
            const double dot_wv_no = dot(wv, no);

            assert(result.size() == values->m_sigma_a.size());
            assert(result.size() == values->m_sigma_s.size());

            for (size_t i = 0, e = result.size(); i < e; ++i)
            {
                const double sigma_a = values->m_sigma_a[i];                            // absorption coefficient
                const double sigma_s = values->m_sigma_s[i];                            // scattering coefficient
                const double sigma_s_prime = sigma_s * (1.0 - values->m_anisotropy);    // reduced scattering coefficient
                const double sigma_t_prime = sigma_s_prime + sigma_a;                   // reduced extinction coefficient
                const double alpha_prime = sigma_s_prime / sigma_t_prime;               // reduced scattering albedo
                const double sigma_t = sigma_s + sigma_a;                               // extinction coefficient

                // Compute extrapolation distance (equation 21).
                const double D = 1.0 / (3.0 * sigma_t_prime);                           // diffusion coefficient
                const double de = 2.131 * D / sqrt(alpha_prime);                        // extrapolation distance

                // Compute corrected distance to real source.
                const double mu0 = -dot_wr_no;
                const double cos_beta = -sqrt((r2 - square(dot(wr, xoxi))) / (r2 + square(de)));
                const double dr =
                    mu0 > 0.0
                        ? sqrt(r2 + D * mu0 * (D * mu0 - 2.0 * de * cos_beta))          // frontlit
                        : sqrt(r2 + 1.0 / square(3.0 * sigma_t));                       // backlit

                // Compute position and distance to virtual source.
                const Vector3d xv = xi + 2.0 * A * de * ni_star;                        // position of the virtual ray source
                const Vector3d xoxv = xo - xv;
                const double dv = norm(xoxv);

                // Evaluate the BSSRDF.
                double value =
                      sd_prime(eta, cp, ce, D, sigma_a, dot_xoxi_wr, dot_wr_no, dot_xoxi_no, dr)
                    - sd_prime(eta, cp, ce, D, sigma_a, dot(xoxv, wv), dot_wv_no, dot(xoxv, no), dv);

                // Clamp negative values to zero (section 6.1).
                if (value < 0.0)
                    value = 0.0;

                // Store result.
                result[i] = static_cast<float>(value);
            }

            // todo: we seem to be missing the S_sigma_E term (single scattering).
            // See equation 12 (section 3.2) of the Directional Dipole paper.
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
            .insert("name", "weight")
            .insert("label", "Weight")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sigma_a")
            .insert("label", "Absorption")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.0030"));

    // Since presets are usually specified in mm.,
    // we set the multipliers so that the values are correct
    // for scenes modeled in meters.
    metadata.push_back(
        Dictionary()
            .insert("name", "sigma_a_multiplier")
            .insert("label", "Absorption Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1000"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sigma_s")
            .insert("label", "Scattering")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "2.29"));

    // Since presets are usually specified in mm.,
    // we set the multipliers so that the values are correct
    // for scenes modeled in meters.
    metadata.push_back(
        Dictionary()
            .insert("name", "sigma_s_multiplier")
            .insert("label", "Scattering Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1000"));

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
            .insert("name", "outside_ior")
            .insert("label", "Outside Index of Refraction")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "5.0")
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "inside_ior")
            .insert("label", "Inside Index of Refraction")
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
