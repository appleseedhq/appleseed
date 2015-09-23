
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
#include "foundation/math/sampling/mappings.h"
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
    // References:
    //
    //   [1] Directional Dipole for Subsurface Scattering
    //       Jeppe Revall Frisvad, Toshiya Hachisuka, Thomas Kim Kjeldsen
    //       http://www.ci.i.u-tokyo.ac.jp/~hachisuka/dirpole.pdf
    //
    //   [2] Directional Dipole for Subsurface Scattering in Translucent Materials
    //       Jeppe Revall Frisvad, Toshiya Hachisuka, Thomas Kim Kjeldsen
    //       http://www.ci.i.u-tokyo.ac.jp/~hachisuka/dirpole_tr.pdf
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
            m_inputs.declare("preset", InputFormatScalar, "0.0");
            m_inputs.declare("sigma_a", InputFormatSpectralReflectance);
            m_inputs.declare("sigma_s", InputFormatSpectralReflectance);
            m_inputs.declare("anisotropy", InputFormatScalar);
            m_inputs.declare("outside_ior", InputFormatScalar);
            m_inputs.declare("inside_ior", InputFormatScalar);
            m_inputs.declare("scale", InputFormatScalar, "1.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        size_t compute_input_data_size(
            const Assembly&         assembly) const APPLESEED_OVERRIDE
        {
            return align(sizeof(DirectionalDipoleBSSRDFInputValues), 16);
        }

        void prepare_inputs(void* data) const APPLESEED_OVERRIDE
        {
            DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<DirectionalDipoleBSSRDFInputValues*>(data);

            // Presets.
            const size_t preset = floor(values->m_preset);
            apply_preset(preset, values);

            // Apply multipliers.
            values->m_sigma_a *= static_cast<float>(values->m_scale);
            values->m_sigma_s *= static_cast<float>(values->m_scale);

            // Precompute sigma_tr.
            effective_extinction_coefficient(
                values->m_sigma_a,
                values->m_sigma_s,
                values->m_anisotropy,
                values->m_sigma_tr);

            // Precompute max radius.
            const double min_sigma_tr = min_value(values->m_sigma_tr);
            values->m_max_radius2 = square(dipole_max_radius(min_sigma_tr));
        }

        virtual bool sample(
            const void*             data,
            BSSRDFSample&           sample) const APPLESEED_OVERRIDE
        {
            const DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<const DirectionalDipoleBSSRDFInputValues*>(data);

            if (values->m_weight == 0.0)
                return false;

            sample.set_eta(values->m_outside_ior / values->m_inside_ior);

            sample.get_sampling_context().split_in_place(3, 1);
            const Vector3d s = sample.get_sampling_context().next_vector2<3>();

            // Sample a channel.
            const size_t channel = truncate<size_t>(s[0] * values->m_sigma_a.size());
            sample.set_channel(channel);

            // Sample a radius.
            const double sigma_tr = values->m_sigma_tr[channel];
            const double radius = sample_exponential_distribution(s[1], sigma_tr);

            // Set the max radius.
            sample.set_rmax2(values->m_max_radius2);

            // Sample an angle.
            const double phi = TwoPi * s[2];

            // Set the sampled point.
            sample.set_point(Vector2d(radius * cos(phi), radius * sin(phi)));

            return true;
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

            const Vector3d incoming_normal =
                dot(incoming_dir, incoming_point.get_shading_normal()) > 0.0
                    ? incoming_point.get_shading_normal()
                    : -incoming_point.get_shading_normal();

            bssrdf(
                values,
                incoming_point.get_point(),
                incoming_normal,
                incoming_dir,
                outgoing_point.get_point(),
                outgoing_point.get_shading_normal(),
                value);

#ifdef DIRPOLE_RECIPROCAL
            // Hack to make the BSSRDF reciprocal ([1] section 6.3).
            Spectrum tmp;
            bssrdf(
                values,
                outgoing_point.get_point(),
                outgoing_point.get_shading_normal(),
                outgoing_dir,
                incoming_point.get_point(),
                incoming_point.get_shading_normal(),    // todo: possibly need to swap the normal here?
                tmp);
            value += tmp;
            value *= 0.5f;
#endif
            value *= static_cast<float>(norm(outgoing_point.get_point() - incoming_point.get_point()));
            value *= static_cast<float>(values->m_weight);
        }

        double evaluate_pdf(
            const void*             data,
            const size_t            channel,
            const double            radius) const APPLESEED_OVERRIDE
        {
            const DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<const DirectionalDipoleBSSRDFInputValues*>(data);

            // PDF of the sampled radius.
            double pdf_radius = 0.0;
            for (size_t i = 0, e = values->m_sigma_tr.size(); i < e; ++i)
            {
                const double sigma_tr = values->m_sigma_tr[i];
                pdf_radius += exponential_distribution_pdf(radius, sigma_tr);
            }

            pdf_radius /= static_cast<double>(values->m_sigma_tr.size());

            // PDF of the sampled angle.
            const double pdf_angle = RcpTwoPi;

            // Compute and return the final PDF.
            return pdf_radius * pdf_angle;
        }

      private:
        void apply_preset(const size_t preset, DirectionalDipoleBSSRDFInputValues* values) const
        {
            switch(preset)
            {
                case 1: // Apple
                    values->m_sigma_s = foundation::Color3f(2.29f, 2.39f, 1.97f);
                    values->m_sigma_a = foundation::Color3f(0.0030f, 0.0034f, 0.046f);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                case 2: // Chicken1
                    values->m_sigma_s = foundation::Color3f(0.15, 0.21, 0.38f);
                    values->m_sigma_a = foundation::Color3f(0.0015, 0.077,  0.19f);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                case 3: // Chicken2
                    values->m_sigma_s = foundation::Color3f(0.19, 0.25, 0.32f);
                    values->m_sigma_a = foundation::Color3f(0.0018, 0.088,  0.20f);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                case 4: // Cream
                    values->m_sigma_s = foundation::Color3f(7.38, 5.47, 3.15f);
                    values->m_sigma_a = foundation::Color3f(0.0002, 0.0028, 0.0163f);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                case 5: // Ketchup
                    values->m_sigma_s = foundation::Color3f(0.18, 0.07, 0.03f);
                    values->m_sigma_a = foundation::Color3f(0.061,  0.97,   1.45f);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                case 6: // Marble
                    values->m_sigma_s = foundation::Color3f(2.19, 2.62, 3.00f);
                    values->m_sigma_a = foundation::Color3f(0.0021, 0.0041, 0.0071f);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                case 7: // Potato
                    values->m_sigma_s = foundation::Color3f(0.68, 0.70, 0.55f);
                    values->m_sigma_a = foundation::Color3f( 0.0024, 0.0090, 0.12);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                case 8: // Skimmilk
                    values->m_sigma_s = foundation::Color3f(0.70, 1.22, 1.90f);
                    values->m_sigma_a = foundation::Color3f(0.0014, 0.0025, 0.0142f);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                case 9: // Skin1
                    values->m_sigma_s = foundation::Color3f(0.74, 0.88, 1.01);
                    values->m_sigma_a = foundation::Color3f(0.032,  0.17,   0.48);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                case 10: // Skin2
                    values->m_sigma_s = foundation::Color3f(1.09, 1.59, 1.79);
                    values->m_sigma_a = foundation::Color3f(0.013,  0.070,  0.145);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                case 11: // Wholemilk
                    values->m_sigma_s = foundation::Color3f(2.55, 3.21, 3.77);
                    values->m_sigma_a = foundation::Color3f(0.0011, 0.0024, 0.014);
                    values->m_anisotropy = 0.0;
                    values->m_inside_ior = 1.3;
                break;

                default:
                break;
            }
        }

        // Evaluate the directional dipole BSSRDF.
        static void bssrdf(
            const DirectionalDipoleBSSRDFInputValues*   values,
            const Vector3d&                             xi,
            const Vector3d&                             ni,
            const Vector3d&                             wi,
            const Vector3d&                             xo,
            const Vector3d&                             no,
            Spectrum&                                   value)
        {
            // Precompute some stuff. Same as for the Better Dipole model.
            const Vector3d xoxi = xo - xi;
            const double r2 = square_norm(xoxi);                                        // square distance between points of incidence and emergence
            const double eta = values->m_outside_ior / values->m_inside_ior;            // relative refractive index
            const double rcp_eta = 1.0 / eta;
            const double cphi_eta = 0.25 * (1.0 - fresnel_first_moment(rcp_eta));
            const double cphi_rcp_eta = 0.25 * (1.0 - fresnel_first_moment(eta));
            const double ce_eta = 0.5 * (1.0 - fresnel_second_moment(rcp_eta));
            const double A = (1.0 - ce_eta) / (2.0 * cphi_eta);                         // reflection parameter

            // Compute normal to modified tangent plane.
            const Vector3d ni_star = cross(xoxi / sqrt(r2), normalize(cross(ni, xoxi)));
            assert(is_normalized(ni_star));

            // Compute direction of real ray source.
            Vector3d wr;
            const bool successful = refract(wi, ni, eta, wr);
            assert(successful);
            assert(is_normalized(wr));

            // Compute direction of virtual ray source.
            const Vector3d wv = -reflect(wr, ni_star);
            assert(is_normalized(wv));

            const double dot_wr_xoxi = dot(xoxi, wr);                                   // r * cos( real source direction, modified plane tangent )
            const double dot_wr_no = dot(wr, no);                                       // cos( real source direction, normal at outgoing )
            const double dot_xoxi_no = dot(xoxi, no);
            const double dot_wv_no = dot(wv, no);

            value.resize(values->m_sigma_a.size());

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const double sigma_a = values->m_sigma_a[i];                            // absorption coefficient
                const double sigma_s = values->m_sigma_s[i];                            // scattering coefficient
                const double sigma_t = sigma_s + sigma_a;                               // extinction coefficient
                const double sigma_s_prime = sigma_s * (1.0 - values->m_anisotropy);    // reduced scattering coefficient
                const double sigma_t_prime = sigma_s_prime + sigma_a;                   // reduced extinction coefficient
                const double alpha_prime = sigma_s_prime / sigma_t_prime;               // reduced scattering albedo
                const double sigma_tr = values->m_sigma_tr[i];                          // effective transport coefficient

                // Compute extrapolation distance ([1] equation 21).
                const double D = 1.0 / (3.0 * sigma_t_prime);                           // diffusion coefficient
                const double de = 2.131 * D / sqrt(alpha_prime);                        // distance to extrapolated boundary
                //const double de = 2.121 * D / alpha_prime;                            // [2] equation 18

                // Compute corrected distance to real source.
                const double cos_beta = -sqrt((r2 - square(dot_wr_xoxi)) / (r2 + de * de));
                const double mu0 = -dot_wr_no;
                const double zr2 =
                    mu0 > 0.0
                        ? D * mu0 * (D * mu0 - 2.0 * de * cos_beta)                     // frontlit
                        : 1.0 / square(3.0 * sigma_t);                                  // backlit
                const double dr = sqrt(r2 + zr2);                                       // distance to real ray source

                // Compute position of virtual source.
                const Vector3d xv = xi + (2.0 * A * de) * ni_star;                      // position of the virtual ray source
                const Vector3d xoxv = xo - xv;
                const double dot_xoxv_no = dot(xoxv, no);

                // Compute distance to virtual source.
                const double dv = norm(xoxv);                                           // distance to virtual ray source
                assert(feq(dv, sqrt(r2 + square(2.0 * A * de))));                       // true because we computed xv using ni_star, not ni

                // Evaluate the BSSRDF.
                const double sdr = sd_prime(cphi_eta, ce_eta, D, sigma_tr, dot_wr_xoxi, dot_wr_no, dot_xoxi_no, dr);
                const double sdv = sd_prime(cphi_eta, ce_eta, D, sigma_tr, dot(wv, xoxv), dot_wv_no, dot_xoxv_no, dv);
                const double result = max((sdr - sdv) / (4.0 * cphi_rcp_eta), 0.0);

                // Store result.
                value[i] = static_cast<float>(result);
            }

            // todo: add reduced intensity component here (S_sigma_E term).
            // See [1] equation 12 (section 3.2).
        }

        // Diffusive part of the BSSRDF.
        static double sd_prime(
            const double            cphi_eta,
            const double            ce_eta,
            const double            D,
            const double            sigma_tr,
            const double            dot_w_x,
            const double            dot_w_n,
            const double            dot_x_n,
            const double            r)
        {
            const double r2 = square(r);
            const double sigma_tr_r = sigma_tr * r;

            const double t0 = RcpFourPiSquare * exp(-sigma_tr_r) / (r2 * r);
            const double t1 = r2 / D + 3.0 * (1.0 + sigma_tr_r) * dot_w_x;
            const double t2 = 3.0 * D * (1.0 + sigma_tr_r) * dot_w_n;
            const double t3 = (1.0 + sigma_tr_r + 3.0 * D * (3.0 * (1.0 + sigma_tr_r) + square(sigma_tr_r)) / r2 * dot_w_x) * dot_x_n;

            return t0 * (cphi_eta * t1 - ce_eta * (t2 - t3));
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
            .insert("name", "preset")
            .insert("label", "Preset")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "11.0")
            .insert("use", "optional")
            .insert("default", "0.0"));

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
            .insert("default", "0.5"));

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
            .insert("default", "0.5"));

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

    metadata.push_back(
        Dictionary()
            .insert("name", "scale")
            .insert("label", "Scale")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "100000.0")
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<BSSRDF> DirectionalDipoleBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new DirectionalDipoleBSSRDF(name, params));
}

}   // namespace renderer
