
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/dipolebssrdf.h"
#include "renderer/modeling/bssrdf/sss.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
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
    //   [1] Directional Dipole Model for Subsurface Scattering
    //       Jeppe Revall Frisvad, Toshiya Hachisuka, Thomas Kim Kjeldsen
    //       http://www.ci.i.u-tokyo.ac.jp/~hachisuka/dirpole.pdf
    //
    //   [2] Directional Dipole for Subsurface Scattering in Translucent Materials
    //       Jeppe Revall Frisvad, Toshiya Hachisuka, Thomas Kim Kjeldsen
    //       http://www.ci.i.u-tokyo.ac.jp/~hachisuka/dirpole_tr.pdf
    //

    const char* Model = "directional_dipole_bssrdf";

    class DirectionalDipoleBSSRDF
      : public DipoleBSSRDF
    {
      public:
        DirectionalDipoleBSSRDF(
            const char*         name,
            const ParamArray&   params)
          : DipoleBSSRDF(name, params)
        {
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual void prepare_inputs(
            const ShadingPoint& shading_point,
            void*               data) const APPLESEED_OVERRIDE
        {
            DipoleBSSRDFInputValues* values =
                reinterpret_cast<DipoleBSSRDFInputValues*>(data);

            do_prepare_inputs<ComputeRdStandardDipole>(shading_point, values);

            if (m_inputs.source("sigma_a") == 0 || m_inputs.source("sigma_s") == 0)
            {
                // Reparamerization weight to match the standard dipole reflectance.
                // This was derived by numerically integrating both models for different alpha_prime values,
                // comparing the resulting Rd curves, fitting a quadratic to the difference between them
                // and scaling by an empirical magic factor.
                values->m_dirpole_reparam_weight.resize(values->m_alpha_prime.size());
                for (size_t i = 0, e = values->m_dirpole_reparam_weight.size(); i < e; ++i)
                {
                    const float a = values->m_alpha_prime[i];
                    const float w = 0.2605589f * square(a) + 0.2622902f * a + 0.007895145f;
                    values->m_dirpole_reparam_weight[i] = w * 1.44773351f;
                }
            }
            else
                values->m_dirpole_reparam_weight.set(1.0f);
        }

        virtual void evaluate_profile(
            const void*         data,
            const double        square_radius,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            assert(!"Should never be called.");
        }

        virtual void evaluate(
            const void*         data,
            const ShadingPoint& outgoing_point,
            const Vector3d&     outgoing_dir,
            const ShadingPoint& incoming_point,
            const Vector3d&     incoming_dir,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            const DipoleBSSRDFInputValues* values =
                reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

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

            double fo;
            const double cos_on = abs(dot(outgoing_dir, outgoing_point.get_shading_normal()));
            fresnel_transmittance_dielectric(fo, values->m_eta, cos_on);

            double fi;
            const double cos_in = abs(dot(incoming_dir, incoming_point.get_shading_normal()));
            fresnel_transmittance_dielectric(fi, values->m_eta, cos_in);

            const double radius = norm(incoming_point.get_point() - outgoing_point.get_point());
            value *= static_cast<float>(radius * fo * fi * values->m_weight);

            value *= values->m_dirpole_reparam_weight;
        }

      private:
        // Evaluate the directional dipole BSSRDF.
        static void bssrdf(
            const DipoleBSSRDFInputValues*  values,
            const Vector3d&                 xi,
            const Vector3d&                 ni,
            const Vector3d&                 wi,
            const Vector3d&                 xo,
            const Vector3d&                 no,
            Spectrum&                       value)
        {
            // Precompute some stuff. Same as for the Better Dipole model.
            const Vector3d xoxi = xo - xi;
            const double r2 = square_norm(xoxi);                                        // square distance between points of incidence and emergence
            const double rcp_eta = 1.0 / values->m_eta;
            const double cphi_eta = 0.25 * (1.0 - fresnel_first_moment(rcp_eta));
            const double cphi_rcp_eta = 0.25 * (1.0 - fresnel_first_moment(values->m_eta));
            const double ce_eta = 0.5 * (1.0 - fresnel_second_moment(rcp_eta));
            const double A = (1.0 - ce_eta) / (2.0 * cphi_eta);                         // reflection parameter

            // Compute normal to modified tangent plane.
            const Vector3d ni_star = cross(xoxi / sqrt(r2), normalize(cross(ni, xoxi)));
            assert(is_normalized(ni_star));

            // Compute direction of real ray source.
            Vector3d wr;
            APPLESEED_UNUSED const bool successful = refract(wi, ni, values->m_eta, wr);
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
                const double sigma_a = values->m_sigma_a[i];
                const double sigma_s = values->m_sigma_s[i];
                const double sigma_t = sigma_s + sigma_a;
                const double sigma_s_prime = sigma_s * (1.0 - values->m_g);
                const double sigma_t_prime = sigma_s_prime + sigma_a;
                const double alpha_prime = values->m_alpha_prime[i];                    // reduced scattering albedo
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
                assert(feq(dv, sqrt(r2 + square(2.0 * A * de)), 1.0e-9));               // true because we computed xv using ni_star, not ni

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

        // Diffusion term of the BSSRDF.
        static double sd_prime(
            const double        cphi_eta,
            const double        ce_eta,
            const double        D,
            const double        sigma_tr,
            const double        dot_w_x,
            const double        dot_w_n,
            const double        dot_x_n,
            const double        r)
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

auto_release_ptr<BSSRDF> DirectionalDipoleBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new DirectionalDipoleBSSRDF(name, params));
}

auto_release_ptr<BSSRDF> DirectionalDipoleBSSRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSSRDF>(new DirectionalDipoleBSSRDF(name, params));
}

}   // namespace renderer
