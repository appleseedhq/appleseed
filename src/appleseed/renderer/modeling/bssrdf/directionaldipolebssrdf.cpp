
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/bssrdf/dipolebssrdf.h"
#include "renderer/modeling/bssrdf/sss.h"

// appleseed.foundation headers.
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class Arena; }

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
            const char*                     name,
            const ParamArray&               params)
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
            Arena&                          arena,
            const ShadingPoint&             shading_point,
            void*                           data) const APPLESEED_OVERRIDE
        {
            DipoleBSSRDFInputValues* values =
                static_cast<DipoleBSSRDFInputValues*>(data);

            do_prepare_inputs<ComputeRdStandardDipole>(shading_point, values);
        }

        virtual void evaluate_profile(
            const void*                     data,
            const ShadingPoint&             outgoing_point,
            const Vector3f&                 outgoing_dir,
            const ShadingPoint&             incoming_point,
            const Vector3f&                 incoming_dir,
            Spectrum&                       value) const APPLESEED_OVERRIDE
        {
            const DipoleBSSRDFInputValues* values =
                static_cast<const DipoleBSSRDFInputValues*>(data);

            const Vector3f xo(outgoing_point.get_point());
            const Vector3f xi(incoming_point.get_point());
            const Vector3f no(outgoing_point.get_shading_normal());
            const Vector3f ni(incoming_point.get_shading_normal());

            bssrdf(values, xi, ni, incoming_dir, xo, no, value);

            // Hack to make the BSSRDF reciprocal ([1] section 6.3):
            // Spectrum tmp;
            // bssrdf(values, xo, no, outgoing_dir, xi, ni, tmp);
            // madd(value, tmp, 0.5f);

            // The directional dipole expression already integrates the BRDF
            // at the incoming point so we need to cancel the 1/Pi term from
            // the Lambertian BRDF.
            value *= Pi<float>();
        }

      private:
        // Evaluate the directional dipole BSSRDF.
        static void bssrdf(
            const DipoleBSSRDFInputValues*  values,
            const Vector3f&                 xi,
            const Vector3f&                 ni,
            const Vector3f&                 wi,
            const Vector3f&                 xo,
            const Vector3f&                 no,
            Spectrum&                       value)
        {
            const Vector3f xoxi = xo - xi;
            const float r2 = square_norm(xoxi);                                         // square distance between points of incidence and emergence
            const float eta = values->m_base_values.m_eta;
            const float rcp_eta = 1.0f / eta;
            const float cphi_eta = 0.25f * (1.0f - fresnel_first_moment_x2(rcp_eta));
            const float ce_eta = 0.5f * (1.0f - fresnel_second_moment_x3(rcp_eta));
            const float A = (1.0f - ce_eta) / (2.0f * cphi_eta);                        // reflection parameter

            // Compute normal to modified tangent plane.
            const Vector3f v = cross(ni, xoxi);
            const float norm_v = norm(v);
            if (norm_v == 0.0f)
            {
                value.set(0.0f);
                return;
            }
            const Vector3f ni_star =
                r2 > 0.0f ? cross(xoxi / sqrt(r2), v / norm_v) : ni;                    // we assume that if r2 > 0, then sqrt(r2) is also > 0
            assert(is_normalized(ni_star));

            // Compute direction of real ray source.
            Vector3f wr;
            APPLESEED_UNUSED const bool successful = refract(wi, ni, eta, wr);
            assert(successful);
            assert(is_normalized(wr));

            // Compute direction of virtual ray source.
            const Vector3f wv = -reflect(wr, ni_star);
            assert(is_normalized(wv));

            const float dot_wr_xoxi = dot(xoxi, wr);                                    // r * cos( real source direction, modified plane tangent )
            const float dot_wr_no = dot(wr, no);                                        // cos( real source direction, normal at outgoing )
            const float dot_xoxi_no = dot(xoxi, no);
            const float dot_wv_no = dot(wv, no);

            value.resize(values->m_sigma_a.size());

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const float sigma_a = values->m_sigma_a[i];
                const float sigma_s = values->m_sigma_s[i];
                const float sigma_t = sigma_s + sigma_a;
                const float sigma_s_prime = sigma_s * (1.0f - values->m_g);
                const float sigma_t_prime = sigma_s_prime + sigma_a;
                const float alpha_prime = values->m_precomputed.m_alpha_prime[i];       // reduced scattering albedo
                const float sigma_tr = values->m_precomputed.m_sigma_tr[i];             // effective transport coefficient

                // Compute extrapolation distance ([1] equation 21).
                const float D = 1.0f / (3.0f * sigma_t_prime);                          // diffusion coefficient
                const float de = 2.131f * D / sqrt(alpha_prime);                        // distance to extrapolated boundary
                //const float de = 2.121f * D / alpha_prime;                            // [2] equation 18

                // Compute corrected distance to real source.
                const float cos_beta = -sqrt((r2 - square(dot_wr_xoxi)) / (r2 + de * de));
                const float mu0 = -dot_wr_no;
                const float zr2 =
                    mu0 > 0.0f
                        ? D * mu0 * (D * mu0 - 2.0f * de * cos_beta)                    // frontlit
                        : 1.0f / square(3.0f * sigma_t);                                // backlit
                const float dr = sqrt(r2 + zr2);                                        // distance to real ray source

                // Compute position of and distance to virtual source.
                const Vector3f xv = xi + (2.0f * A * de) * ni_star;                     // position of the virtual ray source
                const Vector3f xoxv = xo - xv;
                const float dot_xoxv_no = dot(xoxv, no);
                const float dv = norm(xoxv);                                            // distance to virtual ray source

                // Evaluate the BSSRDF.
                const float sdr = sd_prime(cphi_eta, ce_eta, D, sigma_tr, dot_wr_xoxi, dot_wr_no, dot_xoxi_no, dr);
                const float sdv = sd_prime(cphi_eta, ce_eta, D, sigma_tr, dot(wv, xoxv), dot_wv_no, dot_xoxv_no, dv);
                value[i] = max(sdr - sdv, 0.0f);
            }

            // todo: add reduced intensity component here (S_sigma_E term).
            // See [1] equation 12 (section 3.2).
        }

        // Diffusion term of the BSSRDF.
        static float sd_prime(
            const float cphi_eta,
            const float ce_eta,
            const float D,
            const float sigma_tr,
            const float dot_w_x,
            const float dot_w_n,
            const float dot_x_n,
            const float r)
        {
            const float r2 = square(r);
            const float sigma_tr_r = sigma_tr * r;

            const float t0 = RcpFourPiSquare<float>() * exp(-sigma_tr_r) / (r2 * r);
            const float t1 = r2 / D + 3.0f * (1.0f + sigma_tr_r) * dot_w_x;
            const float t2 = 3.0f * D * (1.0f + sigma_tr_r) * dot_w_n;
            const float t3 = (1.0f + sigma_tr_r + 3.0f * D * (3.0f * (1.0f + sigma_tr_r) + square(sigma_tr_r)) / r2 * dot_w_x) * dot_x_n;

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
