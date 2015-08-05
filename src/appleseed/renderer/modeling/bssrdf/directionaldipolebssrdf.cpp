
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
      : public DipoleBSSRDF
    {
      public:
        DirectionalDipoleBSSRDF(
            const char*             name,
            const ParamArray&       params)
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

        virtual bool sample(
            const void*             data,
            BSSRDFSample&           sample) const APPLESEED_OVERRIDE
        {
            sample.set_is_directional(true);
            return DipoleBSSRDF::sample(data, sample);
        }

        virtual void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3d&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3d&         incoming_dir,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const DipoleBSSRDFInputValues* values =
                reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

            bssrdf(
                values,
                incoming_point.get_point(),
                incoming_point.get_shading_normal(),
                incoming_dir,
                outgoing_point.get_point(),
                outgoing_point.get_shading_normal(),
                value);

#ifdef DIRPOLE_RECIPROCAL
            // Hack to make the BSSRDF reciprocal (section 6.3).
            Spectrum tmp;
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

            // Return r * R(r) * weight.
            const double radius = norm(incoming_point.get_point() - outgoing_point.get_point());
            value *= static_cast<float>(radius * values->m_weight);
        }

      private:
        // Diffusive part of the BSSRDF.
        static double sd_prime(
            const double            rcp_eta,
            const double            cphi,
            const double            ce,
            const double            D,
            const double            sigma_tr,
            const double            dot_xw,
            const double            dot_wn,
            const double            dot_xn,
            const double            r)                                                  // distance from point of incidence
        {
            const double r2 = square(r);
            const double sigma_tr_r = sigma_tr * r;
            const double sigma_tr_r_one = sigma_tr_r + 1.0;
            const double cp_rcp_eta = 1.0 - fresnel_first_moment(rcp_eta);              // Cphi(1/eta) * 4

            const double t0 = exp(-sigma_tr_r) / (cp_rcp_eta * r2 * r);
            const double t1 = r2 / D + 3.0 * sigma_tr_r_one * dot_xw;
            const double t2 = 3.0 * D * sigma_tr_r_one * dot_wn;
            const double t3 = (sigma_tr_r_one + 3.0 * D * (3.0 * sigma_tr_r_one + square(sigma_tr_r)) / r2 * dot_xw) * dot_xn;

            return RcpFourPiSquare * t0 * (cphi * t1 - ce * (t2 - t3));
        }

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
            const double rcp_eta = values->m_outside_ior / values->m_inside_ior;        // 1 / relative refractive index
            const double two_c1 = fresnel_first_moment(rcp_eta);
            const double three_c2 = fresnel_second_moment(rcp_eta);
            const double A = (1.0 + three_c2) / (1.0 - two_c1);                         // reflection parameter
            const double cphi = 0.25 * (1.0 - two_c1);
            const double ce = 0.5 * (1.0 - three_c2);
            const double sigma_tr = 1.0 / values->m_dmfp;                               // effective transport coefficient

            // Compute normal to modified tangent plane.
            const Vector3d ni_star = cross(xoxi / sqrt(r2), normalize(cross(ni, xoxi)));
            assert(is_normalized(ni_star));

            // Compute direction of ray sources.
            const double ddn = -dot(wi, ni);
            const Vector3d wr = normalize(wi * -rcp_eta - ni * (ddn * rcp_eta + sqrt(1.0 - square(rcp_eta) * (1.0 - square(ddn)))));
            const Vector3d wv = -reflect(wr, ni_star);                                  // direction of the virtual ray source

            const double dot_xoxi_wr = dot(xoxi, wr);
            const double dot_wr_no = dot(wr, no);
            const double dot_xoxi_no = dot(xoxi, no);
            const double dot_wv_no = dot(wv, no);

            value.resize(values->m_sigma_a.size());

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const double sigma_a = values->m_sigma_a[i];                            // absorption coefficient
                const double sigma_s = values->m_sigma_s[i];                            // scattering coefficient

                if (sigma_s == 0.0)
                {
                    value[i] = 0.0f;
                    continue;
                }

                const double sigma_s_prime = sigma_s * (1.0 - values->m_anisotropy);    // reduced scattering coefficient
                const double sigma_t_prime = sigma_s_prime + sigma_a;                   // reduced extinction coefficient
                const double alpha_prime = sigma_s_prime / sigma_t_prime;               // reduced scattering albedo
                const double sigma_t = sigma_s + sigma_a;                               // extinction coefficient

                // Compute extrapolation distance (equation 21).
                const double D = 1.0 / (3.0 * sigma_t_prime);                           // diffusion coefficient
                const double de = 2.131 * D / sqrt(alpha_prime);                        // distance to extrapolated boundary

                // Compute corrected distance to real source.
                const double mu0 = -dot_wr_no;
                const double cos_beta = -sqrt((r2 - square(dot(wr, xoxi))) / (r2 + square(de)));
                const double zr2 =
                    mu0 > 0.0
                        ? D * mu0 * (D * mu0 - 2.0 * de * cos_beta)                     // frontlit
                        : D * D;                                                        // backlit
                const double dr = sqrt(r2 + zr2);

                // Compute position and distance to virtual source.
                const Vector3d xv = xi + (2.0 * A * de) * ni_star;                      // position of the virtual ray source
                const Vector3d xoxv = xo - xv;
                //const double dv = norm(xoxv);
                const double zv = 2.0 * A * de;
                const double dv = sqrt(r2 + zv * zv);

                // Evaluate the BSSRDF.
                double result =
                      sd_prime(rcp_eta, cphi, ce, D, sigma_tr, dot_xoxi_wr, dot_wr_no, dot_xoxi_no, dr)
                    - sd_prime(rcp_eta, cphi, ce, D, sigma_tr, dot(xoxv, wv), dot_wv_no, dot(xoxv, no), dv);

                // Clamp negative values to zero (section 6.1).
                if (result < 0.0)
                    result = 0.0;

                // Store result.
                value[i] = static_cast<float>(result);
            }

            // todo: add reduced intensity component here (S_sigma_E term).
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

auto_release_ptr<BSSRDF> DirectionalDipoleBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new DirectionalDipoleBSSRDF(name, params));
}

}   // namespace renderer
