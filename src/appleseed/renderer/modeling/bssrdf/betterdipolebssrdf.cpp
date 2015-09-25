
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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
#include "betterdipolebssrdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace renderer  { class ShadingContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Better dipole BSSRDF.
    //
    // Reference:
    //
    //   A Better Dipole
    //   http://www.eugenedeon.com/wp-content/uploads/2014/04/betterdipole.pdf
    //

    const char* Model = "better_dipole_bssrdf";

    class BetterDipoleBSSRDF
      : public DipoleBSSRDF
    {
      public:
        BetterDipoleBSSRDF(
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

        virtual void prepare_inputs(void* data) const APPLESEED_OVERRIDE
        {
            DipoleBSSRDFInputValues* values =
                reinterpret_cast<DipoleBSSRDFInputValues*>(data);

            // Apply multipliers.
            values->m_reflectance *= static_cast<float>(values->m_reflectance_multiplier);
            values->m_dmfp *= values->m_dmfp_multiplier;

            // Clamp reflectance.
            values->m_reflectance = clamp(values->m_reflectance, 0.001f, 1.0f);

            // Compute sigma_a and sigma_s from the reflectance and dmfp parameters.
            const ComputeRdBetterDipole rd_fun(values->m_outside_ior / values->m_inside_ior);
            compute_absorption_and_scattering(
                rd_fun,
                values->m_reflectance,
                values->m_dmfp,
                values->m_anisotropy,
                values->m_sigma_a,
                values->m_sigma_s);

            // Precompute the (square of the) max radius.
            values->m_max_radius2 = square(dipole_max_radius(1.0 / values->m_dmfp));
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

            const double r2 = square_norm(outgoing_point.get_point() - incoming_point.get_point());
            const double eta = values->m_outside_ior / values->m_inside_ior;
            const double two_c1 = fresnel_first_moment(eta);
            const double three_c2 = fresnel_second_moment(eta);
            const double A = (1.0 + three_c2) / (1.0 - two_c1);
            const double cphi = 0.25 * (1.0 - two_c1);
            const double ce = 0.5 * (1.0 - three_c2);
            const double sigma_tr = 1.0 / values->m_dmfp;

            value.resize(values->m_sigma_a.size());

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const double sigma_a = values->m_sigma_a[i];
                const double sigma_s = values->m_sigma_s[i];
                const double sigma_s_prime = sigma_s * (1.0 - values->m_anisotropy);
                const double sigma_t_prime = sigma_s_prime + sigma_a;
                const double alpha_prime = sigma_s_prime / sigma_t_prime;

                const double D = (2.0 * sigma_a + sigma_s_prime) / (3.0 * square(sigma_t_prime));
                const double zr = 1.0 / sigma_t_prime;
                const double zv = -zr - 4.0 * A * D;

                // See the note in the implementation of the standard dipole.
                const double dr = sqrt(r2 + zr * zr);
                const double dv = sqrt(r2 + zv * zv);

                const double rcp_dr = 1.0 / dr;
                const double rcp_dv = 1.0 / dv;
                const double sigma_tr_dr = sigma_tr * dr;
                const double sigma_tr_dv = sigma_tr * dv;
                const double cphi_over_D = cphi / D;
                const double kr = ce * zr * (sigma_tr_dr + 1.0) * square(rcp_dr) + cphi_over_D;
                const double kv = ce * zv * (sigma_tr_dv + 1.0) * square(rcp_dv) + cphi_over_D;
                const double er = exp(-sigma_tr_dr) * rcp_dr;
                const double ev = exp(-sigma_tr_dv) * rcp_dv;
                value[i] = static_cast<float>(square(alpha_prime) * RcpFourPi * (kr * er - kv * ev));
            }

            // Return r * R(r) * weight.
            value *= static_cast<float>(sqrt(r2) * values->m_weight);
        }
    };
}


//
// BetterDipoleBSSRDFFactory class implementation.
//

const char* BetterDipoleBSSRDFFactory::get_model() const
{
    return Model;
}

Dictionary BetterDipoleBSSRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Better Dipole BSSRDF");
}

auto_release_ptr<BSSRDF> BetterDipoleBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new BetterDipoleBSSRDF(name, params));
}

}   // namespace renderer
