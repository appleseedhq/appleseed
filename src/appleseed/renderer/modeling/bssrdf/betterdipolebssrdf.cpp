
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/bssrdf/dipolebssrdf.h"
#include "renderer/modeling/bssrdf/sss.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class Arena; }

using namespace foundation;

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

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const override
        {
            DipoleBSSRDFInputValues* values =
                static_cast<DipoleBSSRDFInputValues*>(data);

            do_prepare_inputs<ComputeRdBetterDipole>(shading_point, values);
        }

        void evaluate_profile(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3f&         incoming_dir,
            Spectrum&               value) const override
        {
            const DipoleBSSRDFInputValues* values =
                static_cast<const DipoleBSSRDFInputValues*>(data);

            const float square_radius =
                static_cast<float>(
                    square_norm(outgoing_point.get_point() - incoming_point.get_point()));

            const float two_c1 = fresnel_first_moment_x2(values->m_base_values.m_eta);
            const float three_c2 = fresnel_second_moment_x3(values->m_base_values.m_eta);
            const float A = (1.0f + three_c2) / (1.0f - two_c1);
            const float cphi = 0.25f * (1.0f - two_c1);
            const float ce = 0.5f * (1.0f - three_c2);

            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                const float sigma_a = values->m_sigma_a[i];
                const float sigma_s = values->m_sigma_s[i];
                const float sigma_s_prime = sigma_s * (1.0f - values->m_g);
                const float sigma_t_prime = sigma_s_prime + sigma_a;
                const float alpha_prime = values->m_precomputed.m_alpha_prime[i];
                const float sigma_tr = values->m_precomputed.m_sigma_tr[i];

                const float D = (2.0f * sigma_a + sigma_s_prime) / (3.0f * square(sigma_t_prime));
                const float zr = 1.0f / sigma_t_prime;
                const float zv = -zr - 4.0f * A * D;

                // See the note in the implementation of the standard dipole.
                const float dr = std::sqrt(square_radius + zr * zr);
                const float dv = std::sqrt(square_radius + zv * zv);

                const float rcp_dr = 1.0f / dr;
                const float rcp_dv = 1.0f / dv;
                const float sigma_tr_dr = sigma_tr * dr;
                const float sigma_tr_dv = sigma_tr * dv;
                const float cphi_over_D = cphi / D;
                const float kr = ce * zr * (sigma_tr_dr + 1.0f) * square(rcp_dr) + cphi_over_D;
                const float kv = ce * zv * (sigma_tr_dv + 1.0f) * square(rcp_dv) + cphi_over_D;
                const float er = std::exp(-sigma_tr_dr) * rcp_dr;
                const float ev = std::exp(-sigma_tr_dv) * rcp_dv;
                value[i] = square(alpha_prime) * RcpFourPi<float>() * (kr * er - kv * ev);
            }
        }
    };
}


//
// BetterDipoleBSSRDFFactory class implementation.
//

void BetterDipoleBSSRDFFactory::release()
{
    delete this;
}

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
