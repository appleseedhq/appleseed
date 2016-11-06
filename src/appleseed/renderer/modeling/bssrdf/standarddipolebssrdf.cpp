
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Francois Beaune, The appleseedhq Organization
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
#include "standarddipolebssrdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/dipolebssrdf.h"
#include "renderer/modeling/bssrdf/sss.h"

// appleseed.foundation headers.
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Standard dipole BSSRDF.
    //
    // References:
    //
    //   [1] A Practical Model for Subsurface Light Transport
    //       https://graphics.stanford.edu/papers/bssrdf/bssrdf.pdf
    //
    //   [2] Light Diffusion in Multi-Layered Translucent Materials
    //       http://www.cs.virginia.edu/~jdl/bib/appearance/subsurface/donner05.pdf
    //

    const char* Model = "standard_dipole_bssrdf";

    class StandardDipoleBSSRDF
      : public DipoleBSSRDF
    {
      public:
        StandardDipoleBSSRDF(
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

        void prepare_inputs(
            const ShadingPoint& shading_point,
            void*               data) const APPLESEED_OVERRIDE
        {
            DipoleBSSRDFInputValues* values =
                reinterpret_cast<DipoleBSSRDFInputValues*>(data);

            do_prepare_inputs<ComputeRdStandardDipole>(shading_point, values);
        }

        virtual void evaluate_profile(
            const void*         data,
            const float         square_radius,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            const DipoleBSSRDFInputValues* values =
                reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

            const float fdr = fresnel_internal_diffuse_reflectance(values->m_precomputed.m_eta);
            const float a = (1.0f + fdr) / (1.0f - fdr);

            value.resize(values->m_sigma_a.size());

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const float sigma_a = values->m_sigma_a[i];
                const float sigma_s = values->m_sigma_s[i];
                const float sigma_s_prime = sigma_s * (1.0f - values->m_g);
                const float sigma_t_prime = sigma_s_prime + sigma_a;
                const float alpha_prime = values->m_precomputed.m_alpha_prime[i];
                const float sigma_tr = values->m_precomputed.m_sigma_tr[i];

                //
                // The extended source represented by the refracted ray in the medium is approximated
                // by a single isotropic point source at the center of mass of the beam, at a depth
                // of one mean free path, i.e. at zr = 1 / sigma_t_prime (searchlight configuration).
                //
                // We have
                //
                //   zr = 1 / sigma_t_prime
                //   zv = -zr - 2 * zb
                //      = -zr - 4 * A * D
                //
                // where
                //
                //   D = 1 / (3 * sigma_t_prime)
                //
                // Note that we use the sign conventions of the Better Dipole model:
                // zr is positive, zv is negative.
                //
                // The expression of zv can thus be simplified:
                //
                //   zv = -zr - 4 * A / (3 * sigma_t_prime)
                //      = -zr - zr * 4/3 * A
                //      = -zr * (1 + 4/3 * A)
                //

                const float zr = 1.0f / sigma_t_prime;
                const float zv = -zr * (1.0f + (4.0f / 3.0f) * a);

                //
                // Let's call xo the outgoing point, xi the incoming point and ni the normal at
                // the incoming point.
                //
                // dr is the (world space) distance between the outgoing point xo and the real
                // point light source located below the surface at xi, i.e. at xi - ni * zr.
                //
                // dv is the (world space) distance between the outgoing point xo and the virtual
                // point light source located above the surface at xi, i.e. at xi + ni * zv.
                //
                // If we were to compute these distances naively, i.e.
                //
                //   dr = || xo - (xi - ni * zr) ||
                //   dv = || xo - (xi + ni * zv) ||
                //
                // we would naturally account for the local curvature of the surface.
                //
                // Since the dipole model assumes a locally flat region, we compute these distances
                // assuming that the vector (xo - xi) is always orthogonal to the normal ni at xi:
                //
                //   dr = sqrt( ||xo - xi||^2 + zr^2 )
                //   dv = sqrt( ||xo - xi||^2 + zv^2 )
                //

                const float dr = sqrt(square_radius + zr * zr);
                const float dv = sqrt(square_radius + zv * zv);

                // The expression for R(r) in [1] is incorrect; use the correct expression from [2].
                const float rcp_dr = 1.0f / dr;
                const float rcp_dv = 1.0f / dv;
                const float sigma_tr_dr = sigma_tr * dr;
                const float sigma_tr_dv = sigma_tr * dv;
                const float kr = zr * (sigma_tr_dr + 1.0f) * square(rcp_dr);
                const float kv = zv * (sigma_tr_dv + 1.0f) * square(rcp_dv);
                const float er = exp(-sigma_tr_dr) * rcp_dr;
                const float ev = exp(-sigma_tr_dv) * rcp_dv;
                value[i] = alpha_prime * RcpFourPi<float>() * (kr * er - kv * ev);
            }

            // Return r * R(r) * weight.
            value *= sqrt(square_radius) * values->m_weight;
        }
    };
}


//
// StandardDipoleBSSRDFFactory class implementation.
//

const char* StandardDipoleBSSRDFFactory::get_model() const
{
    return Model;
}

Dictionary StandardDipoleBSSRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Standard Dipole BSSRDF");
}

auto_release_ptr<BSSRDF> StandardDipoleBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new StandardDipoleBSSRDF(name, params));
}

auto_release_ptr<BSSRDF> StandardDipoleBSSRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSSRDF>(new StandardDipoleBSSRDF(name, params));
}

}   // namespace renderer
