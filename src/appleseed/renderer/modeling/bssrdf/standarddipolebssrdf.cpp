
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
#include "foundation/image/colorspace.h"
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
          , m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
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

            // Precompute the relative index of refraction.
            const double outside_ior =
                shading_point.is_entering()
                    ? shading_point.get_ray().get_current_ior()
                    : shading_point.get_ray().get_previous_ior();

            values->m_eta = outside_ior / values->m_ior;

            // Clamp anisotropy.
            values->m_anisotropy = clamp(values->m_anisotropy, 0.0, 0.999);

            if (m_inputs.source("sigma_a") == 0 || m_inputs.source("sigma_s") == 0)
            {
                //
                // Compute sigma_a, sigma_s and sigma_tr from the diffuse surface reflectance
                // and diffuse mean free path (dmfp).
                //

                if (values->m_reflectance.size() != values->m_dmfp.size())
                {
                    // Since it does not really make sense to convert a dmfp,
                    // a per channel distance, as if it were a color,
                    // we instead always convert the reflectance to match the
                    // size of the dmfp.
                    if (values->m_dmfp.is_spectral())
                    {
                        Spectrum::upgrade(
                            values->m_reflectance,
                            values->m_reflectance);
                    }
                    else
                    {
                        Spectrum::downgrade(
                            m_lighting_conditions,
                            values->m_reflectance,
                            values->m_reflectance);
                    }
                }

                // Apply multipliers to input values.
                values->m_reflectance *= static_cast<float>(values->m_reflectance_multiplier);
                values->m_dmfp *= static_cast<float>(values->m_dmfp_multiplier);

                // Clamp input values.
                clamp_in_place(values->m_reflectance, 0.001f, 0.999f);
                clamp_low_in_place(values->m_dmfp, 1.0e-5f);

                // Compute sigma_a and sigma_s.
                const ComputeRdStandardDipole rd_fun(values->m_eta);
                compute_absorption_and_scattering(
                    rd_fun,
                    values->m_reflectance,
                    values->m_dmfp,
                    values->m_anisotropy,
                    values->m_sigma_a,
                    values->m_sigma_s);

                // Compute sigma_tr = 1 / dmfp.
                values->m_sigma_tr = rcp(values->m_dmfp);
            }
            else
            {
                //
                // Compute sigma_tr from sigma_a and sigma_s.
                //
                // If you want to use the sigma_a and sigma_s values provided in [1],
                // and if your scene is modeled in meters, you will need to *multiply*
                // them by 1000. If your scene is modeled in centimers (e.g. the "size"
                // of the object is 4 units) then you will need to multiply the sigmas
                // by 100.
                //

                effective_extinction_coefficient(
                    values->m_sigma_a,
                    values->m_sigma_s,
                    values->m_anisotropy,
                    values->m_sigma_tr);
            }

            // Precompute some coefficients.
            values->m_sigma_s_prime = values->m_sigma_s * static_cast<float>(1.0 - values->m_anisotropy);
            values->m_sigma_t_prime = values->m_sigma_s_prime + values->m_sigma_a;
            values->m_alpha_prime = values->m_sigma_s_prime / values->m_sigma_t_prime;

            // Build a CDF for channel sampling.
            values->m_channel_pdf = values->m_alpha_prime;
            values->m_channel_cdf.resize(values->m_channel_pdf.size());
            float cumulated_pdf = 0.0f;
            for (size_t i = 0, e = values->m_channel_cdf.size(); i < e; ++i)
            {
                cumulated_pdf += values->m_channel_pdf[i];
                values->m_channel_cdf[i] = cumulated_pdf;
            }
            const float rcp_cumulated_pdf = 1.0f / cumulated_pdf;
            values->m_channel_pdf *= rcp_cumulated_pdf;
            values->m_channel_cdf *= rcp_cumulated_pdf;
            values->m_channel_cdf[values->m_channel_cdf.size() - 1] = 1.0f;

            // Precompute the (square of the) max radius.
            values->m_rmax2 = square(dipole_max_radius(min_value(values->m_sigma_tr)));
        }

        virtual void evaluate_profile(
            const void*         data,
            const double        square_radius,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            const DipoleBSSRDFInputValues* values =
                reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

            const double fdr = fresnel_internal_diffuse_reflectance(values->m_eta);
            const double a = (1.0 + fdr) / (1.0 - fdr);

            value.resize(values->m_sigma_a.size());

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const double sigma_t_prime = values->m_sigma_t_prime[i];
                const double alpha_prime = values->m_alpha_prime[i];
                const double sigma_tr = values->m_sigma_tr[i];

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

                const double zr = 1.0 / sigma_t_prime;
                const double zv = -zr * (1.0 + (4.0 / 3.0) * a);

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

                const double dr = sqrt(square_radius + zr * zr);
                const double dv = sqrt(square_radius + zv * zv);

                // The expression for R(r) in [1] is incorrect; use the correct expression from [2].
                const double rcp_dr = 1.0 / dr;
                const double rcp_dv = 1.0 / dv;
                const double sigma_tr_dr = sigma_tr * dr;
                const double sigma_tr_dv = sigma_tr * dv;
                const double kr = zr * (sigma_tr_dr + 1.0) * square(rcp_dr);
                const double kv = zv * (sigma_tr_dv + 1.0) * square(rcp_dv);
                const double er = exp(-sigma_tr_dr) * rcp_dr;
                const double ev = exp(-sigma_tr_dv) * rcp_dv;
                value[i] = static_cast<float>(alpha_prime * RcpFourPi * (kr * er - kv * ev));
            }

            // Return r * R(r) * weight.
            value *= static_cast<float>(sqrt(square_radius) * values->m_weight);
        }

      private:
        const LightingConditions m_lighting_conditions;
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
