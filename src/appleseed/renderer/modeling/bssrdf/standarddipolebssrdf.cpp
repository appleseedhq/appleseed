
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
#include "standarddipolebssrdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
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
            sample.set_is_directional(false);
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

            const double r2 = square_norm(outgoing_point.get_point() - incoming_point.get_point());
            const double rcp_eta = values->m_outside_ior / values->m_inside_ior;
            const double fdr = fresnel_internal_diffuse_reflectance(rcp_eta);
            const double a = (1.0 + fdr) / (1.0 - fdr);
            const double sigma_tr = 1.0 / values->m_dmfp;

            value.resize(values->m_sigma_a.size());

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const double sigma_a = values->m_sigma_a[i];
                const double sigma_s = values->m_sigma_s[i];
                const double sigma_s_prime = sigma_s * (1.0 - values->m_anisotropy);
                const double sigma_t_prime = sigma_s_prime + sigma_a;
                const double alpha_prime = sigma_s_prime / sigma_t_prime;

                //
                // We have
                //
                //   zr = 1 / sigma_t_prime
                //   zv = zr + 4 * A * D
                //
                // where
                //
                //   D = 1 / (3 * sigma_t_prime)
                //
                // This simplifies to
                //
                //   zr = 1 / sigma_t_prime
                //   zv = zr + 4 * A / (3 * sigma_t_prime)
                //      = zr + zr * 4/3 * A
                //      = zr * (1 + 4/3 * A)
                //

                const double zr = 1.0 / sigma_t_prime;
                const double zv = zr * (1.0 + (4.0 / 3.0) * a);

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

                const double dr = sqrt(r2 + zr * zr);
                const double dv = sqrt(r2 + zv * zv);

                // The expression for R(r) in [1] is incorrect; use the correct expression from [2].
                const double sigma_tr_dr = sigma_tr * dr;
                const double sigma_tr_dv = sigma_tr * dv;
                const double value_r = (sigma_tr_dr + 1.0) * exp(-sigma_tr_dr) / (dr * dr * dr);
                const double value_v = (sigma_tr_dv + 1.0) * exp(-sigma_tr_dv) / (dv * dv * dv);
                value[i] = static_cast<float>(alpha_prime * RcpFourPi * (zr * value_r + zv * value_v));
            }

            // Return r * R(r) * weight.
            value *= static_cast<float>(sqrt(r2) * values->m_weight);
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

}   // namespace renderer
