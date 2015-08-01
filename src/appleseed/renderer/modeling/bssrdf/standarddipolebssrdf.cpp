
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/memory.h"

// Standard headers.
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
    // Standard dipole BSSRDF.
    //
    // Reference:
    //
    //   A Practical Model for Subsurface Light Transport
    //   https://graphics.stanford.edu/papers/bssrdf/bssrdf.pdf
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
            const DipoleBSSRDFInputValues* values =
                reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

            if (values->m_weight == 0.0)
                return false;

            sample.set_is_directional(false);
            sample.set_eta(values->m_inside_ior / values->m_outside_ior);

            // Select the channel leading to the strongest scattering.
            const size_t channel = min_index(values->m_reflectance);
            sample.set_channel(channel);

            // todo: fix.
            const double reflectance = values->m_reflectance[channel];
            if (reflectance == 0.0)
                return false;

            sample.get_sampling_context().split_in_place(2, 1);
            const Vector2d s = sample.get_sampling_context().next_vector2<2>();

            // Sample a radius.
            const double sigma_tr = values->m_sigma_tr[channel];
            const double radius = dipole_sample(sigma_tr, s[0]);

            // Set the max radius.
            sample.set_rmax2(square(dipole_max_radius(sigma_tr)));

            // Sample an angle.
            const double phi = TwoPi * s[1];

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
            const DipoleBSSRDFInputValues* values =
                reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

            const Vector3d& x = outgoing_point.get_point();
            const double eta = values->m_inside_ior / values->m_outside_ior;
            const double Fdr = -1.440 / square(eta) + 0.710 / eta + 0.668 + 0.0636 * eta;
            const double A = (1.0 + Fdr) / (1.0 - Fdr);

            value.resize(values->m_sigma_a.size());

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const double sigma_a = values->m_sigma_a[i];
                const double sigma_s = values->m_sigma_s[i];
                const double sigma_s_prime = sigma_s * (1.0 - values->m_anisotropy);
                const double sigma_t_prime = sigma_s_prime + sigma_a;
                const double alpha_prime = sigma_s_prime / sigma_t_prime;
                const double sigma_tr = values->m_sigma_tr[i];
                const double D = 1.0 / (3.0 * sigma_t_prime);
                const double zr = 1.0 / sigma_t_prime;
                const double zv = zr + 4.0 * A * D;
                const Vector3d xr = incoming_point.get_point() - incoming_point.get_shading_normal() * zr;
                const Vector3d xv = incoming_point.get_point() + incoming_point.get_shading_normal() * zv;
                const double dr = norm(x - xr);
                const double dv = norm(x - xv);
                const double value_r = (sigma_tr * dr + 1.0) * exp(-sigma_tr * dr) / (sigma_t_prime * dr * dr * dr);
                const double value_v = (sigma_tr * dv + 1.0) * exp(-sigma_tr * dv) / (sigma_t_prime * dv * dv * dv);
                value[i] = static_cast<float>(alpha_prime / (4.0 * Pi) * (value_r + zv * value_v));
            }

            const double radius = norm(incoming_point.get_point() - outgoing_point.get_point());
            value *= static_cast<float>(radius * values->m_weight);
        }

        virtual double evaluate_pdf(
            const void*             data,
            const size_t            channel,
            const double            radius) const APPLESEED_OVERRIDE
        {
            const DipoleBSSRDFInputValues* values =
                reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

            const double reflectance = values->m_reflectance[channel];
            if (reflectance == 0.0)
                return 0.0;

            // PDF of the sampled radius.
            const double sigma_tr = values->m_sigma_tr[channel];
            const double pdf_radius = dipole_pdf(radius, sigma_tr);

            // PDF of the sampled angle.
            const double pdf_angle = RcpTwoPi;

            // Compute and return the final PDF.
            return pdf_radius * pdf_angle;
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
