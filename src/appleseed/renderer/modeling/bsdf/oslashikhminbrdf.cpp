
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "oslashikhminbrdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/basis.h"
#include "foundation/math/fp.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cassert>
#include <cmath>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // OSL Ashikhmin-Shirley BRDF.
    //
    // Simplified version of the Ashikhmin-Shirley model used in OSL shaders.
    // It ignores the diffuse component and Fresnel term.
    //
    // References:
    //
    //   http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.4558&rep=rep1&type=pdf
    //   http://jesper.kalliope.org/blog/library/dbrdfs.pdf
    //

    const char* Model = "osl_ashikhmin_brdf";

    class OSLAshikhminBRDFImpl
      : public BSDF
    {
      public:
        OSLAshikhminBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, BSDFSample::Glossy, params)
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

        FORCE_INLINE virtual void sample(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const
        {
            // No reflection below the shading surface.
            const Vector3d& shading_normal = sample.get_shading_normal();
            const double cos_on = dot(sample.get_outgoing_vector(), shading_normal);
            if (cos_on < 0.0)
                return;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute shininess-related values.
            SVal sval;
            compute_sval(sval, values->m_nu, values->m_nv);

            sample.get_sampling_context().split_in_place(2, 1);
            const Vector2d s = sample.get_sampling_context().next_vector2<2>();

            double exp;
            double cos_phi, sin_phi;

            if (sval.m_isotropic)
            {
                const double phi = s[0] * TwoPi;

                cos_phi = cos(phi);
                sin_phi = sin(phi);

                exp = values->m_nu;
            }
            else
            {
                const double phi = sample_anisotropic_glossy(sval.m_k, s[0]);

                cos_phi = cos(phi);
                sin_phi = sin(phi);

                const double exp_u = values->m_nu * cos_phi * cos_phi;
                const double exp_v = values->m_nv * sin_phi * sin_phi;

                exp = exp_u + exp_v;
            }

            const double cos_theta = pow(1.0 - s[1], 1.0 / (exp + 1.0));
            const double sin_theta = sqrt(1.0 - cos_theta * cos_theta);

            // Compute the halfway vector in world space.
            const Vector3d h = sample.get_shading_basis().transform_to_parent(
                Vector3d::unit_vector(cos_theta, sin_theta, cos_phi, sin_phi));

            // Compute the incoming direction in world space.
            const Vector3d incoming(
                force_above_surface(
                    reflect(sample.get_outgoing_vector(), h),
                    sample.get_geometric_normal()));

            // No reflection below the shading surface.
            const double cos_in = dot(incoming, shading_normal);
            if (cos_in < 0.0)
                return;

            // Compute dot products.
            const double cos_oh = abs(dot(sample.get_outgoing_vector(), h));
            const double cos_hn = dot(h, shading_normal);

            // Evaluate the glossy component of the BRDF (equation 4).
            const double num = sval.m_kg * pow(cos_hn, exp);
            const double den = cos_oh * (cos_in + cos_on - cos_in * cos_on);
            sample.value().set(static_cast<float>(num / den));

            // Evaluate the PDF of the glossy component (equation 8).
            sample.set_probability(num / cos_oh);     // omit division by 4 since num = pdf(h) / 4
            assert(sample.get_probability() >= 0.0);

            // Set the scattering mode.
            sample.set_mode(BSDFSample::Glossy);

            sample.set_incoming(incoming);
        }

        FORCE_INLINE virtual double evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes,
            Spectrum&           value) const
        {
            if (!(modes & BSDFSample::Glossy))
                return 0.0;

            // No reflection below the shading surface.
            const Vector3d& shading_normal = shading_basis.get_normal();
            const double cos_in = dot(incoming, shading_normal);
            const double cos_on = dot(outgoing, shading_normal);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute shininess-related values.
            SVal sval;
            compute_sval(sval, values->m_nu, values->m_nv);

            value.set(0.0f);

            // Compute the halfway vector in world space.
            const Vector3d h = normalize(incoming + outgoing);

            // Compute dot products.
            const double cos_oh = dot(outgoing, h);
            const double cos_hn = dot(h, shading_normal);
            const double cos_hu = dot(h, shading_basis.get_tangent_u());
            const double cos_hv = dot(h, shading_basis.get_tangent_v());

            // Evaluate the glossy component of the BRDF (equation 4).
            const double exp_num_u = values->m_nu * cos_hu * cos_hu;
            const double exp_num_v = values->m_nv * cos_hv * cos_hv;
            const double exp_den = 1.0 - cos_hn * cos_hn;
            const double exp = (exp_num_u + exp_num_v) / exp_den;
            const double num = exp_den == 0.0 ? 0.0 : sval.m_kg * pow(cos_hn, exp);
            const double den = cos_oh * (cos_in + cos_on - cos_in * cos_on);
            value.set(static_cast<float>(num / den));

            // Evaluate the PDF of the glossy component (equation 8).
            const double probability = num / cos_oh;     // omit division by 4 since num = pdf(h) / 4
            assert(probability >= 0.0);
            return probability;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const
        {
            if (!(modes & BSDFSample::Glossy))
                return 0.0;

            // No reflection below the shading surface.
            const Vector3d& shading_normal = shading_basis.get_normal();
            const double cos_in = dot(incoming, shading_normal);
            const double cos_on = dot(outgoing, shading_normal);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute shininess-related values.
            SVal sval;
            compute_sval(sval, values->m_nu, values->m_nv);

            // Compute the halfway vector in world space.
            const Vector3d h = normalize(incoming + outgoing);

            // Compute dot products.
            const double cos_oh = dot(outgoing, h);
            const double cos_hn = dot(h, shading_normal);
            const double cos_hu = dot(h, shading_basis.get_tangent_u());
            const double cos_hv = dot(h, shading_basis.get_tangent_v());

            // Evaluate the PDF for the halfway vector (equation 6).
            const double exp_num_u = values->m_nu * cos_hu * cos_hu;
            const double exp_num_v = values->m_nv * cos_hv * cos_hv;
            const double exp_den = 1.0 - cos_hn * cos_hn;
            const double exp = (exp_num_u + exp_num_v) / exp_den;
            const double num = exp_den == 0.0 ? 0.0 : sval.m_kg * pow(cos_hn, exp);

            // Evaluate the PDF of the glossy component (equation 8).
            const double probability = num / cos_oh;     // omit division by 4 since num = pdf(h) / 4
            assert(probability >= 0.0);
            return probability;
        }

      private:
        typedef OSLAshikhminBRDFInputValues InputValues;

        // Precomputed shininess-related values.
        struct SVal
        {
            double      m_kg;               // constant factor of glossy component
            double      m_k;                // constant factor needed during hemisphere (isotropic case only)
            bool        m_isotropic;        // true if the U and V shininess values are the same
        };

        static void compute_sval(SVal& sval, const double nu, const double nv)
        {
            // Check for isotropicity.
            sval.m_isotropic = feq(nu, nv, 1.0e-6);

            // Precompute constant factor of glossy component (equations 4 and 6).
            sval.m_kg = sqrt((nu + 1.0) * (nv + 1.0)) / (8.0 * Pi);

            if (!sval.m_isotropic)
            {
                // Precompute constant factor needed during hemisphere sampling.
                sval.m_k = sqrt((nu + 1.0) / (nv + 1.0));
            }
        }

        static double sample_anisotropic_glossy(const double k, const double s)
        {
            if (s < 0.25)
            {
                // First quadrant.
                const double b = tan(HalfPi * (4.0 * s));
                return atan(k * b);
            }
            else if (s < 0.5)
            {
                // Second quadrant.
                const double b = tan(HalfPi * (4.0 * s - 1.0));
                return atan(k * b) + HalfPi;
            }
            else if (s < 0.75)
            {
                // Third quadrant.
                const double b = tan(HalfPi * (4.0 * s - 2.0));
                return atan(k * b) + Pi;
            }
            else
            {
                // Fourth quadrant.
                const double b = tan(HalfPi * (4.0 * s - 3.0));
                return atan(k * b) + Pi + HalfPi;
            }
        }
    };

    typedef BSDFWrapper<OSLAshikhminBRDFImpl> OSLAshikhminBRDF;
}


//
// OSLAshikhminBRDFFactory class implementation.
//

auto_release_ptr<BSDF> OSLAshikhminBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new OSLAshikhminBRDF(name, params));
}

}   // namespace renderer
