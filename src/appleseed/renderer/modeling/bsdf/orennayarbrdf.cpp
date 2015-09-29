
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Luis B. Barrancos, The appleseedhq Organization
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
#include "orennayarbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/basis.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <algorithm>
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
    // Oren-Nayar BRDF.
    //
    // Reference:
    //
    //   Generalization of Lambert's Reflectance Model
    //   http://www1.cs.columbia.edu/CAVE/publications/pdfs/Oren_SIGGRAPH94.pdf
    //

    const char* Model = "orennayar_brdf";

    class OrenNayarBRDFImpl
      : public BSDF
    {
      public:
        OrenNayarBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, ScatteringMode::Diffuse, params)
        {
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("roughness" , InputFormatScalar, "0.1");
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
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const APPLESEED_OVERRIDE
        {
            // No reflection below the shading surface.
            const Vector3d& n = sample.get_shading_normal();
            const double cos_on = dot(sample.m_outgoing.get_value(), n);
            if (cos_on < 0.0)
                return;

            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector3d wi = sample_hemisphere_cosine(s);

            // Transform the incoming direction to parent space.
            const Vector3d incoming = sample.get_shading_basis().transform_to_parent(wi);

            // No reflection below the shading surface.
            const double cos_in = dot(incoming, n);
            if (cos_in < 0.0)
                return;

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);
            if (values->m_roughness != 0.0)
            {
                oren_nayar_qualitative(
                    cos_on,
                    cos_in,
                    values->m_roughness,
                    values->m_reflectance,
                    values->m_reflectance_multiplier,
                    sample.m_outgoing.get_value(),
                    incoming,
                    n,
                    sample.m_value);
            }
            else
            {
                // Revert to Lambertian when roughness is zero.
                sample.m_value = values->m_reflectance;
                sample.m_value *= static_cast<float>(values->m_reflectance_multiplier * RcpPi);
            }

            // Compute the probability density of the sampled direction.
            sample.m_probability = wi.y * RcpPi;
            assert(sample.m_probability > 0.0);

            // Set the scattering mode.
            sample.m_mode = ScatteringMode::Diffuse;

            sample.m_incoming = Dual3d(incoming);
            sample.compute_reflected_differentials();
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
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_diffuse(modes))
                return 0.0;

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);
            if (values->m_roughness != 0.0)
            {
                oren_nayar_qualitative(
                    cos_on,
                    cos_in,
                    values->m_roughness,
                    values->m_reflectance,
                    values->m_reflectance_multiplier,
                    outgoing,
                    incoming,
                    n,
                    value);
            }
            else
            {
                // Revert to Lambertian when roughness is zero.
                value = values->m_reflectance;
                value *= static_cast<float>(values->m_reflectance_multiplier * RcpPi);
            }

            // Return the probability density of the sampled direction.
            return cos_in * RcpPi;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_diffuse(modes))
                return 0.0;

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            return cos_in * RcpPi;
        }

      private:
        typedef OrenNayarBRDFInputValues InputValues;

        static void oren_nayar_qualitative(
            const double        cos_on,
            const double        cos_in,
            const double        roughness,
            const Spectrum&     reflectance,
            const double        reflectance_multiplier,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const Vector3d&     n,
            Spectrum&           value)
        {
            const double sigma2 = square(roughness);
            const double theta_r = min(acos(cos_on), HalfPi);
            const double theta_i = acos(cos_in);
            const double alpha = max(theta_r, theta_i);
            const double beta = min(theta_r, theta_i);

            // Project outgoing and incoming vectors onto the tangent plane
            // and compute the cosine of the angle between them.
            const Vector3d V_perp_N = normalize(project(outgoing, n));
            const Vector3d I_perp_N = normalize(incoming - n * cos_in);
            const double delta_cos_phi = dot(V_perp_N, I_perp_N);

            // Compute C1 coefficient.
            const double C1 = 1.0 - 0.5 * (sigma2 / (sigma2 + 0.33));

            // Compute C2 coefficient.
            const double sigma2_009 = sigma2 / (sigma2 + 0.09);
            const double C2 =
                  0.45
                * sigma2_009
                * (delta_cos_phi >= 0.0
                      ? sin(alpha)
                      : sin(alpha) - pow_int<3>(2.0 * beta * RcpPi));
            assert(C2 >= 0.0);

            // Compute C3 coefficient.
            const double C3 =
                  0.125
                * sigma2_009
                * square(4.0 * alpha * beta * RcpPiSquare);
            assert(C3 >= 0.0);

            // Direct illumination component.
            value = reflectance;
            value *=
                static_cast<float>(
                    reflectance_multiplier * RcpPi * (
                          C1
                        + delta_cos_phi * C2 * tan(beta)
                        + (1.0 - abs(delta_cos_phi)) * C3 * tan(0.5 * (alpha + beta))));

            // Add interreflection component.
            Spectrum r2 = reflectance;
            r2 *= r2;
            r2 *=
                static_cast<float>(
                      0.17
                    * square(reflectance_multiplier) * RcpPi
                    * cos_in
                    * sigma2 / (sigma2 + 0.13)
                    * (1.0 - delta_cos_phi * square(2.0 * beta * RcpPi)));
            value += r2;

            assert(min_value(value) >= 0.0f);
        }
    };

    typedef BSDFWrapper<OrenNayarBRDFImpl> OrenNayarBRDF;
}


//
// OrenNayarBRDFFactory class implementation.
//

const char* OrenNayarBRDFFactory::get_model() const
{
    return Model;
}

Dictionary OrenNayarBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Oren-Nayar BRDF");
}

DictionaryArray OrenNayarBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance")
            .insert("label", "Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.1"));

    return metadata;
}

auto_release_ptr<BSDF> OrenNayarBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new OrenNayarBRDF(name, params));
}

}   // namespace renderer
