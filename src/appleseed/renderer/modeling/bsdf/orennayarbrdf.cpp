
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

    const char* Model = "orennayar_brdf";

    class OrenNayarBRDFImpl
      : public BSDF
    {
      public:
        OrenNayarBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, BSDFSample::Diffuse, params)
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
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const
        {
            // No reflection below the shading surface.
            const Vector3d& n = sample.get_shading_normal();
            const double cos_on = dot(sample.get_outgoing_vector(), n);
            if (cos_on < 0.0)
                return;

            // Compute the incoming direction in local space.
            sample.get_sampling_context().split_in_place(2, 1);
            const Vector2d s = sample.get_sampling_context().next_vector2<2>();
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
                    sample.get_outgoing_vector(),
                    incoming,
                    n,
                    sample.value());
            }
            else
                sample.value() = values->m_reflectance;

            sample.value() *= static_cast<float>(values->m_reflectance_multiplier * RcpPi);

            // Compute the probability density of the sampled direction.
            sample.set_probability(wi.y * RcpPi);
            assert(sample.get_probability() > 0.0);

            // Set the scattering mode.
            sample.set_mode(BSDFSample::Diffuse);

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
            if (!(modes & BSDFSample::Diffuse))
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
                oren_nayar_qualitative(cos_on, cos_in, values->m_roughness, values->m_reflectance, outgoing, incoming, n, value);
            else
                value = values->m_reflectance;
            value *= static_cast<float>(values->m_reflectance_multiplier * RcpPi);

            // Return the probability density of the sampled direction.
            return cos_in * RcpPi;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const
        {
            if (!(modes & BSDFSample::Diffuse))
                return 0.0;

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            return cos_in * RcpPi;
        }

        void oren_nayar_qualitative(
            const double        cos_on,
            const double        cos_in,
            const double        roughness,
            const Spectrum&     reflectance,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const Vector3d&     n,
            Spectrum&           value) const
        {
            const double theta_r = min(HalfPi, acos(cos_on));
            const double theta_i = acos(cos_in);
            const double alpha = max(theta_i, theta_r);
            const double beta = min(theta_i, theta_r);

            const double sigma2 = square(roughness);

            const double C1 = 1.0 - 0.5 * (sigma2 / (sigma2 + 0.33));

            double C2 = 0.45 * sigma2 / (sigma2 + 0.09);

            const Vector3d V_perp_N = normalize(outgoing - n * dot(outgoing, n));

            const double cos_phi_diff = dot(V_perp_N, normalize(incoming - n * cos_in));

            if (cos_phi_diff >= 0.0)
                C2 *= sin(alpha);
            else
            {
                const double temp = 2.0 * beta * RcpPi;
                C2 *= sin(alpha) - square(temp) * temp;
            }
            assert(C2 >= 0.0);

            const double C3 = 0.125 * (sigma2 / (sigma2 + 0.09) * square(4.0 * alpha * beta * RcpPiSq)) * tan((alpha + beta) * 0.5);
            assert(C3 >= 0.0);

            value = reflectance ;
            value *= static_cast<float>(C1 + (abs(cos_phi_diff) * C2 * tan(beta)) + (1 - abs(cos_phi_diff)) * C3);
            value += square(reflectance) * static_cast<float>(0.17 * cos_in * (sigma2 / (sigma2 + 0.13)) *
                                                             (1 - cos_phi_diff * square(2 * beta * RcpPi)));
            assert(min_value(value) >= 0.0 );
        }

      private:
        typedef OrenNayarBRDFInputValues InputValues;
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
