
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "diffusebtdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <algorithm>
#include <cmath>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Diffuse BTDF.
    //

    const char* Model = "diffuse_btdf";

    class DiffuseBTDFImpl
      : public BSDF
    {
      public:
        DiffuseBTDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Transmissive, BSDFSample::Diffuse, params)
        {
            m_inputs.declare("transmittance", InputFormatSpectralReflectance);
            m_inputs.declare("transmittance_multiplier", InputFormatScalar, "1.0");
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
            // Compute the incoming direction in local space.
            sample.get_sampling_context().split_in_place(2, 1);
            const Vector2d s = sample.get_sampling_context().next_vector2<2>();
            const Vector3d wi = sample_hemisphere_cosine(s);

            // Transform the incoming direction to parent space.
            sample.set_incoming(-sample.get_shading_basis().transform_to_parent(wi));

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);
            sample.value() = values->m_transmittance;
            sample.value() *= static_cast<float>(values->m_transmittance_multiplier * RcpPi);

            // Compute the probability density of the sampled direction.
            sample.set_probability(wi.y * RcpPi);
            assert(sample.get_probability() > 0.0);

            // Set the scattering mode.
            sample.set_mode(BSDFSample::Diffuse);
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

            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = abs(dot(incoming, n));

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);
            value = values->m_transmittance;
            value *= static_cast<float>(values->m_transmittance_multiplier * RcpPi);

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

            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = abs(dot(incoming, n));

            return cos_in * RcpPi;
        }

      private:
        typedef DiffuseBTDFInputValues InputValues;
    };

    typedef BSDFWrapper<DiffuseBTDFImpl> DiffuseBTDF;
}


//
// DiffuseBTDFFactory class implementation.
//

const char* DiffuseBTDFFactory::get_model() const
{
    return Model;
}

Dictionary DiffuseBTDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Diffuse BTDF");
}

DictionaryArray DiffuseBTDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "transmittance")
            .insert("label", "Transmittance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "transmittance_multiplier")
            .insert("label", "Transmittance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<BSDF> DiffuseBTDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new DiffuseBTDF(name, params));
}

}   // namespace renderer
