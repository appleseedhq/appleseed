
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/backfacingpolicy.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

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

    template <typename BackfacingPolicy>
    class DiffuseBTDFImpl
      : public BSDF
    {
      public:
        DiffuseBTDFImpl(
            const char*             name,
            const ParamArray&       params)
          : BSDF(name, Transmissive, ScatteringMode::Diffuse, params)
        {
            m_inputs.declare("transmittance", InputFormatSpectralReflectance);
            m_inputs.declare("transmittance_multiplier", InputFormatFloat, "1.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual size_t compute_input_data_size() const APPLESEED_OVERRIDE
        {
            return sizeof(InputValues);
        }

        virtual void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const APPLESEED_OVERRIDE
        {
            InputValues* values = static_cast<InputValues*>(data);
            new (&values->m_precomputed) InputValues::Precomputed();
            values->m_precomputed.m_backfacing = !shading_point.is_entering();
        }

        virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const int               modes,
            BSDFSample&             sample) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_diffuse(modes))
                return;

            const InputValues* values = static_cast<const InputValues*>(data);
            const BackfacingPolicy backfacing_policy(sample.m_shading_basis, values->m_precomputed.m_backfacing);
            const Vector3f wo = backfacing_policy.transform_to_local(sample.m_outgoing.get_value());

            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2f s = sampling_context.next2<Vector2f>();
            const Vector3f wi = sample_hemisphere_cosine(s);

            // Transform the incoming direction to parent space.
            sample.m_incoming = Dual3f(
                wo.y < 0.0f
                    ?  backfacing_policy.transform_to_parent(wi)
                    : -backfacing_policy.transform_to_parent(wi));

            // Compute the BRDF value.
            sample.m_value = values->m_transmittance;
            sample.m_value *= values->m_transmittance_multiplier * RcpPi<float>();

            // Compute the probability density of the sampled direction.
            sample.m_probability = abs(wi.y) * RcpPi<float>();
            assert(sample.m_probability > 0.0f);

            // Set the scattering mode.
            sample.m_mode = ScatteringMode::Diffuse;
        }

        virtual float evaluate(
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_diffuse(modes))
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);
            const BackfacingPolicy backfacing_policy(shading_basis, values->m_precomputed.m_backfacing);

            const Vector3f& n = backfacing_policy.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = dot(outgoing, n);

            if (cos_in * cos_on < 0.0f)
            {
                // Compute the BRDF value.
                value = values->m_transmittance;
                value *= values->m_transmittance_multiplier * RcpPi<float>();

                // Return the probability density of the sampled direction.
                return abs(cos_in) * RcpPi<float>();
            }
            else
            {
                // No transmission in the same hemisphere as outgoing.
                return 0.0f;
            }
        }

        virtual float evaluate_pdf(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_diffuse(modes))
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);
            const BackfacingPolicy backfacing_policy(shading_basis, values->m_precomputed.m_backfacing);

            const Vector3f& n = backfacing_policy.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = dot(outgoing, n);

            if (cos_in * cos_on < 0.0f)
                return abs(cos_in) * RcpPi<float>();
            else
            {
                // No transmission in the same hemisphere as outgoing.
                return 0.0f;
            }
        }

      private:
        typedef DiffuseBTDFInputValues InputValues;
    };

    typedef BSDFWrapper<DiffuseBTDFImpl<FlipBackfacingNormalsPolicy> > AppleseedDiffuseBTDF;
    typedef BSDFWrapper<DiffuseBTDFImpl<UseOriginalNormalsPolicy> > OSLDiffuseBTDF;
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
    return auto_release_ptr<BSDF>(new AppleseedDiffuseBTDF(name, params));
}

auto_release_ptr<BSDF> DiffuseBTDFFactory::create_osl(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new OSLDiffuseBTDF(name, params));
}

auto_release_ptr<BSDF> DiffuseBTDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSDF>(new AppleseedDiffuseBTDF(name, params));
}

}   // namespace renderer
