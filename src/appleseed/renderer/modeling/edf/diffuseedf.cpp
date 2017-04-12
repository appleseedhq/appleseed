
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
#include "diffuseedf.h"

// appleseed.renderer headers.
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cassert>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Diffuse EDF.
    //

    const char* Model = "diffuse_edf";

    class DiffuseEDF
      : public EDF
    {
      public:
        DiffuseEDF(
            const char*             name,
            const ParamArray&       params)
          : EDF(name, params)
        {
            m_inputs.declare("radiance", InputFormatSpectralIlluminance);
            m_inputs.declare("radiance_multiplier", InputFormatFloat, "1.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!EDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            check_non_zero_emission("radiance", "radiance_multiplier");

            return true;
        }

        virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector2f&         s,
            Vector3f&               outgoing,
            Spectrum&               value,
            float&                  probability) const APPLESEED_OVERRIDE
        {
            assert(is_normalized(geometric_normal));

            const Vector3f wo = sample_hemisphere_cosine(s);
            outgoing = shading_basis.transform_to_parent(wo);

            const InputValues* values = static_cast<const InputValues*>(data);
            value = values->m_radiance;
            value *= values->m_radiance_multiplier;

            probability = wo.y * RcpPi<float>();
            assert(probability > 0.0f);
        }

        virtual void evaluate(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            assert(is_normalized(geometric_normal));
            assert(is_normalized(outgoing));

            const float cos_on = dot(outgoing, shading_basis.get_normal());

            // No emission in or below the shading surface.
            if (cos_on <= 0.0f)
            {
                value.set(0.0f);
                return;
            }

            const InputValues* values = static_cast<const InputValues*>(data);
            value = values->m_radiance;
            value *= values->m_radiance_multiplier;
        }

        virtual void evaluate(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            Spectrum&               value,
            float&                  probability) const APPLESEED_OVERRIDE
        {
            assert(is_normalized(geometric_normal));
            assert(is_normalized(outgoing));

            const float cos_on = dot(outgoing, shading_basis.get_normal());

            // No emission in or below the shading surface.
            if (cos_on <= 0.0f)
            {
                value.set(0.0f);
                probability = 0.0f;
                return;
            }

            const InputValues* values = static_cast<const InputValues*>(data);
            value = values->m_radiance;
            value *= values->m_radiance_multiplier;

            probability = cos_on * RcpPi<float>();
        }

        virtual float evaluate_pdf(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing) const APPLESEED_OVERRIDE
        {
            assert(is_normalized(geometric_normal));
            assert(is_normalized(outgoing));

            const float cos_on = dot(outgoing, shading_basis.get_normal());

            // No emission in or below the shading surface.
            if (cos_on <= 0.0f)
                return 0.0f;

            return cos_on * RcpPi<float>();
        }

        virtual float get_uncached_max_contribution() const APPLESEED_OVERRIDE
        {
            return get_max_contribution("radiance", "radiance_multiplier");
        }

      private:
        typedef DiffuseEDFInputValues InputValues;
    };
}


//
// DiffuseEDFFactory class implementation.
//

const char* DiffuseEDFFactory::get_model() const
{
    return Model;
}

Dictionary DiffuseEDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Diffuse EDF")
            .insert("default_model", "true");
}

DictionaryArray DiffuseEDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "radiance")
            .insert("label", "Radiance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "radiance_multiplier")
            .insert("label", "Radiance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    add_common_input_metadata(metadata);

    return metadata;
}

auto_release_ptr<EDF> DiffuseEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<EDF>(new DiffuseEDF(name, params));
}

auto_release_ptr<EDF> DiffuseEDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<EDF>(new DiffuseEDF(name, params));
}

}   // namespace renderer
