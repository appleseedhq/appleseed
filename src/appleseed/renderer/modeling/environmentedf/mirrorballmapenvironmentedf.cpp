
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "mirrorballmapenvironmentedf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/sourceinputs.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/matrix.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cassert>
#include <cmath>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Project; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Mirror ball environment map EDF.
    //
    // References:
    //
    //   http://www.debevec.org/probes/
    //   http://gl.ict.usc.edu/Data/HighResProbes/
    //

    const char* Model = "mirrorball_map_environment_edf";

    class MirrorBallMapEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        MirrorBallMapEnvironmentEDF(
            const char*             name,
            const ParamArray&       params)
          : EnvironmentEDF(name, params)
        {
            m_inputs.declare("radiance", InputFormatSpectralIlluminance);
            m_inputs.declare("radiance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("exposure", InputFormatFloat, "0.0");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool on_render_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnRenderBeginRecorder&  recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!EnvironmentEDF::on_render_begin(project, parent, recorder, abort_switch))
                return false;

            check_non_zero_emission("radiance", "radiance_multiplier");

            InputValues values;
            get_inputs().evaluate_uniforms(&values);
            m_exposure_multiplier = std::pow(2.0f, values.m_exposure);

            return true;
        }

        void sample(
            const ShadingContext&   shading_context,
            const Vector2f&         s,
            Vector3f&               outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            const Vector3f local_outgoing = sample_sphere_uniform(s);
            probability = RcpFourPi<float>();

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            outgoing = transform.vector_to_parent(local_outgoing);

            lookup_envmap(shading_context, local_outgoing, value);
        }

        void evaluate(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            Spectrum&               value) const override
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Vector3f local_outgoing = transform.vector_to_local(outgoing);

            lookup_envmap(shading_context, local_outgoing, value);
        }

        void evaluate(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Vector3f local_outgoing = transform.vector_to_local(outgoing);

            lookup_envmap(shading_context, local_outgoing, value);
            probability = RcpFourPi<float>();
        }

        float evaluate_pdf(
            const Vector3f&         outgoing) const override
        {
            assert(is_normalized(outgoing));
            return RcpFourPi<float>();
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            Spectrum    m_radiance;             // emitted radiance in W.m^-2.sr^-1
            float       m_radiance_multiplier;  // emitted radiance multiplier
            float       m_exposure;             // emitted radiance multiplier in f-stops
        };

        float m_exposure_multiplier;

        void lookup_envmap(
            const ShadingContext&   shading_context,
            const Vector3f&         direction,
            Spectrum&               value) const
        {
            // Compute the texture coordinates corresponding to this direction.
            const float d = std::sqrt(square(direction[0]) + square(direction[1]));
            const float r = RcpTwoPi<float>() * std::acos(direction[2]) / d;
            const Vector2f uv(0.5f + direction[0] * r, 0.5f + direction[1] * r);

            // Evaluate the input.
            InputValues values;
            m_inputs.evaluate(shading_context.get_texture_cache(), SourceInputs(uv), &values);
            if (is_finite(values.m_radiance))
            {
                value = values.m_radiance;
                value *= values.m_radiance_multiplier * m_exposure_multiplier;
            }
            else value.set(0.0f);
        }
    };
}


//
// MirrorBallMapEnvironmentEDFFactory class implementation.
//

void MirrorBallMapEnvironmentEDFFactory::release()
{
    delete this;
}

const char* MirrorBallMapEnvironmentEDFFactory::get_model() const
{
    return Model;
}

Dictionary MirrorBallMapEnvironmentEDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Mirror Ball Map Environment EDF")
            .insert("help", "Mirror ball image-based lighting environment");
}

DictionaryArray MirrorBallMapEnvironmentEDFFactory::get_input_metadata() const
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
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "1.0")
            .insert("help", "Environment texture"));

    metadata.push_back(
        Dictionary()
            .insert("name", "radiance_multiplier")
            .insert("label", "Radiance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Environment texture radiance multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "exposure")
            .insert("label", "Exposure")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-8.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "8.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("help", "Environment exposure"));

    add_common_input_metadata(metadata);

    return metadata;
}

auto_release_ptr<EnvironmentEDF> MirrorBallMapEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new MirrorBallMapEnvironmentEDF(name, params));
}

}   // namespace renderer
