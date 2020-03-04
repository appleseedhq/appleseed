
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
#include "constanthemisphereenvironmentedf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/matrix.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cassert>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Project; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Per-hemisphere constant-emittance environment EDF.
    //

    const char* Model = "constant_hemisphere_environment_edf";

    class ConstantHemisphereEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        ConstantHemisphereEnvironmentEDF(
            const char*             name,
            const ParamArray&       params)
          : EnvironmentEDF(name, params)
        {
            m_inputs.declare("upper_hemi_radiance", InputFormatSpectralIlluminance);
            m_inputs.declare("lower_hemi_radiance", InputFormatSpectralIlluminance);
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!EnvironmentEDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            if (!check_uniform("upper_hemi_radiance") || !check_uniform("lower_hemi_radiance"))
                return false;

            if (is_uniform_zero_spectrum("upper_hemi_radiance") && is_uniform_zero_spectrum("lower_hemi_radiance"))
                warn_zero_emission();

            m_inputs.evaluate_uniforms(&m_values);

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

            value =
                local_outgoing.y >= 0.0f
                    ? m_values.m_upper_hemi_radiance
                    : m_values.m_lower_hemi_radiance;
        }

        void evaluate(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            Spectrum&               value) const override
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Transformd::MatrixType& parent_to_local = transform.get_parent_to_local();
            const float local_outgoing_y =
                static_cast<float>(parent_to_local[ 4]) * outgoing.x +
                static_cast<float>(parent_to_local[ 5]) * outgoing.y +
                static_cast<float>(parent_to_local[ 6]) * outgoing.z;

            value =
                local_outgoing_y >= 0.0f
                    ? m_values.m_upper_hemi_radiance
                    : m_values.m_lower_hemi_radiance;
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
            const Transformd::MatrixType& parent_to_local = transform.get_parent_to_local();
            const float local_outgoing_y =
                static_cast<float>(parent_to_local[ 4]) * outgoing.x +
                static_cast<float>(parent_to_local[ 5]) * outgoing.y +
                static_cast<float>(parent_to_local[ 6]) * outgoing.z;

            value =
                local_outgoing_y >= 0.0f
                    ? m_values.m_upper_hemi_radiance
                    : m_values.m_lower_hemi_radiance;

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
            Spectrum    m_upper_hemi_radiance;              // radiance emitted by the upper hemisphere, in W.m^-2.sr^-1
            Spectrum    m_lower_hemi_radiance;              // radiance emitted by the lower hemisphere, in W.m^-2.sr^-1
        };

        InputValues     m_values;
    };
}


//
// ConstantHemisphereEnvironmentEDFFactory class implementation.
//

void ConstantHemisphereEnvironmentEDFFactory::release()
{
    delete this;
}

const char* ConstantHemisphereEnvironmentEDFFactory::get_model() const
{
    return Model;
}

Dictionary ConstantHemisphereEnvironmentEDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Per-Hemisphere Constant Environment EDF")
            .insert("help", "An environment made of two hemispheres of constant radiance");
}

DictionaryArray ConstantHemisphereEnvironmentEDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "upper_hemi_radiance")
            .insert("label", "Upper Hemisphere Radiance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", "0.7")
            .insert("help", "Upper hemisphere radiance"));

    metadata.push_back(
        Dictionary()
            .insert("name", "lower_hemi_radiance")
            .insert("label", "Lower Hemisphere Radiance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", "0.3")
            .insert("help", "Lower hemisphere radiance"));

    add_common_input_metadata(metadata);

    return metadata;
}

auto_release_ptr<EnvironmentEDF> ConstantHemisphereEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new ConstantHemisphereEnvironmentEDF(name, params));
}

}   // namespace renderer
