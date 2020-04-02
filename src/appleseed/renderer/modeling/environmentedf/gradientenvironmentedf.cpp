
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
#include "gradientenvironmentedf.h"

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
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
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
    // Gradient environment EDF.
    //

    const char* Model = "gradient_environment_edf";

    class GradientEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        GradientEnvironmentEDF(
            const char*             name,
            const ParamArray&       params)
          : EnvironmentEDF(name, params)
        {
            m_inputs.declare("horizon_radiance", InputFormatSpectralIlluminance);
            m_inputs.declare("zenith_radiance", InputFormatSpectralIlluminance);
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

            if (!check_uniform("horizon_radiance") || !check_uniform("zenith_radiance"))
                return false;

            if (is_uniform_zero_spectrum("horizon_radiance") && is_uniform_zero_spectrum("zenith_radiance"))
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

            compute_gradient(local_outgoing.y, value);
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

            compute_gradient(local_outgoing_y, value);
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

            compute_gradient(local_outgoing_y, value);
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
            Spectrum    m_horizon_radiance;             // radiance emitted at horizon, in W.m^-2.sr^-1
            Spectrum    m_zenith_radiance;              // radiance emitted at zenith, in W.m^-2.sr^-1
        };

        InputValues     m_values;

        void compute_gradient(const float y, Spectrum& output) const
        {
            // Compute the blending factor between the horizon and zenith colors.
            const float angle = std::acos(std::abs(y));
            const float blend = angle * (1.0f / HalfPi<float>());

            // Blend the horizon and zenith radiances.
            Spectrum horizon_radiance = m_values.m_horizon_radiance;
            horizon_radiance *= blend;
            output = m_values.m_zenith_radiance;
            output *= 1.0f - blend;
            output += horizon_radiance;
        }
    };
}


//
// GradientEnvironmentEDFFactory class implementation.
//

void GradientEnvironmentEDFFactory::release()
{
    delete this;
}

const char* GradientEnvironmentEDFFactory::get_model() const
{
    return Model;
}

Dictionary GradientEnvironmentEDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Gradient Environment EDF")
            .insert("help", "An environment made of a vertical radiance gradient, from zenith to horizon");
}

DictionaryArray GradientEnvironmentEDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "horizon_radiance")
            .insert("label", "Horizon Radiance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", "0.3")
            .insert("help", "Horizon radiance"));

    metadata.push_back(
        Dictionary()
            .insert("name", "zenith_radiance")
            .insert("label", "Zenith Radiance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", "0.7")
            .insert("help", "Zenith radiance"));

    add_common_input_metadata(metadata);

    return metadata;
}

auto_release_ptr<EnvironmentEDF> GradientEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new GradientEnvironmentEDF(name, params));
}

}   // namespace renderer
