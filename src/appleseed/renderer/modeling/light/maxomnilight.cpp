
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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
#include "maxomnilight.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/utility/autodeskmax.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/distance.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cmath>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // A light source compatible with the Omni light of Autodesk 3ds Max.
    //

    const char* Model = "max_omni_light";

    class MaxOmniLight
      : public Light
    {
      public:
        MaxOmniLight(
            const char*             name,
            const ParamArray&       params)
          : Light(name, params)
        {
            m_inputs.declare("intensity", InputFormat::SpectralIlluminance);
            m_inputs.declare("intensity_multiplier", InputFormat::Float, "1.0");
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
            if (!Light::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            if (!check_uniform("intensity") || !check_uniform("intensity_multiplier"))
                return false;

            check_non_zero_emission("intensity", "intensity_multiplier");

            m_inputs.evaluate_uniforms(&m_values);
            m_values.m_intensity *= m_values.m_intensity_multiplier;

            m_decay_start = m_params.get_optional<float>("decay_start", 0.0f);
            m_decay_exponent = m_params.get_optional<float>("decay_exponent", 2.0f);

            return true;
        }

        void sample(
            const ShadingContext&   shading_context,
            const Transformd&       light_transform,
            const Vector3d&         target_point,
            const Vector2d&         s,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            position = light_transform.get_parent_origin();
            outgoing = normalize(target_point - position);
            value = m_values.m_intensity;
            probability = 1.0f;
        }

        void sample(
            const ShadingContext&   shading_context,
            const Transformd&       light_transform,
            const Vector2d&         s,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            position = light_transform.get_parent_origin();
            outgoing = sample_sphere_uniform(s);
            value = m_values.m_intensity;

            // todo: only correct if m_decay_exponent == 2.
            probability = RcpFourPi<float>();
        }

        float compute_distance_attenuation(
            const Vector3d&         target,
            const Vector3d&         position) const override
        {
            return
                autodesk_max_decay(
                    std::sqrt(static_cast<float>(square_distance(target, position))),
                    m_decay_start,
                    m_decay_exponent);
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            Spectrum    m_intensity;                // emitted intensity in W.sr^-1
            float       m_intensity_multiplier;     // emitted intensity multiplier
        };

        InputValues     m_values;

        float           m_decay_start;              // distance at which light decay starts
        float           m_decay_exponent;           // exponent of the light decay function
    };
}


//
// MaxOmniLightFactory class implementation.
//

void MaxOmniLightFactory::release()
{
    delete this;
}

const char* MaxOmniLightFactory::get_model() const
{
    return Model;
}

Dictionary MaxOmniLightFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Autodesk 3ds Max Omni Light")
            .insert("help", "A light source compatible with the Omni light of Autodesk 3ds Max");
}

DictionaryArray MaxOmniLightFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "intensity")
            .insert("label", "Intensity")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", "1.0")
            .insert("help", "Light intensity"));

    metadata.push_back(
        Dictionary()
            .insert("name", "intensity_multiplier")
            .insert("label", "Intensity Multiplier")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Light intensity multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "decay_start")
            .insert("label", "Decay Start")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("help", "Distance at which light decay starts"));

    metadata.push_back(
        Dictionary()
            .insert("name", "decay_exponent")
            .insert("label", "Decay Exponent")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "4.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "2.0")
            .insert("help", "Exponent of the light decay function"));

    add_common_input_metadata(metadata);

    return metadata;
}

auto_release_ptr<Light> MaxOmniLightFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Light>(new MaxOmniLight(name, params));
}

}   // namespace renderer
