
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/utility/autodeskmax.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/distance.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
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
    // A light source compatible with the Omni light of Autodesk 3ds Max.
    //

    const char* Model = "max_omni_light";

    class MaxOmniLight
      : public Light
    {
      public:
        MaxOmniLight(
            const char*         name,
            const ParamArray&   params)
          : Light(name, params)
        {
            m_inputs.declare("intensity", InputFormatSpectralIlluminance);
            m_inputs.declare("intensity_multiplier", InputFormatScalar, "1.0");
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
            const Project&      project,
            const Assembly&     assembly,
            IAbortSwitch*       abort_switch) APPLESEED_OVERRIDE
        {
            if (!Light::on_frame_begin(project, assembly, abort_switch))
                return false;

            if (!check_uniform("intensity") || !check_uniform("intensity_multiplier"))
                return false;

            check_non_zero_emission("intensity", "intensity_multiplier");

            m_inputs.evaluate_uniforms(&m_values);
            m_values.m_intensity *= static_cast<float>(m_values.m_intensity_multiplier);

            m_decay_start = m_params.get_optional<double>("decay_start", 0.0);
            m_decay_exponent = m_params.get_optional<double>("decay_exponent", 2.0);

            return true;
        }

        virtual void sample(
            InputEvaluator&     input_evaluator,
            const Transformd&   light_transform,
            const Vector2d&     s,
            Vector3d&           position,
            Vector3d&           outgoing,
            Spectrum&           value,
            double&             probability) const APPLESEED_OVERRIDE
        {
            position = light_transform.get_parent_origin();
            outgoing = sample_sphere_uniform(s);
            value = m_values.m_intensity;
            probability = RcpFourPi;
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Transformd&   light_transform,
            const Vector3d&     target,
            Vector3d&           position,
            Vector3d&           outgoing,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            position = light_transform.get_parent_origin();
            outgoing = normalize(target - position);
            value = m_values.m_intensity;
        }

        double compute_distance_attenuation(
            const Vector3d&     target,
            const Vector3d&     position) const APPLESEED_OVERRIDE
        {
            return
                autodesk_max_decay(
                    sqrt(square_distance(target, position)),
                    m_decay_start,
                    m_decay_exponent);
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            Spectrum    m_intensity;                // emitted intensity in W.sr^-1
            double      m_intensity_multiplier;     // emitted intensity multiplier
        };

        InputValues     m_values;

        double          m_decay_start;              // distance at which light decay starts
        double          m_decay_exponent;           // exponent of the light decay function
    };
}


//
// MaxOmniLightFactory class implementation.
//

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
            .insert("min_value", "0.0")
            .insert("max_value", "10.0")
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Light intensity multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "decay_start")
            .insert("label", "Decay Start")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "10.0")
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("help", "Distance at which light decay starts"));

    metadata.push_back(
        Dictionary()
            .insert("name", "decay_exponent")
            .insert("label", "Decay Exponent")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "4.0")
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
