
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
#include "directionallight.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/light/lighttarget.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/basis.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Directional light.
    //

    const char* Model = "directional_light";

    class DirectionalLight
      : public Light
    {
      public:
        DirectionalLight(
            const char*             name,
            const ParamArray&       params)
          : Light(name, params)
        {
            m_inputs.declare("irradiance", InputFormatSpectralIlluminance);
            m_inputs.declare("irradiance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("exposure", InputFormatScalar, "0.0");
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
            const Assembly&         assembly,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!Light::on_frame_begin(project, assembly, abort_switch))
                return false;

            if (
                !check_uniform("irradiance") ||
                !check_uniform("irradiance_multiplier") ||
                !check_uniform("exposure"))
            {
                return false;
            }

            check_non_zero_emission("irradiance", "irradiance_multiplier");

            const Scene::RenderData& scene_data = project.get_scene()->get_render_data();
            m_scene_center = scene_data.m_center;
            m_scene_radius = scene_data.m_radius;
            m_safe_scene_diameter = scene_data.m_safe_diameter;

            m_inputs.evaluate_uniforms(&m_values);
            m_values.m_irradiance *=
                static_cast<float>(
                    m_values.m_irradiance_multiplier * pow(2.0, m_values.m_exposure));

            return true;
        }

        virtual void sample(
            InputEvaluator&         input_evaluator,
            const Transformd&       light_transform,
            const Vector2d&         s,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            double&                 probability) const APPLESEED_OVERRIDE
        {
            sample_disk(
                light_transform,
                s,
                m_scene_center,
                m_scene_radius,
                position,
                outgoing,
                value,
                probability);
        }

        virtual void sample(
            InputEvaluator&         input_evaluator,
            const Transformd&       light_transform,
            const Vector2d&         s,
            const LightTargetArray& targets,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            double&                 probability) const APPLESEED_OVERRIDE
        {
            const size_t target_count = targets.size();

            if (target_count > 0)
            {
                const double x = s[0] * target_count;
                const size_t target_index = truncate<size_t>(x);
                const Vector2d target_s(x - target_index, s[1]);
                const LightTarget& target = targets[target_index];

                sample_disk(
                    light_transform,
                    target_s,
                    target.get_center(),
                    target.get_radius(),
                    position,
                    outgoing,
                    value,
                    probability);
            }
            else
            {
                sample_disk(
                    light_transform,
                    s,
                    m_scene_center,
                    m_scene_radius,
                    position,
                    outgoing,
                    value,
                    probability);
            }
        }

        virtual void evaluate(
            InputEvaluator&         input_evaluator,
            const Transformd&       light_transform,
            const Vector3d&         target,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            outgoing = -normalize(light_transform.get_parent_z());
            position = target - m_safe_scene_diameter * outgoing;
            value = m_values.m_irradiance;
        }

        virtual double compute_distance_attenuation(
            const Vector3d&         target,
            const Vector3d&         position) const APPLESEED_OVERRIDE
        {
            return 1.0;
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            Spectrum    m_irradiance;               // emitted irradiance in W.m^-2
            double      m_irradiance_multiplier;    // emitted irradiance multiplier
            double      m_exposure;                 // emitted irradiance multiplier in f-stops
        };

        Vector3d        m_scene_center;             // world space
        double          m_scene_radius;             // world space
        double          m_safe_scene_diameter;      // world space

        InputValues     m_values;

        void sample_disk(
            const Transformd&       light_transform,
            const Vector2d&         s,
            const Vector3d&         disk_center,
            const double            disk_radius,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            double&                 probability) const
        {
            outgoing = -normalize(light_transform.get_parent_z());

            const Basis3d basis(outgoing);
            const Vector2d p = sample_disk_uniform(s);

            position =
                  disk_center
                - m_safe_scene_diameter * basis.get_normal()
                + disk_radius * p[0] * basis.get_tangent_u()
                + disk_radius * p[1] * basis.get_tangent_v();

            value = m_values.m_irradiance;

            probability = 1.0 / (Pi * disk_radius * disk_radius);
        }
    };
}


//
// DirectionalLightFactory class implementation.
//

const char* DirectionalLightFactory::get_model() const
{
    return Model;
}

Dictionary DirectionalLightFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Directional Light")
            .insert("help", "A light source that emits light in a single direction");
}

DictionaryArray DirectionalLightFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "irradiance")
            .insert("label", "Irradiance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", "1.0")
            .insert("help", "Light intensity"));

    metadata.push_back(
        Dictionary()
            .insert("name", "irradiance_multiplier")
            .insert("label", "Irradiance Multiplier")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "10.0")
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Light intensity multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "exposure")
            .insert("label", "Exposure")
            .insert("type", "numeric")
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("min_value", "-64.0")
            .insert("max_value", "64.0")
            .insert("help", "Light exposure"));

    add_common_input_metadata(metadata);

    return metadata;
}

auto_release_ptr<Light> DirectionalLightFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Light>(new DirectionalLight(name, params));
}

}   // namespace renderer
