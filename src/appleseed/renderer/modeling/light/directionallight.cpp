
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/sampling.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Forward declarations.
namespace renderer  { class Assembly; }

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
            const char*         name,
            const ParamArray&   params)
          : Light(name, params)
        {
            m_inputs.declare("exitance", InputFormatSpectrum);
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&      project,
            const Assembly&     assembly) override
        {
            if (!Light::on_frame_begin(project, assembly))
                return false;

            if (!check_uniform("exitance"))
                return false;

            check_non_zero_exitance("exitance");

            m_inputs.evaluate_uniforms(&m_values);
            m_safe_scene_radius = project.get_scene()->compute_radius();
            m_outgoing = normalize(get_transform().vector_to_parent(Vector3d(0.0, 0.0, -1.0)));
            m_basis.build(m_outgoing);

            return true;
        }

        virtual void sample(
            InputEvaluator&     input_evaluator,
            const Vector2d&     s,
            Vector3d&           position,
            Vector3d&           outgoing,
            Spectrum&           value,
            double&             probability) const override
        {
            const Vector2d point_on_disk = sample_disk_uniform(s);
            position =
                m_basis.transform_to_parent(
                    m_safe_scene_radius * Vector3d(point_on_disk[0], -1.0, point_on_disk[1]));
            outgoing = m_outgoing;
            value = m_values.m_exitance;
            probability = RcpPi;
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     target,
            Vector3d&           position,
            Vector3d&           outgoing,
            Spectrum&           value) const override
        {
            position = target - m_safe_scene_radius * m_outgoing;
            outgoing = m_outgoing;
            value = m_values.m_exitance;
        }

      private:
        struct InputValues
        {
            Spectrum    m_exitance;             // radiant exitance, in W.m^-2
            Alpha       m_exitance_alpha;       // unused
        };

        InputValues     m_values;
        double          m_safe_scene_radius;    // world space
        Vector3d        m_outgoing;             // world space
        Basis3d         m_basis;                // world space
    };
}


//
// DirectionalLightFactory class implementation.
//

const char* DirectionalLightFactory::get_model() const
{
    return Model;
}

const char* DirectionalLightFactory::get_human_readable_model() const
{
    return "Directional Light";
}

DictionaryArray DirectionalLightFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "exitance")
            .insert("label", "Exitance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", ""));

    return definitions;
}

auto_release_ptr<Light> DirectionalLightFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Light>(new DirectionalLight(name, params));
}

}   // namespace renderer
