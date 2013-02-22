
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "spotlight.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/source.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/matrix.h"
#include "foundation/math/sampling.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cmath>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Spot light.
    //

    const char* Model = "spot_light";

    class SpotLight
      : public Light
    {
      public:
        SpotLight(
            const char*         name,
            const ParamArray&   params)
          : Light(name, params)
        {
            m_inputs.declare("exitance", InputFormatSpectrum);
            m_inputs.declare("exitance_multiplier", InputFormatScalar, "1.0");
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&      project,
            const Assembly&     assembly) OVERRIDE
        {
            if (!Light::on_frame_begin(project, assembly))
                return false;

            m_radiance_source = m_inputs.source("exitance");
            m_radiance_multiplier_source = m_inputs.source("exitance_multiplier");
            check_non_zero_radiance(m_radiance_source, m_radiance_multiplier_source);

            const double inner_half_angle = deg_to_rad(m_params.get_required<double>("inner_angle", 20.0) / 2.0);
            const double outer_half_angle = deg_to_rad(m_params.get_required<double>("outer_angle", 30.0) / 2.0);
            const double tilt_angle = deg_to_rad(m_params.get_optional<double>("tilt_angle", 0.0));

            m_cos_inner_half_angle = cos(inner_half_angle);
            m_cos_outer_half_angle = cos(outer_half_angle);
            m_rcp_screen_half_size = 1.0 / tan(outer_half_angle);

            m_transform =
                Transformd::from_local_to_parent(
                    Matrix4d::rotation(Vector3d(1.0, 0.0, 0.0), -HalfPi)) *
                get_transform();
            m_position = m_transform.point_to_parent(Vector3d(0.0));
            m_axis = normalize(m_transform.vector_to_parent(Vector3d(0.0, 1.0, 0.0)));

            const Vector3d up = m_transform.vector_to_parent(Vector3d(sin(tilt_angle), 0.0, cos(tilt_angle)));
            const Vector3d v = -m_axis;
            const Vector3d u = normalize(cross(up, v));
            const Vector3d n = cross(v, u);

            m_screen_basis.build(n, u, v);

            return true;
        }

        virtual void sample(
            InputEvaluator&     input_evaluator,
            const Vector2d&     s,
            Vector3d&           position,
            Vector3d&           outgoing,
            Spectrum&           value,
            double&             probability) const OVERRIDE
        {
            position = m_position;
            outgoing = m_transform.vector_to_parent(sample_cone_uniform(s, m_cos_outer_half_angle));
            probability = sample_cone_uniform_pdf(m_cos_outer_half_angle);
            compute_radiance(input_evaluator, outgoing, value);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     target,
            Vector3d&           position,
            Vector3d&           outgoing,
            Spectrum&           value) const OVERRIDE
        {
            position = m_position;
            outgoing = normalize(target - position);

            if (dot(outgoing, m_axis) > m_cos_outer_half_angle)
                compute_radiance(input_evaluator, outgoing, value);
            else value.set(0.0f);
        }

      private:
        struct InputValues
        {
            Spectrum    m_radiance;             // emitted radiance in W.m^-2.sr^-1
            Alpha       m_radiance_alpha;       // unused
            double      m_radiance_multiplier;  // emitted radiance multiplier
        };

        const Source*   m_radiance_source;
        const Source*   m_radiance_multiplier_source;

        double          m_cos_inner_half_angle;
        double          m_cos_outer_half_angle;
        double          m_rcp_screen_half_size;

        Transformd      m_transform;
        Vector3d        m_position;             // world space
        Vector3d        m_axis;                 // world space
        Basis3d         m_screen_basis;         // world space

        void compute_radiance(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           radiance) const
        {
            const double cos_theta = dot(outgoing, m_axis);
            assert(cos_theta > m_cos_outer_half_angle);

            const Vector3d d = outgoing / cos_theta - m_axis;
            const double x = dot(d, m_screen_basis.get_tangent_u()) * m_rcp_screen_half_size;
            const double y = dot(d, m_screen_basis.get_normal()) * m_rcp_screen_half_size;
            const Vector2d uv(0.5 * (x + 1.0), 0.5 * (y + 1.0));

            const InputValues* values = input_evaluator.evaluate<InputValues>(m_inputs, uv);
            radiance = values->m_radiance;
            radiance *= static_cast<float>(values->m_radiance_multiplier);

            if (cos_theta < m_cos_inner_half_angle)
            {
                radiance *=
                    static_cast<float>(
                        smoothstep(m_cos_outer_half_angle, m_cos_inner_half_angle, cos_theta));
            }
        }
    };
}


//
// SpotLightFactory class implementation.
//

const char* SpotLightFactory::get_model() const
{
    return Model;
}

const char* SpotLightFactory::get_human_readable_model() const
{
    return "Spot Light";
}

DictionaryArray SpotLightFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "exitance")
            .insert("label", "Radiance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "exitance_multiplier")
            .insert("label", "Radiance Multiplier")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "inner_angle")
            .insert("label", "Inner Angle")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", "20.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "outer_angle")
            .insert("label", "Outer Angle")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", "30.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "tilt_angle")
            .insert("label", "Tilt Angle")
            .insert("widget", "text_box")
            .insert("use", "optional")
            .insert("default", "0.0"));

    return definitions;
}

auto_release_ptr<Light> SpotLightFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Light>(new SpotLight(name, params));
}

}   // namespace renderer
