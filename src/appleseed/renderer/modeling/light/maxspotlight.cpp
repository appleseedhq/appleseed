
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
#include "maxspotlight.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/sourceinputs.h"
#include "renderer/utility/autodeskmax.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/distance.h"
#include "foundation/math/matrix.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cmath>

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
    // A light source compatible with the Spot light of Autodesk 3ds Max.
    //

    const char* Model = "max_spot_light";

    class MaxSpotLight
      : public Light
    {
      public:
        MaxSpotLight(
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

            m_intensity_source = m_inputs.source("intensity");
            m_intensity_multiplier_source = m_inputs.source("intensity_multiplier");
            check_non_zero_emission(m_intensity_source, m_intensity_multiplier_source);

            const double inner_half_angle = deg_to_rad(m_params.get_required<double>("inner_angle", 20.0) / 2.0);
            const double outer_half_angle = deg_to_rad(m_params.get_required<double>("outer_angle", 30.0) / 2.0);
            const double tilt_angle = deg_to_rad(m_params.get_optional<double>("tilt_angle", 0.0));

            m_cos_inner_half_angle = std::cos(inner_half_angle);
            m_cos_outer_half_angle = std::cos(outer_half_angle);
            m_rcp_screen_half_size = 1.0 / std::tan(outer_half_angle);
            m_up = Vector3d(std::sin(tilt_angle), std::cos(tilt_angle), 0.0);

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

            const Vector3d axis = -normalize(light_transform.get_parent_z());

            if (dot(outgoing, axis) > m_cos_outer_half_angle)
                compute_radiance(shading_context, light_transform, axis, outgoing, value);
            else value.set(0.0f);

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
            outgoing =
                light_transform.vector_to_parent(
                    rotate_minus_pi_around_x(
                        sample_cone_uniform(s, m_cos_outer_half_angle)));
            probability = sample_cone_uniform_pdf(static_cast<float>(m_cos_outer_half_angle));
            assert(probability > 0.0f);

            const Vector3d axis = -normalize(light_transform.get_parent_z());

            compute_radiance(shading_context, light_transform, axis, outgoing, value);
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

        const Source*   m_intensity_source;
        const Source*   m_intensity_multiplier_source;

        double          m_cos_inner_half_angle;
        double          m_cos_outer_half_angle;
        double          m_rcp_screen_half_size;

        Vector3d        m_up;                       // light space

        float           m_decay_start;              // distance at which light decay starts
        float           m_decay_exponent;           // exponent of the light decay function

        static Vector3d rotate_minus_pi_around_x(const Vector3d& v)
        {
            return Vector3d(v[0], v[2], -v[1]);
        }

        void compute_radiance(
            const ShadingContext&   shading_context,
            const Transformd&       light_transform,
            const Vector3d&         axis,
            const Vector3d&         outgoing,
            Spectrum&               radiance) const
        {
            const Vector3d up = light_transform.vector_to_parent(m_up);
            const Vector3d v = -axis;
            const Vector3d u = normalize(cross(up, v));
            const Vector3d n = cross(v, u);

            const double cos_theta = dot(outgoing, axis);
            assert(cos_theta > m_cos_outer_half_angle);

            const Vector3d d = outgoing / cos_theta - axis;
            const float x = static_cast<float>(dot(d, u) * m_rcp_screen_half_size);
            const float y = static_cast<float>(dot(d, n) * m_rcp_screen_half_size);
            const Vector2f uv(0.5f * (x + 1.0f), 0.5f * (y + 1.0f));

            InputValues values;
            m_inputs.evaluate(shading_context.get_texture_cache(), SourceInputs(uv), &values);

            radiance = values.m_intensity;
            radiance *= values.m_intensity_multiplier;

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
// MaxSpotLightFactory class implementation.
//

void MaxSpotLightFactory::release()
{
    delete this;
}

const char* MaxSpotLightFactory::get_model() const
{
    return Model;
}

Dictionary MaxSpotLightFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Autodesk 3ds Max Spot Light")
            .insert("help", "A light source compatible with the Spot light of Autodesk 3ds Max");
}

DictionaryArray MaxSpotLightFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "intensity")
            .insert("label", "Intensity")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "1.0")
            .insert("help", "Light intensity"));

    metadata.push_back(
        Dictionary()
            .insert("name", "intensity_multiplier")
            .insert("label", "Intensity Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Light intensity multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "inner_angle")
            .insert("label", "Inner Angle")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-180.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "180.0")
                    .insert("type", "soft"))
            .insert("use", "required")
            .insert("default", "20.0")
            .insert("help", "Cone distribution inner angle"));

    metadata.push_back(
        Dictionary()
            .insert("name", "outer_angle")
            .insert("label", "Outer Angle")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-180.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "180.0")
                    .insert("type", "soft"))
            .insert("use", "required")
            .insert("default", "30.0")
            .insert("help", "Cone distribution outer angle"));

    metadata.push_back(
        Dictionary()
            .insert("name", "tilt_angle")
            .insert("label", "Tilt Angle")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-360.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "360.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("help", "Rotate the spot light around its axis; only useful when using the light intensity is textured (gobo)"));

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

auto_release_ptr<Light> MaxSpotLightFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Light>(new MaxSpotLight(name, params));
}

}   // namespace renderer
