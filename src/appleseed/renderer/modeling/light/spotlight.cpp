
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
#include "spotlight.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/sampling.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class Project; }

using namespace foundation;

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
          , m_cos_theta_max(cos(deg_to_rad(params.get_required<double>("angle", 30.0))))
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

        virtual void on_frame_begin(
            const Project&      project,
            const Assembly&     assembly) override
        {
            m_axis = get_transform().vector_to_parent(Vector3d(0.0, 1.0, 0.0));
        }
 
        virtual void sample(
            const void*         data,
            const Vector2d&     s,
            Vector3d&           outgoing,
            Spectrum&           value,
            double&             probability) const override
        {
            outgoing = get_transform().vector_to_parent(sample_cone_uniform(s, m_cos_theta_max));
            value = static_cast<const InputValues*>(data)->m_exitance;
            probability = sample_cone_uniform_pdf(m_cos_theta_max);
        }

        virtual void evaluate(
            const void*         data,
            const Vector3d&     outgoing,
            Spectrum&           value) const override
        {
            const double cos_theta = dot(outgoing, m_axis);

            if (cos_theta > m_cos_theta_max)
                value = static_cast<const InputValues*>(data)->m_exitance;
            else value.set(0.0f);
        }

        virtual void evaluate(
            const void*         data,
            const Vector3d&     outgoing,
            Spectrum&           value,
            double&             probability) const override
        {
            const double cos_theta = dot(outgoing, m_axis);

            if (cos_theta > m_cos_theta_max)
            {
                value = static_cast<const InputValues*>(data)->m_exitance;
                probability = sample_cone_uniform_pdf(m_cos_theta_max);
            }
            else
            {
                value.set(0.0f);
                probability = 0.0f;
            }
        }

        virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     outgoing) const override
        {
            const double cos_theta = dot(outgoing, m_axis);

            return
                cos_theta > m_cos_theta_max
                    ? sample_cone_uniform_pdf(m_cos_theta_max)
                    : 0.0;
        }

      private:
        struct InputValues
        {
            Spectrum    m_exitance;         // radiant exitance, in W.m^-2
            Alpha       m_exitance_alpha;   // unused
        };

        const double    m_cos_theta_max;
        Vector3d        m_axis;
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
            .insert("label", "Exitance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "angle")
            .insert("label", "Angle")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", "30.0"));

    return definitions;
}

auto_release_ptr<Light> SpotLightFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Light>(new SpotLight(name, params));
}

}   // namespace renderer
