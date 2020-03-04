
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
#include "edfenvironmentshader.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <string>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class AOVComponents; }
namespace renderer      { class PixelContext; }
namespace renderer      { class Project; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // EDF-based environment shader.
    //

    const char* Model = "edf_environment_shader";

    class EDFEnvironmentShader
      : public EnvironmentShader
    {
      public:
        EDFEnvironmentShader(
            const char*             name,
            const ParamArray&       params)
          : EnvironmentShader(name, params)
          , m_env_edf(nullptr)
        {
            m_inputs.declare("environment_edf", InputFormatEntity);
            m_inputs.declare("alpha_value", InputFormatFloat, "1.0");
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
            if (!EnvironmentShader::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            const OnFrameBeginMessageContext context("environment edf", this);

            // Cache the bound environment EDF.
            m_env_edf = dynamic_cast<const EnvironmentEDF*>(m_inputs.get_entity("environment_edf"));

            // Abort if no environment EDF was found.
            if (m_env_edf == nullptr)
            {
                RENDERER_LOG_ERROR(
                    "%scannot find environment edf \"%s\".",
                    context.get(),
                    m_params.get_required<std::string>("environment_edf", "").c_str());
                return false;
            }

            // Evaluate and store alpha value.
            InputValues uniform_values;
            m_inputs.evaluate_uniforms(&uniform_values);
            m_alpha_value = uniform_values.m_alpha_value;

            return true;
        }

        void evaluate(
            const ShadingContext&   shading_context,
            const PixelContext&     pixel_context,
            const Vector3d&         direction,
            ShadingResult&          shading_result,
            ShadingComponents&      shading_components,
            AOVComponents&          aov_components) const override
        {
            Spectrum value;
            m_env_edf->evaluate(shading_context, Vector3f(direction), value);

            shading_components.m_emission = value;

            shading_result.m_main.rgb() = value.to_rgb(g_std_lighting_conditions);
            shading_result.m_main.a = m_alpha_value;
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            float m_alpha_value;
        };

        const EnvironmentEDF*       m_env_edf;
        float                       m_alpha_value;
    };
}


//
// EDFEnvironmentShaderFactory class implementation.
//

void EDFEnvironmentShaderFactory::release()
{
    delete this;
}

const char* EDFEnvironmentShaderFactory::get_model() const
{
    return Model;
}

Dictionary EDFEnvironmentShaderFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Environment EDF-Based Environment Shader")
            .insert("default_model", "true");
}

DictionaryArray EDFEnvironmentShaderFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "environment_edf")
            .insert("label", "Environment EDF")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary()
                    .insert("environment_edf", "Environment EDFs"))
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_value")
            .insert("label", "Alpha Value")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<EnvironmentShader> EDFEnvironmentShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentShader>(
            new EDFEnvironmentShader(name, params));
}

}   // namespace renderer
