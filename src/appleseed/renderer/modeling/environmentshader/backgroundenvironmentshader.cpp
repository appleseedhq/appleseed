
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Francois Beaune, The appleseedhq Organization
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
#include "backgroundenvironmentshader.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/shadingfragmentstack.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingfragment.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Background environment shader.
    //

    const char* Model = "background_environment_shader";

    class BackgroundEnvironmentShader
      : public EnvironmentShader
    {
      public:
        BackgroundEnvironmentShader(
            const char*         name,
            const ParamArray&   params)
          : EnvironmentShader(name, params)
        {
            m_inputs.declare("color", InputFormatSpectralIlluminance);
            m_inputs.declare("alpha", InputFormatScalar, "1.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual void evaluate(
            const ShadingContext&   shading_context,
            const PixelContext&     pixel_context,
            InputEvaluator&         input_evaluator,
            const Vector3d&         direction,
            ShadingResult&          shading_result) const APPLESEED_OVERRIDE
        {
            const Vector2d& s = pixel_context.get_sample_position();
            const InputValues* input_values =
                input_evaluator.evaluate<InputValues>(m_inputs, Vector2d(s[0], 1.0 - s[1]));

            shading_result.m_color_space = ColorSpaceSpectral;
            shading_result.m_main.m_color = input_values->m_color;
            shading_result.m_main.m_alpha[0] = static_cast<float>(input_values->m_alpha);
            shading_result.m_aovs.m_color.set(0.0f);
            shading_result.m_aovs.m_alpha.set(0.0f);
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            Spectrum        m_color;
            ScalarInput     m_alpha;
        };
    };
}


//
// BackgroundEnvironmentShaderFactory class implementation.
//

const char* BackgroundEnvironmentShaderFactory::get_model() const
{
    return Model;
}

Dictionary BackgroundEnvironmentShaderFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Background Environment Shader");
}

DictionaryArray BackgroundEnvironmentShaderFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "color")
            .insert("label", "Color")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha")
            .insert("label", "Alpha")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "1.0")
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<EnvironmentShader> BackgroundEnvironmentShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentShader>(
            new BackgroundEnvironmentShader(name, params));
}

auto_release_ptr<EnvironmentShader> BackgroundEnvironmentShaderFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<EnvironmentShader>(
            new BackgroundEnvironmentShader(name, params));
}

}   // namespace renderer
