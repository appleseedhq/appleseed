
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/input/sourceinputs.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Forward declarations.
namespace renderer  { class AOVComponents; }
namespace renderer  { class ShadingComponents; }

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
            const char*             name,
            const ParamArray&       params)
          : EnvironmentShader(name, params)
        {
            m_inputs.declare("color", InputFormatSpectralIlluminance);
            m_inputs.declare("alpha", InputFormatFloat, "1.0");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        void evaluate(
            const ShadingContext&   shading_context,
            const PixelContext&     pixel_context,
            const Vector3d&         direction,
            ShadingResult&          shading_result,
            ShadingComponents&      shading_components,
            AOVComponents&          aov_components) const override
        {
            const Vector2f s(pixel_context.get_sample_position());
            const Vector2f uv(s[0], 1.0f - s[1]);

            InputValues values;
            m_inputs.evaluate(
                shading_context.get_texture_cache(),
                SourceInputs(uv),
                &values);

            shading_result.m_main.rgb() = values.m_color.to_rgb(g_std_lighting_conditions);
            shading_result.m_main.a = values.m_alpha;
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            Spectrum    m_color;
            float       m_alpha;
        };
    };
}


//
// BackgroundEnvironmentShaderFactory class implementation.
//

void BackgroundEnvironmentShaderFactory::release()
{
    delete this;
}

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
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha")
            .insert("label", "Alpha")
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

auto_release_ptr<EnvironmentShader> BackgroundEnvironmentShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentShader>(
            new BackgroundEnvironmentShader(name, params));
}

}   // namespace renderer
