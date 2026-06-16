
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
#include "constantsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/kernel/shading/shadowcatcher.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/sourceinputs.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Forward declarations.
namespace renderer  { class AOVComponents; }
namespace renderer  { class PixelContext; }
namespace renderer  { class ShadingComponents; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Constant color surface shader.
    //

    const char* Model = "constant_surface_shader";

    class ConstantSurfaceShader
      : public SurfaceShader
    {
      public:
        ConstantSurfaceShader(
            const char*                 name,
            const ParamArray&           params)
          : SurfaceShader(name, params)
        {
            m_inputs.declare("color", InputFormat::SpectralIlluminanceWithAlpha);
            m_inputs.declare("color_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("alpha_multiplier", InputFormat::Float, "1.0");

            const std::string alpha_source = m_params.get_optional<std::string>("alpha_source", "color");
            if (alpha_source == "color")
                m_alpha_source = AlphaSourceColor;
            else if (alpha_source == "material")
                m_alpha_source = AlphaSourceObjectAndMaterial;
            else
            {
                RENDERER_LOG_ERROR(
                    "invalid value \"%s\" for parameter \"alpha_source\", "
                    "using default value \"color\".",
                    alpha_source.c_str());
                m_alpha_source = AlphaSourceColor;
            }
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
            SamplingContext&            sampling_context,
            const PixelContext&         pixel_context,
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point,
            ShadingResult&              shading_result,
            ShadingComponents&          shading_components,
            AOVComponents&              aov_components,
            ShadowCatcher&              shadow_catcher) const override
        {
            // Evaluate the shader inputs.
            InputValues values;
            m_inputs.evaluate(
                shading_context.get_texture_cache(),
                SourceInputs(shading_point.get_uv(0)),
                &values);

            // Initialize the shading result.
            shading_result.m_main.rgb() = values.m_color.illuminance_to_rgb(g_std_lighting_conditions);

            // This surface shader can override alpha.
            if (m_alpha_source == AlphaSourceColor)
                shading_result.m_main.a = values.m_alpha[0] * values.m_alpha_multiplier;
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            Spectrum    m_color;
            Alpha       m_alpha;
            float       m_color_multiplier;
            float       m_alpha_multiplier;
        };

        enum AlphaSource
        {
            AlphaSourceColor,
            AlphaSourceObjectAndMaterial
        };

        AlphaSource     m_alpha_source;
    };
}


//
// ConstantSurfaceShaderFactory class implementation.
//

void ConstantSurfaceShaderFactory::release()
{
    delete this;
}

const char* ConstantSurfaceShaderFactory::get_model() const
{
    return Model;
}

Dictionary ConstantSurfaceShaderFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Constant");
}

DictionaryArray ConstantSurfaceShaderFactory::get_input_metadata() const
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
            .insert("name", "alpha_source")
            .insert("label", "Alpha Source")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Alpha channel of the color", "color")
                    .insert("Alpha map of the object and/or material", "material"))
            .insert("use", "optional")
            .insert("default", "color"));

    metadata.push_back(
        Dictionary()
            .insert("name", "color_multiplier")
            .insert("label", "Color Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("default", "1.0")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_multiplier")
            .insert("label", "Alpha Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("default", "1.0")
            .insert("use", "optional"));

    return metadata;
}

auto_release_ptr<SurfaceShader> ConstantSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<SurfaceShader>(new ConstantSurfaceShader(name, params));
}

}   // namespace renderer
