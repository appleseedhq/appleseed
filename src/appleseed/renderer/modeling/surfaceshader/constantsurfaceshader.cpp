
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
#include "constantsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

using namespace foundation;
using namespace std;

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
            const char*             name,
            const ParamArray&       params)
          : SurfaceShader(name, params)
        {
            m_inputs.declare("color", InputFormatSpectrum);
            m_inputs.declare("color_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("alpha_multiplier", InputFormatScalar, "1.0");

            const string alpha_source = m_params.get_optional<string>("alpha_source", "color");
            if (alpha_source == "color")
                m_alpha_source = AlphaSourceColor;
            else if (alpha_source == "material")
                m_alpha_source = AlphaSourceMaterial;
            else 
            {
                RENDERER_LOG_ERROR(
                    "invalid value \"%s\" for parameter \"alpha_source\", "
                    "using default value \"color\".",
                    alpha_source.c_str());
                m_alpha_source = AlphaSourceColor;
            }
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return Model;
        }

        virtual void evaluate(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingResult&          shading_result) const override
        {
            // Evaluate the shader inputs.
            InputValues values;
            m_inputs.evaluate(
                shading_context.get_texture_cache(),
                shading_point.get_uv(0),
                &values);

            shading_result.m_color_space = ColorSpaceSpectral;
            shading_result.m_color = values.m_color;

            // Handle alpha mapping.
            if (m_alpha_source == AlphaSourceColor)
                shading_result.m_alpha = values.m_alpha;
            else
            {
                const Material* material = shading_point.get_material();
                if (material && material->get_alpha_map())
                {
                    // Evaluate the alpha map at the shading point.
                    material->get_alpha_map()->evaluate(
                        shading_context.get_texture_cache(),
                        shading_point.get_uv(0),
                        shading_result.m_alpha);
                }
                else shading_result.m_alpha = Alpha(1.0f);
            }

            // Apply multipliers.
            shading_result.m_color *= static_cast<float>(values.m_color_multiplier);
            shading_result.m_alpha *= static_cast<float>(values.m_alpha_multiplier);
        }

      private:
        struct InputValues
        {
            Spectrum    m_color;
            Alpha       m_alpha;
            double      m_color_multiplier;
            double      m_alpha_multiplier;
        };

        enum AlphaSource
        {
            AlphaSourceColor,
            AlphaSourceMaterial
        };

        AlphaSource     m_alpha_source;
    };
}


//
// ConstantSurfaceShaderFactory class implementation.
//

const char* ConstantSurfaceShaderFactory::get_model() const
{
    return Model;
}

const char* ConstantSurfaceShaderFactory::get_human_readable_model() const
{
    return "Constant";
}

DictionaryArray ConstantSurfaceShaderFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "color")
            .insert("label", "Color")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "alpha_source")
            .insert("label", "Alpha Source")
            .insert("widget", "dropdown_list")
            .insert("dropdown_items",
                Dictionary()
                    .insert("Alpha channel of the color", "color")
                    .insert("Alpha map of the material", "material"))
            .insert("use", "optional")
            .insert("default", "color"));

    definitions.push_back(
        Dictionary()
            .insert("name", "color_multiplier")
            .insert("label", "Color Multiplier")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("default", "1.0")
            .insert("use", "optional"));

    definitions.push_back(
        Dictionary()
            .insert("name", "alpha_multiplier")
            .insert("label", "Alpha Multiplier")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("default", "1.0")
            .insert("use", "optional"));

    return definitions;
}

auto_release_ptr<SurfaceShader> ConstantSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<SurfaceShader>(
            new ConstantSurfaceShader(name, params));
}

}   // namespace renderer
