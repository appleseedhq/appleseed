
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
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/containers/specializedarrays.h"

// Forward declarations.
namespace renderer  { class TextureCache; }

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
            InputValues values;
            m_inputs.evaluate(
                shading_context.get_texture_cache(),
                shading_point.get_uv(0),
                &values);

            shading_result.m_color_space = ColorSpaceSpectral;
            shading_result.m_color = values.m_color;
            shading_result.m_alpha = values.m_alpha;
        }

        virtual void evaluate_alpha_mask(
            SamplingContext&        sampling_context,
            TextureCache&           texture_cache,
            const ShadingPoint&     shading_point,
            Alpha&                  alpha) const override
        {
            InputValues values;
            m_inputs.evaluate(
                texture_cache,
                shading_point.get_uv(0),
                &values);

            alpha = values.m_alpha;
        }

      private:
        struct InputValues
        {
            Spectrum    m_color;
            Alpha       m_alpha;
        };
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
