
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "oslsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/aov/shadingfragmentstack.h"
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingfragment.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/project/project.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Forward declarations.
namespace renderer  { class PixelContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // OSL surface shader.
    //

    const char* Model = "osl_surface_shader";

    OIIO::ustring g_color_output("Cout");
    OIIO::ustring g_alpha_output("Aout");

    class OSLSurfaceShader
      : public SurfaceShader
    {
      public:
        OSLSurfaceShader(
            const char*                 name,
            const ParamArray&           params)
          : SurfaceShader(name, params)
        {
            m_inputs.declare("surface_shader", InputFormatEntity, 0);
            m_inputs.declare("osl_shader", InputFormatEntity, "");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&              project,
            const BaseGroup*            parent,
            OnFrameBeginRecorder&       recorder,
            foundation::IAbortSwitch*   abort_switch) APPLESEED_OVERRIDE
        {
            if (!SurfaceShader::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            m_surface_shader =
                static_cast<SurfaceShader*>(m_inputs.get_entity("surface_shader"));

            m_lighting_conditions =
                &project.get_frame()->get_lighting_conditions();

            m_shader_group =
                static_cast<const ShaderGroup*>(m_inputs.get_entity("osl_shader"));

            if (m_shader_group && !m_shader_group->is_valid())
                m_shader_group = 0;

            return true;
        }

        virtual void on_frame_end(
            const Project&              project,
            const BaseGroup*            parent) APPLESEED_OVERRIDE
        {
            m_surface_shader = 0;
            m_lighting_conditions = 0;
            m_shader_group = 0;

            SurfaceShader::on_frame_end(project, parent);
        }

        virtual void evaluate(
            SamplingContext&            sampling_context,
            const PixelContext&         pixel_context,
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point,
            ShadingResult&              shading_result,
            AOVAccumulatorContainer&    aov_accumulators) const APPLESEED_OVERRIDE
        {
            if (m_shader_group)
            {
                const OSL::ShaderSymbol* color_sym =
                    m_shader_group->surface_shader_color_symbol();

                const OSL::ShaderSymbol* alpha_sym =
                    m_shader_group->surface_shader_alpha_symbol();

                if (color_sym && alpha_sym)
                {
                    m_surface_shader->evaluate(
                        sampling_context,
                        pixel_context,
                        shading_context,
                        shading_point,
                        shading_result,
                        aov_accumulators);

                    shading_result.transform_to_linear_rgb(*m_lighting_conditions);

                    shading_context.execute_osl_surface_shader(
                        *m_shader_group,
                        shading_point,
                        shading_result.m_main.m_color.rgb(),
                        shading_result.m_main.m_alpha[0]);

                    const float* c =
                        reinterpret_cast<const float*>(
                            shading_context.get_osl_shading_system().symbol_address(
                                *shading_context.get_osl_shading_context(),
                                color_sym));

                    const float* a =
                        reinterpret_cast<const float*>(
                            shading_context.get_osl_shading_system().symbol_address(
                                *shading_context.get_osl_shading_context(),
                                alpha_sym));

                    shading_result.set_main_to_linear_rgba(Color4f(c[0], c[1], c[2], *a));
                    return;
                }
            }

            // If we arrive here, something went wrong.
            shading_result.set_main_to_opaque_pink_linear_rgba();
        }

      private:
        const SurfaceShader*        m_surface_shader;
        const LightingConditions*   m_lighting_conditions;
        const ShaderGroup*          m_shader_group;
        const void*                 m_color_symbol;
        const void*                 m_alpha_symbol;
    };
}


//
// OSLSurfaceShaderFactory class implementation.
//

const char* OSLSurfaceShaderFactory::get_model() const
{
    return Model;
}

Dictionary OSLSurfaceShaderFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "OSL");
}

DictionaryArray OSLSurfaceShaderFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "surface_shader")
            .insert("label", "Surface Shader")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary()
                    .insert("surface_shader", "Surface Shaders"))
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "osl_shader")
            .insert("label", "OSL Shader")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary()
                    .insert("shader_group", "Shader Groups"))
            .insert("use", "optional"));

    return metadata;
}

auto_release_ptr<SurfaceShader> OSLSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<SurfaceShader>(
            new OSLSurfaceShader(name, params));
}

}   // namespace renderer
