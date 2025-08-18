
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
#include "physicalsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/kernel/shading/shadowcatcher.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/surfaceshader/nprsurfaceshaderhelper.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class AOVComponents; }
namespace renderer  { class PixelContext; }
namespace renderer  { class Project; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // A surface shader that uses physically-based rendering to shade pixels.
    //

    const char* Model = "physical_surface_shader";

    class PhysicalSurfaceShader
      : public SurfaceShader
    {
      public:
        PhysicalSurfaceShader(
            const char*                 name,
            const ParamArray&           params)
          : SurfaceShader(name, params)
        {
            m_inputs.declare("color_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("alpha_multiplier", InputFormat::Float, "1.0");
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
            const Project&              project,
            const BaseGroup*            parent,
            OnFrameBeginRecorder&       recorder,
            IAbortSwitch*               abort_switch) override
        {
            m_lighting_samples = m_params.get_optional<size_t>("lighting_samples", 1);
            return true;
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
            assert(shading_context.get_lighting_engine() != nullptr);

            // OSL shaders can modify the shading basis in the shading point when using bump,
            // normal maps or anisotropy. When using more than 1 lighting sample, we need to
            // save and restore the basis for each sample.
            const Basis3d shading_basis = shading_point.get_shading_basis();
            shading_context.get_lighting_engine()->compute_lighting(
                sampling_context,
                pixel_context,
                shading_context,
                shading_point,
                shading_components,
                aov_components,
                shadow_catcher);

            if (m_lighting_samples > 1)
            {
                for (size_t i = 1, e = m_lighting_samples; i < e; ++i)
                {
                    shading_point.set_shading_basis(shading_basis);
                    shading_context.get_lighting_engine()->compute_lighting(
                        sampling_context,
                        pixel_context,
                        shading_context,
                        shading_point,
                        shading_components,
                        aov_components,
                        shadow_catcher);
                }

                shading_components /= static_cast<float>(m_lighting_samples);
            }

            // Run NPR surface shader if any.
            if (const Material* material = shading_point.get_material())
            {
                if (const ShaderGroup* sg = material->get_render_data().m_shader_group)
                {
                    if (sg->has_npr())
                    {
                        NPRSurfaceShaderHelper::evaluate(
                            sampling_context,
                            shading_context,
                            shading_point,
                            shading_components,
                            aov_components);
                    }
                }
            }

            shading_result.m_main.rgb() =
                shading_components.m_beauty.illuminance_to_rgb(g_std_lighting_conditions);
        }

      private:
        size_t m_lighting_samples;
    };
}


//
// PhysicalSurfaceShaderFactory class implementation.
//

void PhysicalSurfaceShaderFactory::release()
{
    delete this;
}

const char* PhysicalSurfaceShaderFactory::get_model() const
{
    return Model;
}

Dictionary PhysicalSurfaceShaderFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Physical")
            .insert("default_model", "true");
}

DictionaryArray PhysicalSurfaceShaderFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "lighting_samples")
            .insert("label", "Lighting Samples")
            .insert("type", "integer")
            .insert("min",
                Dictionary()
                    .insert("value", "1")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1000")
                    .insert("type", "soft"))
            .insert("default", "1")
            .insert("use", "optional"));

    return metadata;
}

auto_release_ptr<SurfaceShader> PhysicalSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<SurfaceShader>(new PhysicalSurfaceShader(name, params));
}

}   // namespace renderer
