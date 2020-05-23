
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
#include "shadingengine.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/aov/aovcomponents.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/kernel/shading/shadowcatcher.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/surfaceshader/diagnosticsurfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/vector.h"

using namespace foundation;

namespace renderer
{

//
// ShadingEngine class implementation.
//

ShadingEngine::ShadingEngine(const ParamArray& params)
{
    create_diagnostic_surface_shader(params);
}

bool ShadingEngine::on_render_begin(
    const Project&              project,
    OnRenderBeginRecorder&      recorder,
    IAbortSwitch*               abort_switch)
{
    bool success = true;

    if (m_diagnostic_surface_shader.get())
        success = success && m_diagnostic_surface_shader->on_render_begin(project, nullptr, recorder, abort_switch);

    return success;
}

bool ShadingEngine::on_frame_begin(
    const Project&              project,
    OnFrameBeginRecorder&       recorder,
    IAbortSwitch*               abort_switch)
{
    bool success = true;

    if (m_diagnostic_surface_shader.get())
        success = success && m_diagnostic_surface_shader->on_frame_begin(project, nullptr, recorder, abort_switch);

    return success;
}

void ShadingEngine::create_diagnostic_surface_shader(const ParamArray& params)
{
    if (params.dictionaries().exist("override_shading"))
    {
        m_diagnostic_surface_shader =
            DiagnosticSurfaceShaderFactory().create(
                "__diagnostic_surface_shader",
                params.child("override_shading"));
    }
}

bool ShadingEngine::shade_hit_point(
    SamplingContext&            sampling_context,
    const PixelContext&         pixel_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    AOVAccumulatorContainer&    aov_accumulators,
    ShadingResult&              shading_result) const
{
    // Compute the alpha channel of the main output.
    shading_result.m_main.a = shading_point.get_alpha()[0];

    // Retrieve the material of the intersected surface.
    const Material* material = shading_point.get_material();

    if (material != nullptr &&
        material->get_render_data().m_shader_group != nullptr)
    {
        const ShaderGroup* sg = material->get_render_data().m_shader_group;
        const bool has_matte = sg->has_matte();

        if (sg->has_transparency() || has_matte)
        {
            shading_context.execute_osl_transparency_and_matte(*sg, shading_point);

            if (has_matte)
            {
                Color4f matte;
                const bool any_matte_closure =
                    process_matte_tree(
                        shading_point.get_osl_shader_globals().Ci,
                        matte.rgb(),
                        matte.a);

                if (any_matte_closure)
                {
                    shading_result.m_main = matte;
                    return true;
                }
            }

            if (sg->has_transparency())
            {
                Alpha alpha;
                process_transparency_tree(shading_point.get_osl_shader_globals().Ci, alpha);
                shading_result.m_main.a *= alpha[0];
            }
        }
    }

    // Shade the sample if it isn't fully transparent.
    if (shading_result.m_main.a > 0.0f)
    {
        // Use the diagnostic surface shader if there is one.
        const SurfaceShader* surface_shader = m_diagnostic_surface_shader.get();

        if (surface_shader == nullptr)
        {
            if (material == nullptr)
            {
                // The intersected surface has no material: return solid pink.
                shading_result.set_main_to_opaque_pink();
                return true;
            }

            // Use the surface shader of the intersected surface.
            surface_shader = material->get_render_data().m_surface_shader;

            if (surface_shader == nullptr)
            {
                // The intersected surface has no surface shader: return solid pink.
                shading_result.set_main_to_opaque_pink();
                return true;
            }
        }

        ShadowCatcher shadow_catcher;
        if (strcmp(shading_point.get_object_instance().get_name(), "Box001_sc_inst") == 0)
            shadow_catcher.m_enabled = true;

        // Execute the surface shader.
        ShadingComponents shading_components;
        AOVComponents aov_components;
        surface_shader->evaluate(
            sampling_context,
            pixel_context,
            shading_context,
            shading_point,
            shading_result,
            shading_components,
            aov_components,
            shadow_catcher);

        // Accumulate shading components and AOV components into the shading result and AOVs.
        aov_accumulators.write(
            pixel_context,
            shading_point,
            shading_components,
            aov_components,
            shading_result);

        // If it's shadow catcher
        // find light radiance value without material impact
        // find light radiance value without calculating transmission, which should give us light value with no shadows
        // based on these two values find "shadow value" and apply it to alpha value
        if (strcmp(shading_point.get_object_instance().get_name(), "Box001_sc_inst") == 0)
        {
            shading_result.m_main.a = 1.0f - shadow_catcher.m_shadow_ratio;

            return true;
        }

        // Apply alpha premultiplication.
        shading_result.apply_alpha_premult();
    }

    return false;
}

void ShadingEngine::shade_environment(
    SamplingContext&            sampling_context,
    const PixelContext&         pixel_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    AOVAccumulatorContainer&    aov_accumulators,
    ShadingResult&              shading_result) const
{
    // Retrieve the environment shader of the scene.
    const EnvironmentShader* environment_shader =
        shading_point.get_scene().get_environment()->get_environment_shader();

    if (environment_shader != nullptr)
    {
        // There is an environment shader: execute it.
        ShadingComponents shading_components;
        AOVComponents aov_components;
        environment_shader->evaluate(
            shading_context,
            pixel_context,
            normalize(shading_point.get_ray().m_dir),
            shading_result,
            shading_components,
            aov_components);

        // Accumulate shading components and AOV components into the shading result and AOVs.
        aov_accumulators.write(
            pixel_context,
            shading_point,
            shading_components,
            aov_components,
            shading_result);

        // Apply alpha premultiplication.
        shading_result.apply_alpha_premult();
    }
}

}   // namespace renderer
