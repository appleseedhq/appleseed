
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/aov/shadingfragmentstack.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingfragment.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#ifdef APPLESEED_WITH_OSL
#include "renderer/modeling/shadergroup/shadergroup.h"
#endif
#include "renderer/modeling/surfaceshader/diagnosticsurfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ShadingEngine class implementation.
//

ShadingEngine::ShadingEngine(const ParamArray& params)
{
    create_diagnostic_surface_shader(params);
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

void ShadingEngine::shade_hit_point(
    SamplingContext&        sampling_context,
    const PixelContext&     pixel_context,
    const ShadingContext&   shading_context,
    const ShadingPoint&     shading_point,
    ShadingResult&          shading_result) const
{
    // Compute the alpha channel of the main output.
    shading_result.m_main.m_alpha = shading_point.get_alpha();

    // Retrieve the material of the intersected surface.
    const Material* material = shading_point.get_material();

#ifdef APPLESEED_WITH_OSL
    // Apply OSL transparency if needed.
    if (material &&
        material->get_render_data().m_shader_group &&
        material->get_render_data().m_shader_group->has_transparency())
    {
        Alpha alpha;
        shading_context.execute_osl_transparency(
            *material->get_render_data().m_shader_group,
            shading_point,
            alpha);
        shading_result.m_main.m_alpha *= alpha;
    }
#endif

    // Shade the sample if it isn't fully transparent.
    if (shading_result.m_main.m_alpha[0] > 0.0f || shading_point.shade_alpha_cutouts())
    {
        // Use the diagnostic surface shader if there is one.
        const SurfaceShader* surface_shader = m_diagnostic_surface_shader.get();

        if (surface_shader == 0)
        {
            if (material == 0)
            {
                // The intersected surface has no material: return solid pink.
                shading_result.set_main_to_opaque_pink_linear_rgba();
                shading_result.set_aovs_to_transparent_black_linear_rgba();
                return;
            }

            // Use the surface shader of the intersected surface.
            surface_shader = material->get_render_data().m_surface_shader;

            if (surface_shader == 0)
            {
                // The intersected surface has no surface shader: return solid pink.
                shading_result.set_main_to_opaque_pink_linear_rgba();
                shading_result.set_aovs_to_transparent_black_linear_rgba();
                return;
            }
        }

        // Execute the surface shader.
        surface_shader->evaluate(
            sampling_context,
            pixel_context,
            shading_context,
            shading_point,
            shading_result);

        // Set AOVs.
        shading_result.set_entity_aov(shading_point.get_assembly());
        shading_result.set_entity_aov(shading_point.get_assembly_instance());
        shading_result.set_entity_aov(shading_point.get_object());
        shading_result.set_entity_aov(shading_point.get_object_instance());
        if (material)
            shading_result.set_entity_aov(*material);
        shading_result.set_entity_aov(*surface_shader);
    }
    else
    {
        // Alpha is zero: shade as transparent black.
        shading_result.set_main_to_transparent_black_linear_rgba();
        shading_result.set_aovs_to_transparent_black_linear_rgba();
    }
}

void ShadingEngine::shade_environment(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const ShadingPoint&     shading_point,
    ShadingResult&          shading_result) const
{
    // Retrieve the environment shader of the scene.
    const EnvironmentShader* environment_shader =
        shading_point.get_scene().get_environment()->get_environment_shader();

    if (environment_shader)
    {
        // There is an environment shader: execute it.
        InputEvaluator input_evaluator(shading_context.get_texture_cache());
        const ShadingRay& ray = shading_point.get_ray();
        const Vector3d direction = normalize(ray.m_dir);
        environment_shader->evaluate(
            shading_context,
            input_evaluator,
            direction,
            shading_result);

        // Set environment shader AOV.
        shading_result.set_entity_aov(*environment_shader);
    }
    else
    {
        // No environment shader: shade as transparent black.
        shading_result.set_main_to_transparent_black_linear_rgba();
        shading_result.set_aovs_to_transparent_black_linear_rgba();
    }
}

}   // namespace renderer
