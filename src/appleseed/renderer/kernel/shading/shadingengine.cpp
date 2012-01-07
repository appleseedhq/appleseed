
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/surfaceshader/constantsurfaceshader.h"
#include "renderer/modeling/surfaceshader/diagnosticsurfaceshader.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"

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
    const ShadingContext&   shading_context,
    const ShadingPoint&     shading_point,
    ShadingResult&          shading_result) const
{
    // Use the diagnostic surface shader if there is one.
    const SurfaceShader* surface_shader = m_diagnostic_surface_shader.get();

    if (surface_shader == 0)
    {
        // There is no diagnostic shader. Retrieve the material of the intersected surface.
        const Material* material = shading_point.get_material();

        if (material == 0)
        {
            // The intersected surface has no material: return solid pink.
            shading_result.set_to_solid_pink();
            return;
        }

        // Use the surface shader of the intersected surface.
        surface_shader = material->get_surface_shader();

        if (surface_shader == 0)
        {
            // The intersected surface has no surface shader: return solid pink.
            shading_result.set_to_solid_pink();
            return;
        }
    }

    // Execute the surface shader.
    surface_shader->evaluate(
        sampling_context,
        shading_context,
        shading_point,
        shading_result);
}

void ShadingEngine::shade_environment(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const ShadingPoint&     shading_point,
    ShadingResult&          shading_result) const
{
    // Retrieve the environment shader of the scene.
    const Scene& scene = shading_point.get_scene();
    const EnvironmentShader* environment_shader =
        scene.get_environment()->get_environment_shader();

    if (environment_shader)
    {
        // There is an environment shader: execute it.
        InputEvaluator input_evaluator(shading_context.get_texture_cache());
        const ShadingRay& ray = shading_point.get_ray();
        const Vector3d direction = normalize(ray.m_dir);
        environment_shader->evaluate(
            input_evaluator,
            direction,
            shading_result);
    }
}

}   // namespace renderer
