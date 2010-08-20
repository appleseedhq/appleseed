
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/input/inputevaluator.h"
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

// Constructor.
ShadingEngine::ShadingEngine(const ParamArray& params)
{
    // Create the material used to shade surfaces without materials.
    m_missing_material_sshader =
        ConstantSurfaceShaderFactory::create(
            "__missing_material_surface_shader",
            ParamArray());
    m_missing_material =
        MaterialFactory::create(
            "__missing_material",
            m_missing_material_sshader.get());

    // Create the diagnostic material, if requested.
    if (params.dictionaries().exist("override_shading"))
    {
        m_diagnostic_material_sshader =
            DiagnosticSurfaceShaderFactory::create(
                "__diagnostic_surface_shader",
                params.child("override_shading"));
        m_diagnostic_material =
            MaterialFactory::create(
                "__diagnostic_material",
                m_diagnostic_material_sshader.get());
    }
}

// Shade a given intersection point.
void ShadingEngine::shade(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const ShadingPoint&     shading_point,
    ShadingResult&          shading_result) const
{
    if (shading_point.hit())
    {
        // Retrieve the material at the intersection point.
        const Material* material = m_diagnostic_material.get();
        if (material == 0)
        {
            // No diagnostic material: use the material assigned to the triangle.
            material = shading_point.get_material();

            // The triangle has no material: use a substitute.
            if (material == 0)
                material = m_missing_material.get();
        }

        // Retrieve the surface shader of the material.
        const SurfaceShader& surface_shader = material->get_surface_shader();

        // Execute the surface shader.
        surface_shader.evaluate(
            sampling_context,
            shading_context,
            shading_point,
            shading_result);
    }
    else
    {
        // Retrieve the environment shader of the scene.
        const Scene& scene = shading_point.get_scene();
        const Environment* environment = scene.get_environment();
        const EnvironmentShader* environment_shader =
            environment ? environment->get_environment_shader() : 0;

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
        else
        {
            // No environment shader: return transparent black.
            shading_result.clear();
        }
    }
}

}   // namespace renderer
