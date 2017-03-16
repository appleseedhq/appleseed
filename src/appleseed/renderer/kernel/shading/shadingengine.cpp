
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingray.h"
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
    SamplingContext&            sampling_context,
    const PixelContext&         pixel_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    AOVAccumulatorContainer&    aov_accumulators) const
{
    // Compute the alpha channel of the main output.
    aov_accumulators.alpha().set(shading_point.get_alpha());

    // Retrieve the material of the intersected surface.
    const Material* material = shading_point.get_material();

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
        aov_accumulators.alpha().apply_multiplier(alpha);
    }

    // Shade the sample if it isn't fully transparent.
    if (aov_accumulators.alpha().get()[0] > 0.0f || shading_point.shade_alpha_cutouts())
    {
        // Use the diagnostic surface shader if there is one.
        const SurfaceShader* surface_shader = m_diagnostic_surface_shader.get();

        if (surface_shader == 0)
        {
            if (material == 0)
            {
                // The intersected surface has no material: return solid pink.
                aov_accumulators.beauty().set_to_pink_linear_rgb();
                aov_accumulators.alpha().set(Alpha(1.0f));
                return;
            }

            // Use the surface shader of the intersected surface.
            surface_shader = material->get_render_data().m_surface_shader;

            if (surface_shader == 0)
            {
                // The intersected surface has no surface shader: return solid pink.
                aov_accumulators.beauty().set_to_pink_linear_rgb();
                aov_accumulators.alpha().set(Alpha(1.0f));
                return;
            }
        }

        // Execute the surface shader.
        surface_shader->evaluate(
            sampling_context,
            pixel_context,
            shading_context,
            shading_point,
            aov_accumulators);
    }
}

void ShadingEngine::shade_environment(
    SamplingContext&            sampling_context,
    const PixelContext&         pixel_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    AOVAccumulatorContainer&    aov_accumulators) const
{
    // Retrieve the environment shader of the scene.
    const EnvironmentShader* environment_shader =
        shading_point.get_scene().get_environment()->get_environment_shader();

    if (environment_shader)
    {
        // There is an environment shader: execute it.
        const ShadingRay& ray = shading_point.get_ray();
        const Vector3d direction = normalize(ray.m_dir);
        Spectrum value;
        Alpha alpha;
        environment_shader->evaluate(
            shading_context,
            pixel_context,
            direction,
            value,
            alpha);
        aov_accumulators.beauty().set(value);
        aov_accumulators.alpha().set(alpha);
    }
}

}   // namespace renderer
