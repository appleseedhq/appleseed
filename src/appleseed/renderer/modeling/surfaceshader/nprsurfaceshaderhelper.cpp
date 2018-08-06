
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "nprsurfaceshaderhelper.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovcomponents.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/npr/nprclosures.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/shadergroup/shadergroup.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/utility/arena.h"

// Standard headers.
#include <cmath>
#include <limits>

using namespace foundation;
using namespace std;

namespace renderer
{

void NPRSurfaceShaderHelper::evaluate(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    AOVComponents&              components,
    ShadingComponents&          radiance) const
{
    const Material* material = shading_point.get_material();
    const ShaderGroup* sg = material->get_render_data().m_shader_group;

    evaluate_npr(sampling_context, shading_context, shading_point, sg, radiance, components);
}

void NPRSurfaceShaderHelper::evaluate_npr(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    const ShaderGroup*          sg,
    ShadingComponents&          radiance,
    AOVComponents&              components) const
{
    // For now, we only work in RGB mode.
    if (radiance.m_beauty.get_mode() == Spectrum::Spectral)
        return;

    // Make the shading results available to OSL.
    shading_point.m_surface_shader_diffuse = Color3f(
        radiance.m_diffuse[0],
        radiance.m_diffuse[1],
        radiance.m_diffuse[2]);

    shading_point.m_surface_shader_diffuse += Color3f(
        radiance.m_indirect_diffuse[0],
        radiance.m_indirect_diffuse[1],
        radiance.m_indirect_diffuse[2]);

    shading_point.m_surface_shader_glossy = Color3f(
        radiance.m_glossy[0],
        radiance.m_glossy[1],
        radiance.m_glossy[2]);

    shading_point.m_surface_shader_glossy += Color3f(
        radiance.m_indirect_glossy[0],
        radiance.m_indirect_glossy[1],
        radiance.m_indirect_glossy[2]);

    shading_point.m_surface_shader_emission = Color3f(
        radiance.m_emission[0],
        radiance.m_emission[1],
        radiance.m_emission[2]);

    // Execute the OSL shader.
    shading_context.execute_osl_npr(*sg, shading_point);

    Arena arena;
    const CompositeNPRClosure c(shading_point.get_osl_shader_globals().Ci, arena);

    Color3f beauty(0.0f);
    size_t num_contour_closures = 0;

    for (size_t i = 0 , e = c.get_closure_count(); i < e ; ++i)
    {
        if (c.get_closure_type(i) == NPRShadingID)
        {
            const Spectrum& col = c.get_closure_weight(i);

            beauty.r += col[0];
            beauty.g += col[1];
            beauty.b += col[2];
        }
        else if (c.get_closure_type(i) == NPRContourID)
            ++num_contour_closures;
    }

    components.m_npr_shading = beauty;

    if (num_contour_closures != 0)
    {
        size_t closure_index = 0;

        // Pick a contour closure randomly if we have more than one.
        if (num_contour_closures != 1)
        {
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();
            closure_index = truncate<size_t>(num_contour_closures * s);
        }

        closure_index = c.get_nth_contour_closure_index(closure_index);
        Color4f contour = evaluate_npr_contour(
            sampling_context,
            shading_context,
            shading_point,
            c,
            closure_index);

        // Save contour AOV (unpremultiplied).
        components.m_npr_contour = contour;

        // Composite the contour over beauty.
        if (contour.a != 0.0f)
        {
            contour.premultiply();
            beauty *= (1.0f - contour.a);
            beauty += contour.rgb();
        }
    }

    // Replace the beauty sample.
    radiance.m_beauty[0] = beauty.r;
    radiance.m_beauty[1] = beauty.g;
    radiance.m_beauty[2] = beauty.b;
}

Color4f NPRSurfaceShaderHelper::evaluate_npr_contour(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    const CompositeNPRClosure&  c,
    const size_t                closure_index) const
{
    const Intersector& intersector = shading_context.get_intersector();

    const Vector3d& p = shading_point.get_point();
    const Vector3d& n = shading_point.get_shading_normal();
    const ShadingRay& original_ray = shading_point.get_ray();

    // Construct the contour ray.
    ShadingRay ray;
    ray.m_org = original_ray.m_org;
    ray.m_tmin = original_ray.m_tmin;
    ray.m_tmax = numeric_limits<double>::max();
    ray.m_time = original_ray.m_time;
    ray.m_flags = VisibilityFlags::ProbeRay;
    ray.m_depth = original_ray.m_depth;

    ShadingPoint contour_shading_point;

    // Sample radius. Use the average of the norm of dpdx and dpdy
    // and compensate for the fact that our differentials are 1/4 of a pixel.
    const Vector3d& dpdx = shading_point.get_dpdx();
    const Vector3d& dpdy = shading_point.get_dpdy();
    const float diff_radius = static_cast<float>(norm(dpdx) + norm(dpdy)) * 2.0f;

    const NPRContourInputValues* values =
        reinterpret_cast<const NPRContourInputValues*>(c.get_closure_input_values(closure_index));

    const float edge_width = values->m_width * diff_radius;
    Color4f edge_col(values->m_color, values->m_opacity);

    contour_shading_point.clear();

    sampling_context.split_in_place(2, 1);
    const Vector3f s = sample_sphere_uniform(sampling_context.next2<Vector2f>()) * edge_width;

    const Vector3d q = p + Vector3d(s);
    ray.m_dir = normalize(q - ray.m_org);

    if (intersector.trace(ray, contour_shading_point))
    {
        if (values->m_features & NPRContourFeatures::ObjectInstanceID)
        {
            if (&shading_point.get_object_instance() != &contour_shading_point.get_object_instance())
                return edge_col;
        }

        if (values->m_features & NPRContourFeatures::MaterialID)
        {
            if (shading_point.get_material() != contour_shading_point.get_material())
                return edge_col;
        }

        if (values->m_features & NPRContourFeatures::OcclusionEdges)
        {
            const float d = static_cast<float>(abs(shading_point.get_distance() - contour_shading_point.get_distance()));

            if (d > values->m_occlusion_threshold)
                return edge_col;
        }

        if (values->m_features & NPRContourFeatures::CreaseEdges)
        {
            const Vector3d& nc = contour_shading_point.get_shading_normal();
            const float cos_nnc = static_cast<float>(dot(n, nc));

            if (cos_nnc < values->m_cos_crease_threshold)
                return edge_col;
        }
    }
    else
    {
        if (values->m_features & NPRContourFeatures::AllIDFeatures)
            return edge_col;
    }

    return Color4f(values->m_color.r, values->m_color.g, values->m_color.b, 0.0f);
}

}   // namespace renderer
