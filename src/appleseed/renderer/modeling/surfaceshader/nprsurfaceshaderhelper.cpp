
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
#include "foundation/math/scalar.h"
#include "foundation/memory/arena.h"

// Standard headers.
#include <cmath>
#include <limits>

using namespace foundation;

namespace renderer
{

void NPRSurfaceShaderHelper::evaluate(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    ShadingComponents&          shading_components,
    AOVComponents&              aov_components)
{
    const Material* material = shading_point.get_material();
    const ShaderGroup* sg = material->get_render_data().m_shader_group;

#ifdef APPLESEED_WITH_SPECTRAL_SUPPORT
    // For now, we only work in RGB mode.
    if (shading_components.m_beauty.get_mode() == Spectrum::Spectral)
        return;
#endif

    // Make the shading results available to OSL.
    shading_point.m_surface_shader_diffuse = Color3f(
        shading_components.m_diffuse[0],
        shading_components.m_diffuse[1],
        shading_components.m_diffuse[2]);

    shading_point.m_surface_shader_diffuse += Color3f(
        shading_components.m_indirect_diffuse[0],
        shading_components.m_indirect_diffuse[1],
        shading_components.m_indirect_diffuse[2]);

    shading_point.m_surface_shader_glossy = Color3f(
        shading_components.m_glossy[0],
        shading_components.m_glossy[1],
        shading_components.m_glossy[2]);

    shading_point.m_surface_shader_glossy += Color3f(
        shading_components.m_indirect_glossy[0],
        shading_components.m_indirect_glossy[1],
        shading_components.m_indirect_glossy[2]);

    shading_point.m_surface_shader_emission = Color3f(
        shading_components.m_emission[0],
        shading_components.m_emission[1],
        shading_components.m_emission[2]);

    // Execute the OSL shader.
    shading_context.execute_osl_npr(*sg, shading_point);

    Arena arena;
    const CompositeNPRClosure c(shading_point.get_osl_shader_globals().Ci, arena);

    Color3f beauty(0.0f);
    size_t num_contour_closures = 0;

    for (size_t i = 0, e = c.get_closure_count(); i < e; ++i)
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

    aov_components.m_npr_shading = beauty;

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
        aov_components.m_npr_contour = contour;

        // Composite the contour over beauty.
        if (contour.a != 0.0f)
        {
            contour.premultiply_in_place();
            beauty *= 1.0f - contour.a;
            beauty += contour.rgb();
        }
    }

    // Replace the beauty sample.
    shading_components.m_beauty[0] = beauty.r;
    shading_components.m_beauty[1] = beauty.g;
    shading_components.m_beauty[2] = beauty.b;
}

Color4f NPRSurfaceShaderHelper::evaluate_npr_contour(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    const CompositeNPRClosure&  c,
    const size_t                closure_index)
{
    const Intersector& intersector = shading_context.get_intersector();

    const Vector3d& p = shading_point.get_point();
    const Vector3d& n = shading_point.get_shading_normal();
    const ShadingRay& original_ray = shading_point.get_ray();

    const Vector3d& I = original_ray.m_dir;
    const Vector3d dIdx = normalize(original_ray.m_rx_dir - I);
    const Basis3d basis(-I, dIdx);

    // Construct the contour ray.
    ShadingRay ray;
    ray.m_org = original_ray.m_org;
    ray.m_tmin = original_ray.m_tmin;
    ray.m_tmax = std::numeric_limits<double>::max();
    ray.m_time = original_ray.m_time;
    ray.m_flags = VisibilityFlags::ProbeRay;
    ray.m_depth = original_ray.m_depth;

    ShadingPoint other_shading_point;

    // Sample radius. Use the average of the norm of dpdx and dpdy
    // and compensate for the fact that our differentials are 1/4 of a pixel.
    const Vector3d& dpdx = shading_point.get_dpdx();
    const Vector3d& dpdy = shading_point.get_dpdy();
    const double pixel_radius = (norm(dpdx) + norm(dpdy)) * 2.0;

    const NPRContourInputValues* values =
        reinterpret_cast<const NPRContourInputValues*>(c.get_closure_input_values(closure_index));

    const double contour_radius = static_cast<double>(values->m_width) * pixel_radius;

    int discontinuity_samples = 0;
    int total_samples = 0;
    bool diff_contour_found = false;

    // Trace the stencil rays.

    for (size_t q = 1, quality = values->m_quality; q <= quality; ++q)
    {
        const double radius = (q * contour_radius) / static_cast<double>(quality);

        const double angle_step = 45.0 / static_cast<double>(q);
        const double rad_angle_step = deg_to_rad(angle_step);

        const size_t num_samples = static_cast<size_t>(360.0 / angle_step);

        for (size_t i = 0; i < num_samples; ++i)
        {
            const double angle = static_cast<double>(i) * rad_angle_step;
            const double x = std::sin(angle);
            const double y = std::cos(angle);

            const Vector3d pp =
                (radius * x * basis.get_tangent_u()) +
                (radius * y * basis.get_tangent_v()) + p;
            ray.m_dir = normalize(pp - ray.m_org);

            other_shading_point.clear();
            intersector.trace(ray, other_shading_point);

            if (!is_same_object(values->m_features, shading_point, other_shading_point))
                ++discontinuity_samples;

            // Handle forward difference rays.
            if (discontinuity_samples == 0 && !diff_contour_found && q == quality)
            {
                if (other_shading_point.hit_surface() &&
                    values->m_features & static_cast<unsigned int>(NPRContourFeatures::AllDifferenceFeatures))
                {
                    const double abs_x = std::abs(x);
                    const double Eps = 1e-10;

                    if (feq(abs_x, 0.0, Eps) || feq(abs_x, 1.0, Eps))
                    {
                        if (values->m_features & static_cast<unsigned int>(NPRContourFeatures::OcclusionEdges))
                        {
                            const float d = static_cast<float>(std::abs(shading_point.get_distance() - other_shading_point.get_distance()));

                            if (d > values->m_occlusion_threshold)
                                diff_contour_found = true;
                        }

                        if (values->m_features & static_cast<unsigned int>(NPRContourFeatures::CreaseEdges))
                        {
                            const Vector3d& nc = other_shading_point.get_shading_normal();
                            const float cos_nnc = static_cast<float>(dot(n, nc));

                            if (cos_nnc < values->m_cos_crease_threshold)
                                diff_contour_found = true;
                        }
                    }
                }
            }

            ++total_samples;
        }
    }

    if (discontinuity_samples == 0)
        return Color4f(values->m_color, diff_contour_found ? values->m_opacity : 0.0f);
    else
    {
        // Compute the edge strength.
        const float half_samples = total_samples * 0.5f;
        const float alpha = 1.0f - (std::fabs(discontinuity_samples - half_samples) / half_samples);

        return Color4f(values->m_color, alpha * values->m_opacity);
    }
}

bool NPRSurfaceShaderHelper::is_same_object(
    const unsigned int          features,
    const ShadingPoint&         shading_point,
    const ShadingPoint&         other_shading_point)
{
    if (other_shading_point.hit_surface())
    {
        if (features & static_cast<unsigned int>(NPRContourFeatures::ObjectInstanceID))
        {
            if (&shading_point.get_object_instance() != &other_shading_point.get_object_instance())
                return false;
            else
            {
                // Do not consider instances the same object.
                if (&shading_point.get_assembly_instance() != &other_shading_point.get_assembly_instance())
                    return false;
            }
        }

        if (features & static_cast<unsigned int>(NPRContourFeatures::MaterialID))
        {
            if (shading_point.get_material() != other_shading_point.get_material())
                return false;
        }
    }
    else
    {
        if (features & static_cast<unsigned int>(NPRContourFeatures::AllIDFeatures))
            return false;
    }

    return true;
}

}   // namespace renderer
